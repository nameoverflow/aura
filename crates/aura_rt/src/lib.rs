use std::alloc::{alloc_zeroed, dealloc, handle_alloc_error, Layout};
use std::io::{self, Write};
use std::mem;
use std::ptr;
use std::slice;
use std::sync::{Mutex, OnceLock};

#[repr(C)]
pub struct TypeDesc {
    pub size: usize,
    pub name: *const u8,
    pub num_gc_fields: usize,
    pub gc_field_offsets: *const usize,
}

#[repr(C)]
pub struct ShadowFrame {
    pub prev: *mut ShadowFrame,
    pub num_roots: usize,
    pub roots: *mut *mut u8,
}

#[repr(C)]
struct GcHeader {
    mark: u8,
    _pad: [u8; 7],
    type_desc: *const TypeDesc,
    next: *mut GcHeader,
    size: usize,
}

struct GarbageCollector {
    all_objects: *mut GcHeader,
    shadow_stack: *mut ShadowFrame,
    bytes_allocated: usize,
    threshold: usize,
    growth_factor: f64,
}

impl GarbageCollector {
    fn new() -> Self {
        Self {
            all_objects: ptr::null_mut(),
            shadow_stack: ptr::null_mut(),
            bytes_allocated: 0,
            threshold: 1024 * 1024,
            growth_factor: 2.0,
        }
    }

    unsafe fn alloc(&mut self, size: usize, type_desc: *const TypeDesc) -> *mut u8 {
        if self.bytes_allocated.saturating_add(size) > self.threshold {
            self.collect();
        }

        let layout = object_layout(size);
        let raw = alloc_zeroed(layout);
        if raw.is_null() {
            handle_alloc_error(layout);
        }

        let header = raw as *mut GcHeader;
        (*header).mark = 0;
        (*header)._pad = [0; 7];
        (*header).type_desc = type_desc;
        (*header).next = self.all_objects;
        (*header).size = size;

        self.all_objects = header;
        self.bytes_allocated = self.bytes_allocated.saturating_add(size);

        raw.add(mem::size_of::<GcHeader>())
    }

    unsafe fn collect(&mut self) {
        self.mark();
        self.sweep();
    }

    unsafe fn mark(&mut self) {
        let mut frame = self.shadow_stack;
        while !frame.is_null() {
            let roots = (*frame).roots;
            if !roots.is_null() {
                for i in 0..(*frame).num_roots {
                    let root_ptr = *roots.add(i);
                    self.mark_payload(root_ptr);
                }
            }
            frame = (*frame).prev;
        }
    }

    unsafe fn mark_payload(&mut self, payload: *mut u8) {
        if payload.is_null() {
            return;
        }

        let header = payload.sub(mem::size_of::<GcHeader>()) as *mut GcHeader;
        if header.is_null() || (*header).mark != 0 {
            return;
        }
        (*header).mark = 1;

        let type_desc = (*header).type_desc;
        if type_desc.is_null() {
            return;
        }

        let num_fields = (*type_desc).num_gc_fields;
        let offsets = (*type_desc).gc_field_offsets;
        if offsets.is_null() {
            return;
        }

        for i in 0..num_fields {
            let offset = *offsets.add(i);
            if offset + mem::size_of::<*mut u8>() > (*header).size {
                continue;
            }
            let child_slot = payload.add(offset) as *mut *mut u8;
            let child = *child_slot;
            self.mark_payload(child);
        }
    }

    unsafe fn sweep(&mut self) {
        let mut prev: *mut GcHeader = ptr::null_mut();
        let mut current = self.all_objects;
        let mut live_bytes = 0usize;

        while !current.is_null() {
            let next = (*current).next;
            if (*current).mark == 0 {
                if prev.is_null() {
                    self.all_objects = next;
                } else {
                    (*prev).next = next;
                }

                let size = (*current).size;
                let layout = object_layout(size);
                dealloc(current as *mut u8, layout);
                self.bytes_allocated = self.bytes_allocated.saturating_sub(size);
            } else {
                (*current).mark = 0;
                live_bytes = live_bytes.saturating_add((*current).size);
                prev = current;
            }
            current = next;
        }

        self.threshold = ((live_bytes as f64) * self.growth_factor).max(1024.0) as usize;
        if self.threshold < self.bytes_allocated {
            self.threshold = self.bytes_allocated.saturating_mul(2).max(1024);
        }
    }

    unsafe fn reset(&mut self) {
        let mut current = self.all_objects;
        while !current.is_null() {
            let next = (*current).next;
            let layout = object_layout((*current).size);
            dealloc(current as *mut u8, layout);
            current = next;
        }
        self.all_objects = ptr::null_mut();
        self.shadow_stack = ptr::null_mut();
        self.bytes_allocated = 0;
        self.threshold = 1024 * 1024;
    }

    fn object_count(&self) -> usize {
        let mut count = 0usize;
        let mut current = self.all_objects;
        while !current.is_null() {
            count += 1;
            // SAFETY: The linked list is owned by the GC lock.
            unsafe {
                current = (*current).next;
            }
        }
        count
    }
}

fn object_layout(payload_size: usize) -> Layout {
    Layout::from_size_align(
        mem::size_of::<GcHeader>() + payload_size,
        mem::align_of::<GcHeader>(),
    )
    .expect("invalid GC allocation layout")
}

// SAFETY: Runtime GC is protected by a global mutex; raw pointers are only touched under the lock.
unsafe impl Send for GarbageCollector {}

static GC: OnceLock<Mutex<GarbageCollector>> = OnceLock::new();

fn with_gc<R>(f: impl FnOnce(&mut GarbageCollector) -> R) -> R {
    let gc = GC.get_or_init(|| Mutex::new(GarbageCollector::new()));
    let mut guard = gc.lock().expect("aura runtime GC lock poisoned");
    f(&mut guard)
}

#[no_mangle]
pub extern "C" fn aura_rt_init() {
    let _ = GC.get_or_init(|| Mutex::new(GarbageCollector::new()));
}

#[no_mangle]
pub extern "C" fn aura_rt_shutdown() {
    with_gc(|gc| {
        // SAFETY: GC internals are only accessed while holding the lock.
        unsafe {
            gc.collect();
            gc.reset();
        }
    });
}

#[no_mangle]
pub unsafe extern "C" fn aura_gc_alloc(size: usize, type_desc: *const TypeDesc) -> *mut u8 {
    with_gc(|gc| gc.alloc(size, type_desc))
}

#[no_mangle]
pub extern "C" fn aura_gc_collect() {
    with_gc(|gc| {
        // SAFETY: GC internals are only accessed while holding the lock.
        unsafe { gc.collect() }
    });
}

#[no_mangle]
pub unsafe extern "C" fn aura_gc_push_frame(frame: *mut ShadowFrame) {
    if frame.is_null() {
        return;
    }
    with_gc(|gc| {
        (*frame).prev = gc.shadow_stack;
        gc.shadow_stack = frame;
    });
}

#[no_mangle]
pub unsafe extern "C" fn aura_gc_pop_frame(frame: *mut ShadowFrame) {
    if frame.is_null() {
        return;
    }

    with_gc(|gc| {
        if gc.shadow_stack == frame {
            gc.shadow_stack = (*frame).prev;
            return;
        }

        let mut current = gc.shadow_stack;
        while !current.is_null() {
            let next = (*current).prev;
            if next == frame {
                (*current).prev = (*frame).prev;
                return;
            }
            current = next;
        }
    });
}

#[no_mangle]
pub extern "C" fn aura_gc_bytes_allocated() -> usize {
    with_gc(|gc| gc.bytes_allocated)
}

#[no_mangle]
pub extern "C" fn aura_gc_object_count() -> usize {
    with_gc(|gc| gc.object_count())
}

#[no_mangle]
pub extern "C" fn aura_runtime_gc() {
    aura_gc_collect();
}

#[no_mangle]
pub unsafe extern "C" fn aura_print(bytes: *const u8, len: usize) {
    if bytes.is_null() {
        return;
    }
    let slice = slice::from_raw_parts(bytes, len);
    let _ = io::stdout().write_all(slice);
    let _ = io::stdout().flush();
}

#[no_mangle]
pub unsafe extern "C" fn aura_println(bytes: *const u8, len: usize) {
    aura_print(bytes, len);
    let _ = io::stdout().write_all(b"\n");
    let _ = io::stdout().flush();
}

#[no_mangle]
pub unsafe extern "C" fn aura_panic(
    message: *const u8,
    len: usize,
    file: *const u8,
    line: u32,
) -> ! {
    let msg = if message.is_null() {
        "<null panic message>".to_string()
    } else {
        String::from_utf8_lossy(slice::from_raw_parts(message, len)).to_string()
    };

    let file_str = if file.is_null() {
        "<unknown>".to_string()
    } else {
        // file is expected to be a C string for diagnostics.
        let mut n = 0usize;
        while *file.add(n) != 0 {
            n += 1;
        }
        String::from_utf8_lossy(slice::from_raw_parts(file, n)).to_string()
    };

    eprintln!("panic at {}:{}\n  {}", file_str, line, msg);
    std::process::abort();
}

#[no_mangle]
pub extern "C" fn aura_assert(condition: bool) {
    assert!(condition, "assertion failed");
}

#[no_mangle]
pub extern "C" fn aura_assert_eq_i64(a: i64, b: i64) {
    assert_eq!(a, b, "assert_eq failed");
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    unsafe fn payload_ptr_to_slot(payload: *mut u8) -> *mut *mut u8 {
        payload as *mut *mut u8
    }

    static TEST_LOCK: OnceLock<Mutex<()>> = OnceLock::new();

    fn with_test_runtime<R>(f: impl FnOnce() -> R) -> R {
        let lock = TEST_LOCK.get_or_init(|| Mutex::new(()));
        let _guard = lock.lock().expect("test runtime lock poisoned");
        aura_rt_shutdown();
        aura_rt_init();
        let out = f();
        aura_rt_shutdown();
        out
    }

    fn scalar_desc() -> TypeDesc {
        TypeDesc {
            size: 8,
            name: ptr::null(),
            num_gc_fields: 0,
            gc_field_offsets: ptr::null(),
        }
    }

    fn node_desc(offsets: &[usize]) -> TypeDesc {
        TypeDesc {
            size: mem::size_of::<*mut u8>(),
            name: ptr::null(),
            num_gc_fields: offsets.len(),
            gc_field_offsets: offsets.as_ptr(),
        }
    }

    #[test]
    fn gc_collects_unreachable_objects() {
        with_test_runtime(|| {
            let desc = scalar_desc();
            unsafe {
                let _a = aura_gc_alloc(8, &desc);
                let _b = aura_gc_alloc(8, &desc);
                let _c = aura_gc_alloc(8, &desc);
            }
            assert_eq!(aura_gc_object_count(), 3);
            aura_gc_collect();
            assert_eq!(aura_gc_object_count(), 0);
        });
    }

    #[test]
    fn gc_preserves_rooted_chain() {
        with_test_runtime(|| {
            let offsets = [0usize];
            let desc = node_desc(&offsets);

            unsafe {
                let a = aura_gc_alloc(mem::size_of::<*mut u8>(), &desc);
                let b = aura_gc_alloc(mem::size_of::<*mut u8>(), &desc);
                *payload_ptr_to_slot(a) = b;

                let mut roots = [a];
                let mut frame = ShadowFrame {
                    prev: ptr::null_mut(),
                    num_roots: roots.len(),
                    roots: roots.as_mut_ptr(),
                };
                aura_gc_push_frame(&mut frame);
                aura_gc_collect();
                assert_eq!(aura_gc_object_count(), 2);

                aura_gc_pop_frame(&mut frame);
                aura_gc_collect();
                assert_eq!(aura_gc_object_count(), 0);
            }
        });
    }

    #[test]
    fn gc_collects_cycles() {
        with_test_runtime(|| {
            let offsets = [0usize];
            let desc = node_desc(&offsets);

            unsafe {
                let a = aura_gc_alloc(mem::size_of::<*mut u8>(), &desc);
                let b = aura_gc_alloc(mem::size_of::<*mut u8>(), &desc);
                *payload_ptr_to_slot(a) = b;
                *payload_ptr_to_slot(b) = a;
            }

            assert_eq!(aura_gc_object_count(), 2);
            aura_gc_collect();
            assert_eq!(aura_gc_object_count(), 0);
        });
    }
}
