use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use aura_codegen::CodeGen;
use aura_lexer::Lexer;
use aura_parser::Parser;
use aura_resolve::ResolvedModule;
use aura_resolve::Resolver;
use aura_types::{TypeChecker, TypedModule};
use inkwell::context::Context;

fn workspace_root() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop(); // crates/
    path.pop(); // workspace root
    path
}

fn find_runtime_staticlib() -> Option<PathBuf> {
    let target_dir = workspace_root().join("target").join("debug");
    let direct = target_dir.join("libaura_rt.a");
    if direct.exists() {
        return Some(direct);
    }

    let deps_dir = target_dir.join("deps");
    let entries = fs::read_dir(deps_dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("a")
            && path
                .file_name()
                .and_then(|n| n.to_str())
                .is_some_and(|n| n.starts_with("libaura_rt-"))
        {
            return Some(path);
        }
    }
    None
}

fn ensure_runtime_staticlib() -> Result<PathBuf, String> {
    if let Some(path) = find_runtime_staticlib() {
        return Ok(path);
    }

    let status = Command::new("cargo")
        .current_dir(workspace_root())
        .args(["build", "-p", "aura_rt"])
        .status()
        .map_err(|e| format!("failed to build aura_rt runtime: {e}"))?;

    if !status.success() {
        return Err("failed to build aura_rt runtime".into());
    }

    find_runtime_staticlib().ok_or_else(|| "could not locate built libaura_rt.a".into())
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("Usage: aura <build|run|check> [path] [options]");
        std::process::exit(1);
    }

    let result = match args[0].as_str() {
        "build" => cmd_build(&args[1..]),
        "run" => cmd_run(&args[1..]),
        "check" => cmd_check(&args[1..]),
        other => Err(format!(
            "unknown command '{}'. expected one of: build, run, check",
            other
        )),
    };

    if let Err(err) = result {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn cmd_build(args: &[String]) -> Result<(), String> {
    let mut source: Option<PathBuf> = None;
    let mut output: Option<PathBuf> = None;
    let mut release = false;
    let mut emit_ir = false;
    let mut no_link = false;

    let mut i = 0usize;
    while i < args.len() {
        match args[i].as_str() {
            "--release" => release = true,
            "--emit-ir" => emit_ir = true,
            "--no-link" => no_link = true,
            "-o" => {
                i += 1;
                if i >= args.len() {
                    return Err("missing path after -o".into());
                }
                output = Some(PathBuf::from(&args[i]));
            }
            flag if flag.starts_with('-') => {
                return Err(format!("unknown build flag '{flag}'"));
            }
            path => {
                if source.is_some() {
                    return Err(format!(
                        "multiple source paths provided (extra: '{}')",
                        path
                    ));
                }
                source = Some(PathBuf::from(path));
            }
        }
        i += 1;
    }

    let source = source.unwrap_or_else(|| PathBuf::from("src/main.aura"));
    let output = output.or_else(|| Some(default_output_path(&source, release)));

    let built = compile_file(&source, output, emit_ir, no_link)?;
    if !emit_ir {
        eprintln!("built: {}", built.display());
    }
    Ok(())
}

fn cmd_run(args: &[String]) -> Result<(), String> {
    let mut source: Option<PathBuf> = None;
    let mut release = false;

    for arg in args {
        match arg.as_str() {
            "--release" => release = true,
            flag if flag.starts_with('-') => {
                return Err(format!("unknown run flag '{flag}'"));
            }
            path => {
                if source.is_some() {
                    return Err(format!(
                        "multiple source paths provided (extra: '{}')",
                        path
                    ));
                }
                source = Some(PathBuf::from(path));
            }
        }
    }

    let source = source.unwrap_or_else(|| PathBuf::from("src/main.aura"));
    let output = default_output_path(&source, release);

    let exe_path = compile_file(&source, Some(output), false, false)?;
    let status = Command::new(&exe_path)
        .status()
        .map_err(|e| format!("failed to execute '{}': {e}", exe_path.display()))?;

    if !status.success() {
        let code = status.code().unwrap_or(1);
        return Err(format!("program exited with status code {code}"));
    }

    Ok(())
}

fn cmd_check(args: &[String]) -> Result<(), String> {
    let mut source: Option<PathBuf> = None;
    for arg in args {
        if arg.starts_with('-') {
            return Err(format!("unknown check flag '{arg}'"));
        }
        if source.is_some() {
            return Err(format!("multiple source paths provided (extra: '{}')", arg));
        }
        source = Some(PathBuf::from(arg));
    }

    let source = source.unwrap_or_else(|| PathBuf::from("src/main.aura"));
    let _ = check_file(&source)?;
    eprintln!("check ok: {}", source.display());
    Ok(())
}

fn check_file(source_path: &Path) -> Result<(ResolvedModule, TypedModule), String> {
    let source = fs::read_to_string(source_path)
        .map_err(|e| format!("error: could not read '{}': {}", source_path.display(), e))?;

    let mut lexer = Lexer::new(&source, 0);
    let tokens = lexer.tokenize();

    for tok in &tokens {
        if let aura_lexer::TokenKind::Error(msg) = &tok.kind {
            return Err(format!(
                "lexer error at {}..{}: {}",
                tok.span.start, tok.span.end, msg
            ));
        }
    }

    let mut parser = Parser::new(tokens);
    let module = parser.parse_module().map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let resolver = Resolver::new();
    let resolved = resolver.resolve(module).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    let checker = TypeChecker::new();
    let typed = checker.check(&resolved).map_err(|errors| {
        errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    Ok((resolved, typed))
}

fn compile_file(
    source_path: &Path,
    output_path: Option<PathBuf>,
    emit_ir: bool,
    no_link: bool,
) -> Result<PathBuf, String> {
    let (resolved, typed) = check_file(source_path)?;

    let context = Context::create();
    let module_name = source_path
        .file_stem()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| "main".into());

    let mut codegen = CodeGen::new(&context, &module_name);
    codegen.set_expr_types(typed.expr_types.clone());
    codegen.compile_module(&resolved.module)?;

    if emit_ir {
        println!("{}", codegen.print_ir());
    }

    let exe_path = output_path.unwrap_or_else(|| source_path.with_extension(""));
    if let Some(parent) = exe_path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            format!(
                "failed to create output directory '{}': {}",
                parent.display(),
                e
            )
        })?;
    }

    let obj_path = exe_path.with_extension("o");
    codegen.write_object_file(&obj_path)?;

    if no_link {
        return Ok(obj_path);
    }

    let runtime_lib = ensure_runtime_staticlib()?;

    let mut link = Command::new("cc");
    link.arg(obj_path.to_str().unwrap_or(""))
        .arg(runtime_lib.to_str().unwrap_or(""))
        .arg("-o")
        .arg(exe_path.to_str().unwrap_or(""))
        .arg("-lm");

    #[cfg(target_os = "linux")]
    {
        link.args(["-ldl", "-lpthread", "-lrt", "-lutil"]);
    }

    let status = link
        .status()
        .map_err(|e| format!("failed to invoke linker: {e}"))?;

    if !status.success() {
        return Err("linking failed".into());
    }

    let _ = fs::remove_file(&obj_path);
    Ok(exe_path)
}

fn default_output_path(source: &Path, release: bool) -> PathBuf {
    let profile = if release { "release" } else { "debug" };
    let stem = source
        .file_stem()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| "main".into());
    PathBuf::from("target").join(profile).join(stem)
}
