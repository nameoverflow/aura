use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

use aura_lexer::Lexer;
use aura_parser::Parser;
use aura_resolve::Resolver;
use aura_types::TypeChecker;
use aura_codegen::CodeGen;
use inkwell::context::Context;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: aurac <source.aura> [--emit-ir] [--no-link] [-o <output>]");
        std::process::exit(1);
    }

    let source_path = &args[1];
    let emit_ir = args.contains(&"--emit-ir".to_string());
    let no_link = args.contains(&"--no-link".to_string());
    let output_path = args.iter().position(|a| a == "-o")
        .and_then(|i| args.get(i + 1))
        .cloned();

    // Read source file
    let source = match fs::read_to_string(source_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: could not read '{}': {}", source_path, e);
            std::process::exit(1);
        }
    };

    // Compile
    match compile(&source, source_path, emit_ir, no_link, output_path) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}

fn compile(
    source: &str,
    source_path: &str,
    emit_ir: bool,
    no_link: bool,
    output_path: Option<String>,
) -> Result<(), String> {
    // Step 1: Lex
    let mut lexer = Lexer::new(source, 0);
    let tokens = lexer.tokenize();

    // Check for lexer errors
    for tok in &tokens {
        if let aura_lexer::TokenKind::Error(msg) = &tok.kind {
            return Err(format!("lexer error at {}..{}: {}", tok.span.start, tok.span.end, msg));
        }
    }

    // Step 2: Parse
    let mut parser = Parser::new(tokens);
    let module = parser.parse_module().map_err(|errors| {
        errors.iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    // Step 3: Name Resolution
    let resolver = Resolver::new();
    let resolved = resolver.resolve(module).map_err(|errors| {
        errors.iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    // Step 4: Type Check
    let checker = TypeChecker::new();
    let _typed = checker.check(&resolved).map_err(|errors| {
        errors.iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    })?;

    // Step 5: Code Generation
    let context = Context::create();
    let module_name = PathBuf::from(source_path)
        .file_stem()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| "main".into());

    let mut codegen = CodeGen::new(&context, &module_name);
    codegen.compile_module(&resolved.module)?;

    if emit_ir {
        println!("{}", codegen.print_ir());
        return Ok(());
    }

    // Write object file
    let obj_path = PathBuf::from(source_path).with_extension("o");
    codegen.write_object_file(&obj_path)?;

    if no_link {
        eprintln!("wrote object file: {}", obj_path.display());
        return Ok(());
    }

    // Link to executable
    let exe_path = output_path.map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(source_path).with_extension(""));

    let status = Command::new("cc")
        .args([
            obj_path.to_str().unwrap(),
            "-o",
            exe_path.to_str().unwrap(),
            "-lm",
        ])
        .status()
        .map_err(|e| format!("failed to invoke linker: {e}"))?;

    if !status.success() {
        return Err("linking failed".into());
    }

    // Clean up object file
    let _ = fs::remove_file(&obj_path);

    eprintln!("compiled: {} -> {}", source_path, exe_path.display());
    Ok(())
}
