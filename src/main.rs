
#![allow(dead_code)]

mod cli;

use cli::{CliArgs, CliArg, CliArgList};
use compiler::util::{
    error::{Error, ErrorSection, ErrorType},
    strings::{StringMap, StringIdx}
};

use std::{process::exit, fs, env, collections::HashMap};


fn main() {
    #[cfg(target_os = "windows")] {
        use windows::Win32::System::Console;
        unsafe {
            let output = Console::GetStdHandle(Console::STD_OUTPUT_HANDLE)
                .expect("Failed to get console handle");
            let mut console_mode: Console::CONSOLE_MODE = Default::default();
            Console::GetConsoleMode(output, &mut console_mode)
                .expect("Failed to get console mode");
            console_mode |= Console::ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            Console::SetConsoleMode(output, console_mode)
                .expect("Failed to set console mode");
        }
    }
    if let Err(errors) = do_compilation() {
        println!("{}", errors);
        exit(1);
    }
}

pub fn display_errors(errors: Vec<Error>, strings: &mut StringMap, color: bool) -> String {
    errors.into_iter()
        .map(|e| e.display(strings, color))
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn do_compilation() -> Result<(), String> {
    let mut strings = StringMap::new();
    // parse cli args
    const CLI_ARG_MAIN: CliArg = CliArg::optional("m", "specifies the path of the main procedure", &["full-main-proc-path"]);
    const CLI_ARG_TARGET: CliArg = CliArg::required("t", "specifies the target format", &["target-format ('c' / 'js')"]);
    const CLI_ARG_OUTPUT: CliArg = CliArg::required("o", "specifies the output file", &["output-file"]);
    const CLI_ARG_DISABLE_COLOR: CliArg = CliArg::optional("c", "disables colored output", &[]);
    const CLI_ARG_CALL_DEPTH: CliArg = CliArg::optional("s", "overwrites the maximum call depth", &["max-call-depth"]);
    let arg_list = CliArgList::new()
        .add(CLI_ARG_MAIN)
        .add(CLI_ARG_TARGET)
        .add(CLI_ARG_OUTPUT)
        .add(CLI_ARG_DISABLE_COLOR);
    let args = CliArgs::parse(&arg_list, &env::args().collect::<Vec<String>>()[1..]).map_err(|e| display_errors(vec![e], &mut strings, true))?;
    let target_str = args.values(CLI_ARG_TARGET)
        .expect("is required")
        .last()
        .expect("is required to have one value")
        .clone();
    let main_proc = args.values(CLI_ARG_MAIN)
        .map(|vals| vals.last().expect("is required to have one value").clone());
    let output_file = args.values(CLI_ARG_OUTPUT)
        .expect("is required")
        .last()
        .expect("is required to have one value")
        .clone();
    let color = args.values(CLI_ARG_DISABLE_COLOR)
        .is_none();
    let mut files = HashMap::new();
    for file_path in args.free_values() {
        files.insert(
            strings.insert(file_path),
            read_file(file_path, &mut strings).map_err(|e| display_errors(vec![e], &mut strings, color))?
        );
    }
    let max_call_depth = match args.values(CLI_ARG_CALL_DEPTH)
        .map(|v| v.last().expect("is required to have one value").clone()) {
        Some(v) => if let Ok(n) = v.parse::<usize>() { Some(n) }
            else { return Err(display_errors(vec![Error::new([
                ErrorSection::Error(ErrorType::NotANumber(v))
            ].into())], &mut strings, color)); },
        None => None
    };
    let output = compiler::compile(&mut strings, files, &target_str, main_proc, max_call_depth, color)
        .map_err(|e| display_errors(e, &mut strings, color))?;
    write_file(&output_file, output).map_err(|e| display_errors(vec![e], &mut strings, color))?;
    return Ok(());
}

pub fn read_file(
    file_path: &String,
    strings: &mut StringMap,
) -> Result<StringIdx, Error> {
    fs::read_to_string(file_path)
        .map(|c| Ok(strings.insert(&c)))
        .unwrap_or_else(|e| Err(Error::new([
            ErrorSection::Error(ErrorType::FileSystemError(e.to_string())),
            ErrorSection::Info(format!("While trying to read '{}'", file_path))
        ].into())))
}

pub fn write_file(path: &String, content: String) -> Result<(), Error> {
    return fs::write(path, content).map_err(|error| Error::new([
        ErrorSection::Error(ErrorType::FileSystemError(error.to_string())),
        ErrorSection::Info(format!("While trying to write to '{}'", path))
    ].into()))
}