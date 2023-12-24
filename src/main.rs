
#![allow(dead_code)]

mod cli;

use cli::{CliArgs, CliArg, CliArgList};
use compiler::util::{
    error::{Error, ErrorSection, ErrorType},
    strings::{StringMap, StringIdx}
};

use std::{fs, env, collections::HashMap};


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
    let mut strings = StringMap::new();
    if let Err(errors) = do_compilation(&mut strings) {
        for error in errors {
            println!("{}", error.display(&mut strings, true));
        }
    }
}

pub fn do_compilation(strings: &mut StringMap) -> Result<(), Vec<Error>> {
    // parse cli args
    const CLI_ARG_MAIN: CliArg = CliArg::optional("m", "specifies the path of the main procedure", &["full-main-proc-path"]);
    const CLI_ARG_TARGET: CliArg = CliArg::required("t", "specifies the target format", &["target-format ('c' / 'js')"]);
    const CLI_ARG_OUTPUT: CliArg = CliArg::required("o", "specifies the output file", &["output-file"]);
    let arg_list = CliArgList::new()
        .add(CLI_ARG_MAIN)
        .add(CLI_ARG_TARGET)
        .add(CLI_ARG_OUTPUT);
    let args = CliArgs::parse(&arg_list, &env::args().collect::<Vec<String>>()[1..]).map_err(|e| vec![e])?;
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
    let mut files = HashMap::new();
    for file_path in args.free_values() {
        files.insert(
            strings.insert(file_path),
            read_file(file_path, strings).map_err(|e| vec![e])?
        );
    }
    let output = compiler::compile(strings, files, &target_str, main_proc)?;
    write_file(&output_file, output).map_err(|e| vec![e])?;
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