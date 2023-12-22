
use crate::{util::error::{Error, ErrorSection, ErrorType}, style_cyan, style_reset, style_dark_red, style_bold_cyan};

use std::collections::HashMap;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CliArg {
    name: &'static str,
    description: &'static str,
    required: bool,
    value_descriptions: &'static [&'static str]
}

impl CliArg {
    pub const fn optional(name: &'static str, description: &'static str, value_descriptions: &'static [&'static str]) -> CliArg {
        CliArg { name, description, required: false, value_descriptions }
    }
    pub const fn required(name: &'static str, description: &'static str, value_descriptions: &'static [&'static str]) -> CliArg {
        CliArg { name, description, required: true, value_descriptions }
    }
}


pub struct CliArgList {
    args: Vec<CliArg>
}

impl CliArgList {
    pub fn new() -> CliArgList {
        return CliArgList { args: Vec::new() }
    } 
    pub fn add(mut self, arg: CliArg) -> Self {
        self.args.push(arg);
        self
    }
    pub fn describe(&self) -> String {
        let mut output = String::from("Available arguments:");
        for arg in &self.args {
            output.push('\n');
            output.push_str(style_bold_cyan!());
            output.push('-');
            output.push_str(arg.name);
            output.push_str(concat!(style_reset!(), style_cyan!()));
            for value_description in arg.value_descriptions {
                output.push(' ');
                output.push('<');
                output.push_str(value_description);
                output.push('>');
            }
            if arg.required {
                output.push_str(concat!(style_dark_red!(), " (Required!)"));
            }
            output.push_str("\n");
            output.push_str(style_reset!());
            output.push_str("  -> ");
            output.push_str(arg.description);
            output.push_str(style_reset!());
        }
        return output;
    }
}



pub struct CliArgs {
    arg_values: HashMap<CliArg, Vec<String>>,
    free_values: Vec<String>
}

impl CliArgs {
    pub fn parse(args: &CliArgList, env_args: &[String]) -> Result<CliArgs, Error> {
        let mut parsed = CliArgs {
            arg_values: HashMap::new(),
            free_values: Vec::new()
        };
        let mut i = 0;
        while i < env_args.len() {
            let current = &env_args[i];
            if current.len() > 1 && current.starts_with("-") {
                let arg_name = String::from(&current[1..]);
                let mut arg = None;
                for searched_arg in &args.args {
                    if searched_arg.name == arg_name {
                        arg = Some(searched_arg);
                        break;
                    }
                }
                if let Some(arg) = arg {
                    i += 1;
                    let mut collected_args = Vec::new();
                    while i < env_args.len() && !env_args[i].starts_with("-") {
                        collected_args.push(env_args[i].clone());
                        i += 1;
                    }
                    if collected_args.len() < arg.value_descriptions.len() {
                        return Err(Error::new([
                            ErrorSection::Error(ErrorType::InvalidArgumentCount(arg_name, arg.value_descriptions.len(), collected_args.len())),
                            ErrorSection::Help(args.describe())
                        ].into()));
                    }
                    parsed.arg_values.insert(*arg, collected_args);
                } else {
                    return Err(Error::new([
                        ErrorSection::Error(ErrorType::ArgumentDoesNotExist(arg_name)),
                        ErrorSection::Help(args.describe())
                    ].into()));
                }
            } else {
                parsed.free_values.push(current.clone());
                i += 1;
            }
        }
        for arg in &args.args {
            if arg.required && !parsed.arg_values.contains_key(arg) {
                return Err(Error::new([
                    ErrorSection::Error(ErrorType::MissingArgument(arg.name)),
                    ErrorSection::Help(args.describe())
                ].into()))
            }
        }
        Ok(parsed)
    }

    pub fn free_values(&self) -> &Vec<String> { &self.free_values }
    pub fn values(&self, arg: CliArg) -> Option<&Vec<String>> { self.arg_values.get(&arg) }
}

