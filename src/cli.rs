
use crate::util::error::{Error, ErrorSection, ErrorType};

use std::collections::HashMap;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CliArg {
    name: &'static str,
    required: bool,
    value_count: usize
}

impl CliArg {
    pub const fn optional(name: &'static str, value_count: usize) -> CliArg {
        CliArg { name, required: false, value_count }
    }
    pub const fn required(name: &'static str, value_count: usize) -> CliArg {
        CliArg { name, required: true, value_count }
    }
}


pub struct CliArgs {
    arg_values: HashMap<CliArg, Vec<String>>,
    free_values: Vec<String>
}

impl CliArgs {
    pub fn parse(args: &[CliArg], env_args: &[String]) -> Result<CliArgs, Error> {
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
                for searched_arg in args {
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
                    if collected_args.len() < arg.value_count {
                        return Err(Error::new([
                            ErrorSection::Error(ErrorType::InvalidArgumentCount(arg_name, arg.value_count, collected_args.len()))
                        ].into()));
                    }
                    parsed.arg_values.insert(*arg, collected_args);
                } else {
                    return Err(Error::new([
                        ErrorSection::Error(ErrorType::ArgumentDoesNotExist(arg_name)),
                        ErrorSection::Help(format!("List of available arguments: {}", args.iter().map(|a| format!("\n- {}", a.name)).collect::<Vec<String>>().join("\n")))
                    ].into()));
                }
            } else {
                parsed.free_values.push(current.clone());
                i += 1;
            }
        }
        Ok(parsed)
    }

    pub fn free_values(&self) -> &Vec<String> { &self.free_values }
    pub fn values(&self, arg: CliArg) -> Option<&Vec<String>> { self.arg_values.get(&arg) }
}

