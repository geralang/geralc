
use crate::frontend::modules::NamespacePath;
use crate::util::strings::StringMap;
use crate::backend::ir::{IrSymbol, IrTypeBank};

pub struct CompileTarget(
    pub fn(Vec<IrSymbol>, IrTypeBank, NamespacePath, &mut StringMap) -> String
);
