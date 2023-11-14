
use crate::frontend::modules::NamespacePath;
use crate::util::strings::StringMap;
use crate::backend::ir::{IrSymbol, IrTypeBank, IrTypeBankMapping};

pub struct CompileTarget(
    pub fn(Vec<IrSymbol>, IrTypeBank, IrTypeBankMapping, NamespacePath, &mut StringMap) -> String
);
