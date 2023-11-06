
use crate::backend::ir::{IrSymbol, IrTypeBank};
use crate::frontend::modules::NamespacePath;
use crate::util::strings::StringMap;

pub fn generate_wasm(_symbols: Vec<IrSymbol>, _types: IrTypeBank, _main_procedure_path: NamespacePath, _strings: &mut StringMap) -> String {
    todo!("generate WebAssembly")
}



