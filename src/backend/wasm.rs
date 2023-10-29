
use crate::backend::ir::{IrSymbol, IrTypeBank};
use crate::frontend::modules::NamespacePath;
use crate::util::strings::StringMap;

pub fn generate_wasm(symbols: Vec<IrSymbol>, types: IrTypeBank, main_procedure_path: NamespacePath, strings: &mut StringMap) -> String {
    todo!("generate WebAssembly")
}