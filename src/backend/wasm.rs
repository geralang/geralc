
use crate::backend::ir::{IrSymbol, IrTypeBank, IrTypeBankMapping};
use crate::frontend::modules::NamespacePath;
use crate::util::strings::StringMap;

pub fn generate_wasm(
    _symbols: Vec<IrSymbol>,
    _types: IrTypeBank,
    _type_dedup: IrTypeBankMapping,
    _main_procedure_path: NamespacePath,
    _strings: &mut StringMap
) -> String {
    todo!("generate WebAssembly")
}



