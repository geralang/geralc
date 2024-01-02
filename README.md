<p align="center"><img src="./banner.png" height=400/></p>

The Gera compiler is not intended to be used on its own. It is recommended to put `gerac` into the PATH and use `gerap` (the Gera package manager) to build / run code.

### Links

- [Package manager](https://github.com/typesafeschwalbe/gerap)
- [Standard library](https://github.com/typesafeschwalbe/gerastd)
- [VS Code extension](https://github.com/typesafeschwalbe/vscode-gera)
- [C target core dependencies](https://github.com/typesafeschwalbe/geraccoredeps)

### Progress

- [x] Lexer
- [x] Parser
- [x] Pretty errors
- [x] Grammar checking
- [x] Modules
- [x] Type checking
- [x] Interpreter for constants
- [x] Mapping files for external procedures
- [x] Lowering of AST into SSA IR
- [x] C code generation
- [x] Javascript code generation
- [ ] Complete standard library
- [ ] Optimizations on IR
