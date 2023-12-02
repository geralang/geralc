# Gera

*The compiler for Gera, a statically typed and garbage collected programming language without type annotations.*

A VS Code plugin for Gera is available [here](https://github.com/typesafeschwalbe/vscode-gera).

If you would like to read some Gera code, you can read solutions for [Advent of Code 2023](https://adventofcode.com/2023) written in Gera [here](https://github.com/typesafeschwalbe/gera-aoc).

# Progress

*This is a rough outline of the progress made. For further details, feel free to have a look at [the Trello board](https://trello.com/b/BaAKwZsO/gera).*

- [x] Lexer
- [x] Parser
- [x] Pretty errors
- [x] Grammar checking
- [x] Modules
- [x] Type checking
- [x] Interpreter for constants
- [x] Headers for external procedures
- [x] Lowering of AST into SSA IR
- [x] C code generation
- [ ] Standard library (core)
- [ ] Standard library (target = C)
- [x] Javascript code generation
- [ ] Standard library (target = Javascript)
- [ ] Optimizations on IR