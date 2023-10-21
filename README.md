# Gera

*The compiler for Gera, a satically typed and garbage collected programming language.* Currently WIP (Work in progress).

# Progress

*This is a rough outline of the progress made. For further details, feel free to have a look at [the Trello board](https://trello.com/b/BaAKwZsO/gera).*

- [x] Lexer
- [x] Parser
- [x] Pretty errors
- [x] Grammar checking
- [x] Modules
- [x] Type checking
- [x] Interpreter for constants
- [ ] Headers for external procedures
- [ ] Lowering of AST into SSA IR
- [ ] Code generation from IR
    - [ ] Native target (C / assembly?)
    - [ ] Browser-compatible target (Javascript / WASM?)
    - [ ] Serialized IR (readable by compiler as dependency)?