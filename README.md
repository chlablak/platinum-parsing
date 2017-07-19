# platinum-parsing
www.platinum-parsing.org

Platinum Parsing is an all-around solution for conceiving, developping and building a compiler or an interpreter, or just a part of it. It's composed of two parts: the Framework, written in Haskell, and the CLI (Command Line Interface), exposing the functionalities of the Framework.

## Features
- Purely functional, written in Haskell
- Generic approach to compilation
- Full EBNF grammar support
- Tokens definition with built-in RegEx
- Grammar validation on source file
- Dynamic AST generation
- LALR table and DFA generation
- Built-in lexer and parser
- Customisable template export
- CLI and Atom integration
- And more ... \*

\* Platinum Parsing is a recent project aimed to grow, you can find development axis in the _Issues_ menu.

## Getting started
Platinum Parsing is built with the famous [Haskell Tool Stack](https://www.haskellstack.org/), you will need to install it before continuing.

NB: if you prefer to use `cabal` instead of `stack`, feel free to do so.

Platinum Parsing is available on [Hackage](https://hackage.haskell.org/), thus you can install it with the command:

  ```console
  $ stack install platinum-parsing
  ```

This will cause the CLI to be accessible in your terminal, try: `$ pp --help`

If you want to use the Framework directly in your Haskell project, add to your `<project-name>.cabal` file the following line, and run a `$ stack build` afterwards:

  ```yaml
  build-depends: platinum-parsing
  ```

Last option is to clone this repository and build the project by yourself:

  ```console
  $ git clone git@github.com:chlablak/platinum-parsing.git
  or
  $ git clone https://github.com/chlablak/platinum-parsing.git

  $ cd platinum-parsing
  $ stack init
  $ stack build
  ```

## Further reading
- Platinum Parsing has an [Atom](https://atom.io/) integration (plugin) for ease of development, available [here](https://atom.io/packages/platinum-parsing-atom).
- The library documentation is available on the Hackage [package](https://hackage.haskell.org/package/platinum-parsing).
- More documentation (examples, references, ...) can be found in the `doc/` [folder](doc/).

## Contributing
If you have any question, proposition or contribution, feel free to use the _Issues_ menu.
