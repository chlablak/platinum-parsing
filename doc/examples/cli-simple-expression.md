# Example: simple expression
This tutorial aim to provide a quick introduction of usage.

The objective is to parse a simple expression `23-34+87` and get the AST of it, using PP.

We will explore the use of the CLI.

You can find the same example but for the Framework at `doc/examples/framework-simple-expression.md`.

## Step 1: writing the grammar
For this simple language, copy the following grammar in a file named `grammar.ebnf`:

  ```ebnf
  (* This production is a syntax rule *)
  (* It will be used by the parser *)
  Expr = Number, { BinOP, Number }, "\n" ;

  (* Those production are lexical rules (regular expression) *)
  (* They will be used by the lexer to produce tokens *)
  Number %= "[1-9][0-9]*" ;
  BinOP %= "[+-]" ;
  (* newline needed at the end *)
  ```

## Step 2: checking the grammar
Here we will use the CLI to ensure that the grammar is well-formed:

  ```console
  $ pp ebnf -f grammar.ebnf --check
  [PP][INFO] starting...
  [PP][INFO] verbosity: 30
  [PP][INFO] working directory set to: .
  [PP][EBNF][CHECK][INFO] errors:
  [PP][EBNF][CHECK][INFO] warnings:
  [PP][INFO] bye.
  ```

No error or warning was found ! Try to explore other options to discover the `pp ebnf` command:

  ```console
  $ pp ebnf --help
  ```

## Step 3: building lexer and parser tables
Now we can build the DFA and LALR table for our grammar:

  ```console
  $ pp -w lalr -f grammar.ebnf
  [PP][INFO] starting...
  [PP][INFO] verbosity: 30
  [PP][INFO] working directory set to: .
  [PP][INFO] current working directory: /foo/bar/examples/simple-expression
  [PP][INFO] create directory: .pp-work/
  [PP][LALR][TASK][START] compute collection and table
  [PP][LALR][WORK][INFO] ("grammar.ebnf","collection") modified, use computation and save
  [PP][LALR][WORK][INFO] ("grammar.ebnf","table") modified, use computation and save
  [PP][LALR][TASK][END] in 4ms, compute collection and table
  [PP][LALR][TASK][START] compute DFA
  [PP][LALR][WORK][INFO] ("grammar.ebnf","dfa") modified, use computation and save
  [PP][LALR][TASK][END] in 15ms, compute DFA
  [PP][INFO] bye.
  ```

The `-w` option tell the CLI to use the `.pp-work/` folder to save computation, thus next call to the CLI for this grammar will be much faster.

You can explore the `pp lalr` command's options the same way as for `pp ebnf`.

## Step 4: testing on a source file
Write in a file `expr.txt` the following (with a empty line):

  ```text
  23-34+87

  ```

We will test if our grammar is capable of parsing this expression:

  ```console
  $ pp -ws lalr -f grammar.ebnf -t expr.txt --ast
  |Expr
    |[OToken2 "23" "Number"]
    |{<BinOP>,<Number>}
      |[OToken2 "-" "BinOP"]
      |[OToken2 "34" "Number"]
      |{<BinOP>,<Number>}
        |[OToken2 "+" "BinOP"]
        |[OToken2 "87" "Number"]
        |{<BinOP>,<Number>}
    |[OToken2 "\n" "__token_\n"]

  after 14 iterations:
  input accepted
  ```

That's it !

You can take a look at the CLI module `Cmd.Lalr` for example of the Framework usage.
