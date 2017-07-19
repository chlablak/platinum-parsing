# EBNF Cheatsheet
PP implements EBNF as defined in the ISO/IEC 14977:1996.

Production rules are of form:

  ```ebnf
  exprs = expr, { "\n", expr }, [ "\n" ] ;
  expr = number, binop, number ;
  ```

Terminals are enclosed with `"`.

Lexical rules are of form:

  ```ebnf
  number %= "[1-9]", digit, "+" ;
  digit %= "[0-9]" ;
  binop %= "[+-]" ;
  ```

You can always check your EBNF grammar with the CLI:

  ```console
  $ pp ebnf --help
  ```

Take a look at `PP.Grammars.*` modules for more details.
