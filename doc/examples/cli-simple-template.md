# Example: simple template
This tutorial aim to provide a quick introduction of usage.

The objective is to export the LALR table of a grammar, with a template.

We will explore the use of the CLI.

## Step 1: creating the project
First, create a blank project:

  ```console
  $ pp new -n simple-template
  [PP][INFO] starting...
  [PP][INFO] verbosity: 30
  [PP][INFO] working directory set to: .
  [PP][NEW][INFO] create project into directory: simple-template
  [PP][INFO] bye.
  $ cd simple-template/
  ```

## Step 2: writing a sample grammar
For this example we will use a simple grammar.

Edit the `grammar.ebnf` file as follow:

  ```ebnf
  A = "a" | B ;
  B = "b" ;
  (* newline needed at the end *)
  ```

## Step 3: writing the template
Create a new file `table.template` with the following content:

  ```template
  STATE $length(lr.states)$
  NONTERM $length(lr.nonTerms)$
  $lr.nonTerms:{nonTerm|$nonTerm.name$
  }$TABLE $lr.table.total$
  ROW $length(lr.table.rows)$
  $lr.table.rows:{row|$row.state.id$ $if(row.isTerm)$TERM $if(row.term.isEmpty)$EMPTY$elseif(row.term.isToken)$$row.term.token$$else$$row.term.symbol$$endif$$else$NONTERM $row.nonTerm.name$$endif$ $if(row.action.isReduce)$REDUCE $row.action.reduce.name$ $row.action.reduce.length$$elseif(row.action.isShift)$SHIFT $row.action.shift$$elseif(row.action.isGoto)$GOTO $row.action.goto$$elseif(row.action.isError)$ERROR$else$ACCEPT$endif$
  }$
  ```

This template will simply output a raw text LALR table. Of course you can write more complex template, for example for an other programming language in which you want to use the table.

For more details about the template syntax, take a look at `doc/references/template-processor.md`.

## Step 4: modifying the project configuration
Now we need to tell our project to use our template and compile it.

Modify the `pp.yaml` file as follow:

  ```yaml
  grammars:
  - grammar.ebnf
  use-work: true
  tests: []
  templates:
  - file: table.template
    destination: table.compiled
  name: simple-template
  version: '0.0.0'
  description: A short description
  ```

You can see that we added the `table.template` file, and we want the compiled file to be called `table.compiled`.

## Step 5: building the project
Finally we must just build the project:

  ```console
  $ pp build
  [PP][INFO] starting...
  [PP][INFO] verbosity: 30
  [PP][INFO] working directory set to: .
  [PP][BUILD][INFO] build project: simple-template
  [PP][BUILD][INFO] current working directory: /home/chlablak/Documents/HEIG-VD/S56/TB/projects/simple-template
  [PP][BUILD][INFO] create directory: .pp-work/
  [PP][BUILD][INFO] EBNF checks:
  [PP][BUILD][EBNF][CHECK][INFO] errors:
  [PP][BUILD][EBNF][CHECK][INFO] warnings:
  [PP][BUILD][INFO] LALR generation:
  [PP][BUILD][LALR][TASK][START] compute collection and table
  [PP][BUILD][LALR][WORK][INFO] ("grammar.ebnf","collection") modified, use computation and save
  [PP][BUILD][LALR][WORK][INFO] ("grammar.ebnf","table") modified, use computation and save
  [PP][BUILD][LALR][TASK][END] in 3ms, compute collection and table
  [PP][BUILD][LALR][TASK][START] compute DFA
  [PP][BUILD][LALR][WORK][INFO] ("grammar.ebnf","dfa") modified, use computation and save
  [PP][BUILD][LALR][TASK][END] in 0ms, compute DFA
  [PP][BUILD][INFO] Templates compilation:
  [PP][BUILD][TEMPLATE][INFO] table.template > table.compiled
  [PP][BUILD][INFO] Tests execution:
  [PP][INFO] bye.
  ```

Notice the line: `
[PP][BUILD][TEMPLATE][INFO] table.template > table.compiled`

We can take a look at the `table.compiled` file:

  ```text
  STATE 5
  NONTERM 2
  A
  B
  TABLE 25
  ROW 8
  0 NONTERM A GOTO 4
  0 NONTERM B GOTO 1
  0 TERM __token_a SHIFT 2
  0 TERM __token_b SHIFT 3
  1 TERM EMPTY REDUCE A 1
  2 TERM EMPTY REDUCE A 1
  3 TERM EMPTY REDUCE B 1
  4 TERM EMPTY ACCEPT
  ```

That's it !
