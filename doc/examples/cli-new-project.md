# Example: new project
This tutorial aim to provide a quick introduction of usage.

The objective is to create a new project and build it.

We will explore the use of the CLI.

## Step 1: creating the new project
Creating a new project is pretty simple, all you need is your project name:

  ```console
  $ pp new -n project-name
  [PP][INFO] starting...
  [PP][INFO] verbosity: 30
  [PP][INFO] working directory set to: .
  [PP][NEW][INFO] create project into directory: project-name
  [PP][INFO] bye.
  $ cd project-name/
  $ ls -la
  total 20
  drwxrwxr-x 2 abc abc 4096 Jul 20 13:27 .
  drwxrwxr-x 9 abc abc 4096 Jul 20 13:27 ..
  -rw-rw-r-- 1 abc abc    9 Jul 20 13:27 .gitignore
  -rw-rw-r-- 1 abc abc   28 Jul 20 13:27 grammar.ebnf
  -rw-rw-r-- 1 abc abc  133 Jul 20 13:27 pp.yaml
  ```

It creates an empty  `grammar.ebnf` grammar file and the project configuration file: `pp.yaml`

  ```yaml
  grammars:
  - grammar.ebnf
  use-work: true
  tests: []
  templates: []
  name: project-name
  version: '0.0.0'
  description: A short description
  ```

You can find more details about the project file at `doc/references/project-file.md`.

## Step 2: building the project
Before building, we need to write at least one production rule into the grammar. For this example we will just add:

  ```ebnf
  A = "a" ;
  (* newline needed at the end *)
  ```

Then we use the `pp build` command:

  ```console
  $ pp build
  [PP][INFO] starting...
  [PP][INFO] verbosity: 30
  [PP][INFO] working directory set to: .
  [PP][BUILD][INFO] build project: project-name
  [PP][BUILD][INFO] current working directory: /home/chlablak/Documents/HEIG-VD/S56/TB/projects/project-name
  [PP][BUILD][INFO] create directory: .pp-work/
  [PP][BUILD][INFO] EBNF checks:
  [PP][BUILD][EBNF][CHECK][INFO] errors:
  [PP][BUILD][EBNF][CHECK][INFO] warnings:
  [PP][BUILD][INFO] LALR generation:
  [PP][BUILD][LALR][TASK][START] compute collection and table
  [PP][BUILD][LALR][WORK][INFO] ("grammar.ebnf","collection") modified, use computation and save
  [PP][BUILD][LALR][WORK][INFO] ("grammar.ebnf","table") modified, use computation and save
  [PP][BUILD][LALR][TASK][END] in 8ms, compute collection and table
  [PP][BUILD][LALR][TASK][START] compute DFA
  [PP][BUILD][LALR][WORK][INFO] ("grammar.ebnf","dfa") modified, use computation and save
  [PP][BUILD][LALR][TASK][END] in 1ms, compute DFA
  [PP][BUILD][INFO] Templates compilation:
  [PP][BUILD][INFO] Tests execution:
  [PP][INFO] bye.
  ```

The `build` command reads the `pp.yaml` file and do all the needed works.

That's it !
