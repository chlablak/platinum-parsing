# Project file
The project file is `project-name/pp.yaml`, created with: `$ pp new -n project-name`

Here is an example of a modified one:

  ```yaml
  name: project-name                # project's name
  version: '0.0.0'                  # project's version
  description: A short description  # project's description
  grammars:                         # a list of grammars used
  - grammar1.ebnf
  - grammar2.ebnf
  use-work: true                    # should the build use the '.pp-work/' folder
  templates:                        # a list of templates
    - file: template1               # template file
      destination: ctemplate1       # compiled template destination
    - file: template2
      destination: ctemplate2
  tests:                            # a list of tests
    - file: test1.txt               # source file to test
      ast: test1-ast.txt            # output result
    - file: test2.txt
      ast: ''                       # no output
  ```
