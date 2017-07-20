# Project file
The project file is `pp.yaml`, used by the `Project` module of the CLI.

The structure of the file is defined in Haskell like this:

  ```haskell
  -- |Project configuration
  data Project = Project
    { projectName        :: String
    , projectVersion     :: String
    , projectDescription :: String
    , projectGrammars    :: [String]
    , projectTemplates   :: [ProjectTemplate]
    , projectUseWork     :: Bool
    , projectTests       :: [ProjectTest]
    }
    | NoProject
    | MalformedProject String
      deriving (Eq, Show)
  data ProjectTemplate = ProjectTemplate
    { templateFile :: String
    , templateDst  :: String
    } deriving (Eq, Show)
  data ProjectTest = ProjectTest
    { testFile   :: String
    , testAstDst :: String
    } deriving (Eq, Show)
  ```

For more details, please take a look at the `Project` module in the `cli/` folder.

Modules `Cmd.New` and `Cmd.Build` are good places for more informations too.
