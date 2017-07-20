# Work file
The work file is `.pp-work/work.yaml`, used by the `Work` module of the CLI.

The structure of the file is defined in Haskell like this:

  ```haskell
  -- |Working file structure
  newtype Config = Config
    { files :: [ConfigFile]
    } deriving (Eq, Show)
  data ConfigFile = ConfigFile
    { filepath :: FilePath
    , fileid   :: String
    , filehash :: [ConfigHash]
    } deriving (Eq, Show)
  data ConfigHash = ConfigHash
    { hashname  :: String
    , hashvalue :: String
    } deriving (Eq, Show)
  ```

For more details, please take a look at the `Work` module in the `cli/` folder.
