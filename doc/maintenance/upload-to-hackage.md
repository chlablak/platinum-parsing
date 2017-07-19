# Upload to Hackage
This document is about the upload procedure to Hackage.

## Step 0: update the project version
Project version must be updated in `platinum-parsing.cabal` file.

The following convention must be followed: https://pvp.haskell.org/

## Step 1: check building
Run:

  ```console
  $ stack build
  $ stack test
  $ stack haddock platinum-parsing
  ```

Until everything is ok.

## Step 2: generate the gzipped tarball
Run:

  ```console
  $ stack sdist
  $ cp .stack-work/dist/<...>/platinum-parsing-A.B.C.D.tar.gz hackage/
  ```

## Step 3: do a `git tag` of the new version
After a full commit, run:

  ```console
  $ git tag -a vA.B.C.D -m "useful message"
  $ git push --tags
  ```

Eventually, use `github_changelog_generator`: https://github.com/skywinder/Github-Changelog-Generator

## Step 4: upload to Hackage
Use instructions: http://hackage.haskell.org/upload

First do a _Package Candidates_, then if everything is ok, publish permanently.
