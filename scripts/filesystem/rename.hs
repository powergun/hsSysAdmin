#!/usr/bin/env stack runghc

-- real world haskell P/251
-- 

import System.FilePath (replaceExtension)
import System.Directory (renameDirectory, renameFile)

demoRenameFile :: IO ()
demoRenameFile = do
  writeFile "/var/tmp/sut/thereisa.cow" "asd"
  renameFile "/var/tmp/sut/thereisa.cow" "/var/tmp/sut/1337"

demoRenameDir :: IO ()
demoRenameDir = do
  renameDirectory "/var/tmp/sut" "/var/tmp/thereisacow"
  renameDirectory "/var/tmp/thereisacow" "/var/tmp/sut" 

main :: IO ()
main = do
  demoRenameFile
  demoRenameDir

