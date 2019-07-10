#!/usr/bin/env stack runghc

-- real world haskell P/214

import System.Directory
import System.IO

demoDeleteFile :: IO ()
demoDeleteFile = do
  print "//// demo delete file"
  -- file exists
  writeFile "/var/tmp/sut/__todel" "asd"
  renameFile "/var/tmp/sut/__todel" "/var/tmp/sut/todel"

  removeFile "/var/tmp/sut/todel"
  -- file does not exist / exit 1
  removeFile "/var/tmp/sut/todel"

main :: IO ()
main = do
  demoDeleteFile
