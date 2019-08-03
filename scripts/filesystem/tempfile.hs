#!/usr/bin/env stack runghc

-- real world haskell P/214
-- haskell provides openTempFile and openBinaryTempFile

import           System.Directory
import           System.IO

demoWriteToTempFile :: IO ()
demoWriteToTempFile = do
  print "//// demo write to temp file"
  -- to find the best place for temporary files on a given machine
  dir <- getTemporaryDirectory
  print dir
  -- "motion" is the template for basename; it will have some
  -- random characters added to it to ensure that the result is
  -- truly unique
  -- return a tuple
  (filename, h) <- openTempFile dir "motion"
  print filename
  hPutStrLn h "thereisacow"
  -- when you are done with the filem you will want to hClose it
  -- and call removeFIle to delete it
  hClose h
  removeFile filename

main :: IO ()
main =
  demoWriteToTempFile
