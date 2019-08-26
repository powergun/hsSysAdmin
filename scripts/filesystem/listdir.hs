#!/usr/bin/env stack runghc

import           System.Directory

main :: IO ()
main = do
  a <- getDirectoryContents "/var/tmp"
  print a
