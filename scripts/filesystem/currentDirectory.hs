#!/usr/bin/env stack runghc

-- source: tui youtube video
import           System.Directory

main :: IO ()
main = do
  a <- getCurrentDirectory
  print a

