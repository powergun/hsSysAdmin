#!/usr/bin/env stack runghc

import           Control.Monad      (forM_, when)
import           System.Environment (getArgs)
import           System.IO          (readFile)

main :: IO ()
main = do
  args <- getArgs
  when (length args > 0) $ do
    forM_ args $ \filename -> do
      content <- readFile filename
      print content
