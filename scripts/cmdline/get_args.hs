#!/usr/bin/env stack runghc

import System.Environment (getArgs, getProgName)
import Control.Monad (when)

-- real world haskell P/112

-- real world haskell P/230
import System.Console.GetOpt

main :: IO ()
main = do
  -- to test
  -- ./simple_roundtrip.hs asd asd
  -- ["asd","asd"]
  getArgs >>= print
  getProgName >>= print

  -- sanity check the arguments
  -- haskell cookbook L2833
  args <- getArgs
  when (length args /= 2) $ do
    putStrLn $ "Incorrect arguments " ++ (show args)
    error "Provide args"
