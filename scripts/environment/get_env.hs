#!/usr/bin/env stack runghc

-- real world haskell P/231

import System.Environment (getEnv, getEnvironment)

main :: IO ()
main = do
  -- getEnv raises an exception if it doesn't find the var
  getEnv "HOME" >>= print
  -- getEnvironment returns the whole env as [(String, String)]
  -- and then you can use functions such as lookup to find the 
  -- var
  getEnvironment >>= (\env -> print $ "HOME" `lookup` env)

-- Setting env var is not defined in a cross-platform awy
-- if you are on POSIX, you can use putEnv or setEnv from
-- System.Posix.Env
