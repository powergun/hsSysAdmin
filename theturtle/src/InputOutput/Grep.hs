{-# LANGUAGE OverloadedStrings #-}

module InputOutput.Grep (demo) where

-- source: http://hackage.haskell.org/package/turtle-1.5.15/docs/Turtle-Prelude.html#v:grep

import           Turtle

demoSingleFileGrep :: IO ()
demoSingleFileGrep =
  stdout (grep "id" (input "./testdata/records.txt"))

demo :: IO ()
demo = do
  demoSingleFileGrep
