{-# LANGUAGE OverloadedStrings #-}

module InputOutput.StdinStdout (demo) where

{-
source:
http://hackage.haskell.org/package/turtle-1.5.15/docs/Turtle-Tutorial.html
-}

import Turtle
import Data.Text (toUpper)
-- see also:
-- http://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html

demo :: IO ()
demo = do
  -- stdin :: Shell Line
  -- stdout:: Shell Line -> IO ()
  -- Note, to extract and process the Line value, I just need
  -- to implement a private processor function:
  -- Line -> Shell Line
  -- (compare this to String -> IO String)
  stdout (process =<< stdin)

{-
how to gracefully handle the Maybe value from textToLine -
I can borrow what I learned from `thecli` project, use a stack:

MaybeT Shell 

Or use the AppError approach to bubble the exception up
-}
process :: Line -> Shell Line
process l = do
  let t = toUpper . lineToText $ l
  case textToLine t of
    Just l' -> return l'
    _ -> return ""
