{-# LANGUAGE OverloadedStrings #-}

module StrFormatting.TrivialFormat (demo) where

import           Prelude hiding (FilePath)
import           Turtle

demo :: IO ()
demo = do
  print $ format (s% " --- " %d) "iddqd" 12312
  print $ format ("-- " % s % " // " % d) "idkfa" 9812
  -- NOTE: this the return type of format is not compatible with String/[Char]
  -- printf() must be used with the driver (sh etc..)

  -- NOTE: the first argument to format must be named fp
  -- Prelude also exports a FilePath type (alias to String), must hide it
  filename <- pwd
  print $ format fp filename
  print $ format fp ("/ab/v" :: FilePath)

