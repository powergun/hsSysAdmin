{-# LANGUAGE OverloadedStrings #-}

module InputOutput.RWFiles (demo) where

{-
source
http://hackage.haskell.org/package/turtle-1.5.15/docs/Turtle-Tutorial.html
-}

import Turtle

demo :: IO ()
demo = do
  -- read
  stdout (input "./testdata/records.txt")

  -- write
  output "/var/tmp/sut/thereisacow.txt" ("-Test" <|> "-abc" <|> "-3242")
  stdout (input "/var/tmp/sut/thereisacow.txt")
