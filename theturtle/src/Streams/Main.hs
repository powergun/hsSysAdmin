{-# LANGUAGE OverloadedStrings #-}

module Streams.Main where

import           Control.Monad (forM_)
import           Data.Text     (isInfixOf)
import           Prelude       hiding (FilePath)
import           Turtle

process :: FilePath -> Shell ()
process filename = do
  fn <- ls filename  -- sh drives the "iteration"
  liftIO $ print fn

-- use mfilter (using a pure predicate function)
procFilter :: FilePath -> Text -> Shell ()
procFilter filename word = do
    fn <- mfilter predicate $ ls filename
    liftIO $ print fn
  where
    predicate :: FilePath -> Bool
    predicate filename =
      let fn = format fp filename
      in isInfixOf word fn

main :: IO ()
main = do
  -- `view empty` raises ambiguity problem
  view (return 1)
  view (return "~." <|> return "asd")
  sh $ process "./testdata"
  sh $ procFilter "." ".yaml"
