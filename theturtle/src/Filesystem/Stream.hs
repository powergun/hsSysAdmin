{-# LANGUAGE OverloadedStrings #-}

module Filesystem.Stream (demo) where

import Turtle

import qualified Control.Foldl as Fold
-- see also:
-- http://hackage.haskell.org/package/foldl-1.4.5/docs/Control-Foldl.html

demo :: IO ()
demo = do
  view (ls "/var/tmp/sut")
  view (liftIO $ readFile "/etc/shells")
  view $ fold (ls "/var/tmp/sut") Fold.length
