{-# LANGUAGE OverloadedStrings #-}

module Logging.DoLogging (demo) where

import           RIO

demo :: IO ()
demo = runSimpleApp $ do
  logInfo "there is acow"
