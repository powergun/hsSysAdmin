{-# LANGUAGE OverloadedStrings #-}

module Logging.DoLogging (demo) where

import           RIO

demo :: IO ()
demo = runSimpleApp $ do
  logDebug "Debug"
  logInfo "Info"
  logWarn "Warn"
  logError "Error"
