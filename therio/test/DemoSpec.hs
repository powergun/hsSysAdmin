{-# LANGUAGE NoImplicitPrelude #-}
module DemoSpec (spec) where

import           RIO
import           Test.Hspec

import qualified Env.Simple
import qualified Logging.DoLogging

spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
      Logging.DoLogging.demo
      Env.Simple.demo
