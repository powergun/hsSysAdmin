{-# LANGUAGE NoImplicitPrelude #-}

module DemoHelloAliceSmithSpec (spec) where

import           RIO
import           Test.Hspec

import qualified Types.HelloAliceSmith

spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
      Types.HelloAliceSmith.demo
