{-# LANGUAGE NoImplicitPrelude #-}
module DemoTypesHasPropertySpec (spec) where

import           RIO
import           Test.Hspec

import qualified Types.HasProperty

spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
      Types.HasProperty.demo
