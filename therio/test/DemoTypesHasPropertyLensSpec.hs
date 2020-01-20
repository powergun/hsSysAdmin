{-# LANGUAGE NoImplicitPrelude #-}

module DemoTypesHasPropertyLensSpec (spec) where

import           RIO
import           Test.Hspec

import qualified Types.HasPropertyLens

spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
      Types.HasPropertyLens.demo
