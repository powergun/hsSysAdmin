
module DemoQuickCheckRandGen (main) where

import           Control.Monad
import           Data.List

import           QuickCheckRandGen

import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified Test.Tasty.QuickCheck as QC

instance Arbitrary Doc where
 arbitrary =
   oneof [ return Empty
         , liftM Char arbitrary
         , liftM Text arbitrary
         , return Line
         , liftM2 Concat arbitrary arbitrary
         , liftM2 Union arbitrary arbitrary ]

prop_demo :: Doc -> Bool
prop_demo doc = doc == doc

qcProps = testGroup "(checked by QuickCheck)"

  -- For reference only (from official tasty test examples)

  -- [ QC.testProperty "sort == sort . reverse" $
  --     \list -> sort (list :: [Int]) == sort (reverse list)
  -- , QC.testProperty "Fermat's little theorem" $
  --     \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- -- the following property does not hold
  -- , QC.testProperty "Fermat's last theorem" $
  --     \x y z n ->
  --       (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  -- ]

  [ QC.testProperty "iddqd" $
    \doc ->
      id (doc :: Doc) == doc
  ]

main = do
  forM_ [1..100] $ \_ -> generate (arbitrary :: Gen Doc) >>= print

  defaultMain tests
  where
    tests :: TestTree
    tests = testGroup "Tests" [properties]

    properties :: TestTree
    properties = testGroup "Properties" [qcProps]
