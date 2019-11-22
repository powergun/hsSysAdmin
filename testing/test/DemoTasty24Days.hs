
module DemoTasty24Days (demo) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck

import Data.List

demo :: IO ()
demo = defaultMain $
  testGroup "Tests"
    [ testGroup "(checked by SmallCheck)"
        [ testProperty "sort == sort . reverse" $
            \list -> sort (list :: [Int]) == sort (reverse list)

        , testProperty "Fermat's last theorem" $
            1 == 1
        ]

    , testGroup "Unit tests"
        [ testCase "List comparison (different length)" $
            [1, 2, 3] `compare` [1,2] @?= GT

        , testCase "List comparison (same length)" $
            [1, 2, 3] `compare` [1, 2, 12] @?= LT
        ]
    ]
