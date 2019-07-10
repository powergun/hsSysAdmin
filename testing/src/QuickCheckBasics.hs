module QuickCheckBasics
 ( runTests
 ) where

import Test.QuickCheck
import Data.List

-- real world haskell P/296

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where lhs = filter (<  x) xs
        rhs = filter (>= x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs

-- P/298
-- for empty list case we really want to say if the list is 
-- nonempty then .... this is done using the (==>) implication
-- function, which filters out invalid data before running the 
-- property
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs  
prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

prop_ordered xs = ordered (qsort xs)
  where ordered [] = True
        ordered [x] = True
        ordered (x:y:xs) = x <=y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
  -- a permutation of the input, achieved via the list diff function
  where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_append xs ys = not (null xs) ==> not (null ys) ==>
  head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

-- real world haskell P/299
-- testing against a model
-- test it against a model implementation
-- we can tie our impl of list sort to the reference sort func
-- in the standard library, and if they behave the same, we 
-- gain confidence that our sort does the right thing
-- often developers will have a reference implementation or 
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- prototype that, while inefficient, is correct
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- MY NOTE: recall all these algorithm books, work on the naive 
-- impl and make it faster - this method really helps me to work 
-- in this direction
prop_sort_model xs = sort xs == qsort xs

runTests :: IO ()
runTests = do
  print "//// run QuickCheck tests"

  -- real world haskell P/297
  -- QuickCheck generates test data such as this and passes it 
  -- to the property of our choosing, via the quickCheck function
  -- the type of the property itself determines which data 
  -- generator is used
  -- quickCheck then checks that for all the test data produced, 
  -- the property is satisfied
  quickCheck (prop_idempotent :: [Integer] -> Bool)
  -- when developing tests, it is often useful to see the actual 
  -- data generated for each test. To do this, we would replace
  -- quickCheck with its sibling, verboseCheck()
  
  -- OK, passed 100 tests; 23 discarded.
  quickCheck (prop_minimum :: [Integer] -> Property)
  quickCheck (prop_maximum :: [Integer] -> Property)
  quickCheck (prop_append :: [Integer] -> [Integer] -> Property)
  -- P/298
  -- note that we had to change the type of the property from 
  -- being a simple Bool result to the more general Property 
  -- type (the property itself is now a function that filters 
  -- non-empty lists, before testing them, rather than a simple
  -- Boolean constant)
  quickCheck (prop_minimum :: [Integer] -> Property)
  quickCheck (prop_permutation :: [Integer] -> Bool)

  quickCheck (prop_sort_model :: [Integer] -> Bool)
