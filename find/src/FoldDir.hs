module FoldDir where

import Data.Char
import System.Directory
import System.FilePath

import FsUtils

-- real world haskell P/270
-- foldr, foldl neatly generalize the idea of traversing a list
-- while accumulating a result. It's hardly a stretch to extend 
-- the idea of folding from lists to directory trees, but we'd
-- like to add an element of control to our fold. We'll represent
-- this control as an algebraic data type

-- Done: cease traversal and return wrapped value
-- Skip: if the current Info type represents a dir, skip it
-- Continue: use the wrapped value as the input to the next call
--           to the fold function
data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

-- the Iterator type gives us an alias for the function that 
-- we fold with
-- it takes a seed and an Info value, representing a directory 
-- entry and returns both a new seed and an instruction for our 
-- fold function, where the instructions are represented as 
-- the constructors of the Iterate type
type Iterator seed = seed -> Info -> Iterate seed

-- P/270
-- our fold is a kind of left fold, because we start folding 
-- from the first entry we encounter (tail recur)
-- the seed for each step is the result of the prior step
-- MY NOTE:
-- review algorithms/reduction/fold : demoFoldl
-- fold func init elems
-- each time "init" grows with the compute result taken from the 
-- current elem;
-- each time elems get reduced
-- in the last call init contains the results aggregated from 
-- m previous computation, where m = n - 1
-- in the last call there is only one remaining elem to compute
-- with
foldTree :: Iterator a -> a -> FilePath -> IO a

foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)

  where
    fold seed subpath = do 
      names <- getUsefulContents subpath
      -- NOTE: there is a bug in the book's example where the paths
      -- are created inside the walk() function; it incorrectly 
      -- uses the toplevel "path" argument as the dirname which 
      -- leads to non-existing filename, hencing stopping the  
      -- traversal from going deeper (where getInfo fails)
      -- my fix is to create the path list in the fold() function
      -- and pass them to walk()
      -- confirm that this solves the issue and atMostThreeShellScripts()
      -- can correctly return 3 shell script file paths
      let paths = map (subpath </>) names
      walk seed paths
    
    -- real world haskell P/271
    -- walk() is a tail recursive loop, instead of an anonymous
    -- function called by forM as in our earlier functions;
    -- By taking the reins ourselves, we can stop early if we 
    -- need to, which lets us drop out when our iterator returns 
    -- Done
    walk seed (path':paths') = do
      info <- getInfo path'
      case iter seed info of
      -- same calling convention as atMostThreeShellScripts'
        done@(Done _) -> do
          return done
        Skip seed' -> do
          walk seed' paths'
        Continue seed'
          | isDirectory info -> do
              -- when fold is called by walk() and returns, walk()
              -- examines its result to see whether it should continue
              -- or drop out (Done)
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed'' -> walk (unwrap seed'') paths'
          | otherwise -> do
            walk seed' paths'
    
    walk seed _ = do 
      return (Continue seed)

atMostThreeShellScripts :: Iterator [FilePath]
--                         alias      param (seed type is [FilePath])
-- Note that arglist and return type has been omitted because 
-- Iterator is an alias to a complete function definition 
atMostThreeShellScripts paths info
--        (     Iterator   )  arg1
  | length paths == 3 
    = Done paths
  | isDirectory info && takeFileName path == ".git" 
    = Skip paths
  | extension `elem` [".sh"] 
    = Continue (path : paths)
  | otherwise 
    = Continue paths
  where
    extension = map toLower (takeExtension path)
    path = infoPath info

-- can not execute IO action here, meaning I can NOT query the 
-- directory contents in any of the iterators
everything :: Iterator [FilePath]
everything paths info
  = Continue ((infoPath info):paths)

countDir count info = 
  Continue (if isDirectory info
            then count + 1
            else count)