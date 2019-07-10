module Main where

import Data.List
import System.FilePath (takeExtension)

import RecursiveContents (getRecursiveContents)
import BetterPredicate (betterFind)
import ControlledVisit (traverss)
import FoldDir

-- real world haskell P/256
-- example
-- simpleFind (\p -> takeExtension p == ".sh") ".." >>= (print . take 10)
simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)

main :: IO ()
main = do
  print "//// demo getRecursiveContents()"
  getRecursiveContents ".." >>= (print . take 10)
  print "//// demo simpleFind()"
  simpleFind (\p -> takeExtension p == ".sh") ".." >>= (print . take 10)
  print "//// demo betterFind()"
  betterFind sizeCheckOnly ".." >>= (print . take 10)
  print "//// demo traverss()"
  traverss sort ".." >>= (print . take 3)
  print "//// demo foldTree()"
  foldTree atMostThreeShellScripts [] ".." >>= print
  foldTree countDir 0 ".." >>= print
  where
    -- sizeCheckOnly fn perms sz t =
    --   case sz of
    --     Just nsz -> nsz < 3000
    --     Nothing -> False
    -- this is better!
    sizeCheckOnly _ _ (Just sz) _ = sz < 3000
    sizeCheckOnly _ _ _ _ = False
  