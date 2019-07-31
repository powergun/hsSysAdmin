#!/usr/bin/env stack runghc

import Control.Monad (forM, forM_)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.List (isSuffixOf)
import qualified Data.Map as M

-- the basic skeleton of a static analysis tool
preFilter :: FilePath -> Bool
preFilter filePath = filePath `notElem` [".", "..", ".git"]

process :: FilePath -> IO WalkResult
process pth = do
  case (".hs" `isSuffixOf` pth) of
    True -> do 
      contents <- readFile pth
      putStrLn $ pth ++ "   " ++ (show $ length (lines contents))
    False -> return ()
  return emptyResult

walk :: FilePath -> IO WalkResult
walk topDir = do
  names <- getDirectoryContents topDir
  forM_ (filter preFilter names) $ \name -> do
    let pth = topDir </> name
    isDirectory <- doesDirectoryExist pth
    if isDirectory
      then walk(pth)
      else process(pth)
  return emptyResult

data WalkResult = WalkResult (M.Map String Int)
                  deriving (Show)
emptyResult = WalkResult M.empty
instance Semigroup WalkResult where
  (<>) = mappend
instance Monoid WalkResult where
  mempty = emptyResult
  mappend (WalkResult m1) (WalkResult m2) = 
    WalkResult (M.unionWith (+) m1 m2)

demoWalkResultAsMonoid :: IO ()
demoWalkResultAsMonoid = do
  let m1 = WalkResult $ M.fromList [(".hs", 1)]
      m2 = WalkResult $ M.fromList [(".hs", 23), (".json", 12)]
  print $ emptyResult
  print $ m1 `mappend` m2

main :: IO ()
main = do
  print 1
