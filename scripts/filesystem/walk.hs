#!/usr/bin/env stack runghc
-- real world haskell P/253
-- see find / RecursiveContents.hs

import Control.Monad (forM, forM_)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.List (isSuffixOf)

accept :: FilePath -> Bool
accept filePath = filePath `notElem` [".", "..", ".git"]

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  paths <- forM (filter accept names) $ \name -> do
    -- path computed by pure function hence let
    let path = topDir </> name
    -- P/254 doesDirectoryExist is an action; its return type is 
    -- IO Bool; use <- to get the Bool result of the action out 
    -- of its IO wrapper
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

preFilter :: FilePath -> Bool
preFilter filePath = filePath `notElem` [".", "..", ".git"]

process :: FilePath -> IO WalkResult
process pth = do
  case (".hs" `isSuffixOf` pth) of
    True -> do 
      contents <- readFile pth
      putStrLn $ pth ++ "   " ++ (show $ length (lines contents))
    False -> return ()
  return WalkResult

data WalkResult = WalkResult
walk :: FilePath -> IO WalkResult
walk topDir = do
  names <- getDirectoryContents topDir
  forM_ (filter preFilter names) $ \name -> do
    let pth = topDir </> name
    isDirectory <- doesDirectoryExist pth
    if isDirectory
      then walk(pth)
      else process(pth)
  return WalkResult

demoGetRecursiveContents :: IO ()
demoGetRecursiveContents = do
  print "//// demo get recursive contents"
  contents <- getRecursiveContents "../.."
  print $ length contents

demoWalk :: IO ()
demoWalk = do
  print "//// walk with filter"
  result <- walk "../.."
  return ()

main :: IO ()
main = do
  demoGetRecursiveContents
  demoWalk
