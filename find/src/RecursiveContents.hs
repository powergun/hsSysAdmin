module RecursiveContents
  ( getRecursiveContents
  ) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
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