#!/usr/bin/env stack runghc

import Control.Monad (forM_, forM)
import qualified Control.Monad.Writer as W
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.FilePath.Posix (takeExtension)
import Data.List (isSuffixOf)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- the basic skeleton of a static analysis tool

readFileSafe :: FilePath -> IO String
readFileSafe pth = do
  bytes <- B.readFile pth
  return $ BC.unpack bytes

preFilter :: FilePath -> Bool
preFilter baseName
  | baseName `elem` [".", "..", ".git"] = False
  | otherwise = True

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  paths <- forM (filter preFilter names) $ \name -> do
    let path = topDir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

analysis :: [FilePath] -> W.WriterT WalkResult IO ()
analysis [] = return ()
analysis (pth:pths) = do
  let ext = takeExtension pth
  contents <- W.lift (readFileSafe pth)
  W.tell $ WalkResult $ M.fromList [(ext, (length . lines) contents)]
  analysis pths

walkM :: FilePath -> W.WriterT WalkResult IO ()
walkM topDir = do
  pths <- W.lift $ getRecursiveContents topDir
  analysis pths

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

-- display :: WalkResult -> IO ()
-- display (WalkResult m) = do


main :: IO ()
main = do
  (_, r) <- W.runWriterT (walkM "../..")
  print r
  -- (_, r2) <- W.runWriterT (walkM "/Users/wein/work/dev/canva/infrastructure")
  -- print r2

