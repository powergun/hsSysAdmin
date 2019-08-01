#!/usr/bin/env stack runghc

import           Control.Monad         (forM, forM_)
import qualified Control.Monad.Writer  as W
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.List             (isSuffixOf, sortBy)
import qualified Data.Map              as M
import qualified Data.Text             as T
import           System.Directory      (doesDirectoryExist,
                                        getDirectoryContents)
import           System.FilePath       ((</>))
import           System.FilePath.Posix (takeExtension)

-- the basic skeleton of a static analysis tool

readFileSafe :: FilePath -> IO String
readFileSafe pth = do
  bytes <- B.readFile pth
  return $ BC.unpack bytes

preFilter :: (FilePath, FilePath) -> Bool
preFilter (pth, baseName)
  | baseName `elem` [".", "..", ".git", ".terraform"] = False
  | ".com" `isSuffixOf` pth = False
  | otherwise = True

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  let pathAndNames = map (\n -> ((topDir </> n), n)) names
  paths <- forM (filter preFilter pathAndNames) $ \(pth, name) -> do
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

display :: WalkResult -> IO ()
display (WalkResult m) = do
  let sorted = sortBy (\(k1, v1) (k2, v2) -> v2 `compare` v1) $ M.toList m
  forM_ sorted $ \(k, v) -> do
    print $ (T.unpack $ T.justifyRight 10 ' ' (T.pack $ show v)) ++ "  " ++ k

main :: IO ()
main = do
  (_, r) <- W.runWriterT (walkM "..")
  display r
  -- (_, r2) <- W.runWriterT (walkM "/Users/wein/work/dev/canva/infrastructure")
  -- display r2

