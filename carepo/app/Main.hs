{-# LANGUAGE TupleSections #-}

-- source
-- inspired by this post
-- https://codereview.stackexchange.com/questions/119926/walking-a-directory-in-haskell

-- how to get file extension
-- http://hackage.haskell.org/package/system-filepath-0.4.14/docs/Filesystem-Path.html#g:3

import           Control.Monad    (forM)
import           Data.List
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath  ((</>))

data RTreeT m a = Node a [m (RTreeT m a)]

walkDir' :: FilePath -> IO (RTreeT IO [FilePath])
walkDir' r = do
  contents      <- fmap (r </>) . exceptLocal <$> getDirectoryContents r
  (files, dirs) <- filesAndDirs contents
  return $ Node files $ fmap walkDir' dirs

tagDirectories :: [FilePath] -> IO [(FilePath, Bool)]
tagDirectories = mapM (\ x -> (x,) <$> doesDirectoryExist x)

filesAndDirs :: [FilePath] -> IO ([FilePath], [FilePath])
filesAndDirs c = bimap (fmap fst) . partition (not . snd) <$> tagDirectories c
  where bimap f (a, b) = (f a, f b)

exceptLocal :: [FilePath] -> [FilePath]
exceptLocal =
  filter notHidden
  where
    notHidden name | name `elem` [".", ".."] = False
                   | (head name) == '.' = False
                   | "bazel-" `isPrefixOf` name = False
                   | otherwise = True

printRTreeT :: Show a => RTreeT IO a -> IO ()
printRTreeT (Node a mts) = print a >> mapM_ (printRTreeT =<<) mts

main :: IO ()
main = do
  s <- walkDir' "."
  printRTreeT s
  return ()



