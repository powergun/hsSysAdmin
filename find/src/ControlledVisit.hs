module ControlledVisit
  ( traverss
  ) where

import Control.Monad
import System.FilePath ((</>))

import FsUtils

-- real world haskell P/267

--              sort func 
traverss :: ([Info] -> [Info]) -> FilePath
         -> IO [Info]

traverss order path = do
  names <- getUsefulContents path
  -- map (path </> names) to get the abs paths; in Python I'd use 
  -- list comprehension
  -- P/267 read this line right to left
  -- see function/curry for the different currying methods for
  -- binary operators (</> in this case); the syntax a op .. works 
  -- in pointless mode
  contents <- mapM getInfo (path : map (path </>) names)
  -- read from right to left
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
    then traverss order (infoPath info)
    else return [info]
