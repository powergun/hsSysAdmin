{-# LANGUAGE ScopedTypeVariables #-}

module FsUtils where

import Control.Monad
import Control.Exception (bracket, handle, SomeException)
import Data.Time.Clock
import System.Directory
import System.FilePath (takeExtension)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

-- using record syntax to give ourselves free accessor functions
-- such as infoPath
data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
-- WTF is maybe
-- is a function included in prelude
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:maybe
-- The maybe function takes a default value, a function, and a 
-- Maybe value. If the Maybe value is Nothing, the function returns 
-- the default value. Otherwise, it applies the function to the 
-- value inside the Just and returns the result.
-- NOTE: reminds me Python's default=value pattern

-- recall "searchable" is one field in the file permission structure
isDirectory = maybe False searchable . infoPerms
--            ----------------------   --------- evaluation order
-- this produces a new function
--                   defa  func ..
--                              ^^ Maybe value

-- P/268
-- a useful combination: maybeIO, which turns an IO action that 
-- might throw an exception into one that wraps its result in 
-- Maybe
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle 
  (\(e :: SomeException) -> return Nothing) 
  (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  -- recall that bracket will propagate the exception
  -- using bracket is to ensure that the resource is released
  size <- maybeIO (bracket (openFile path ReadMode) (hClose) (hFileSize))
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)