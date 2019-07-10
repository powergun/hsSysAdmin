{-# LANGUAGE ScopedTypeVariables #-}

module BetterPredicate
  ( betterFind
  ) where

import Control.Monad (filterM)
import System.Directory 
  (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

-- P/258 the predicate is pure and can not perform IO
-- Predicate type is just a synonym for a function of four arguments
-- ! not a data with data ctor and value ctor
type Predicate =  FilePath
               -> Permissions
               -> Maybe Integer
               -> UTCTime
               -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize p = do
  handle
    (\(e :: SomeException) -> return Nothing) $
    bracket (openFile p ReadMode) (hClose) $ \h -> do
      size <- hFileSize h
      return (Just size)
  
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    -- P/259
    -- check is an IO-capable wrapper for our pure predicate p,
    -- it does all the dirty work of IO on p's behalf so that
    -- we can keep p incapable of unwanted side effects
    -- ! this is a great idiom
    -- after gathering the metadata, check calls p and then 
    -- uses return to wrap p's result with IO
    check name = do
      perms <- getPermissions name
      size <- getFileSize name -- returns (Maybe Integer)
      modified <- getModificationTime name -- returns UTCTime
      return (p name perms size modified)

-- real world haskell P/262
-- sometimes this kind of library is referred to as an embedded
-- domain-specific language: we use our programming language's 
-- native facilities (hence embedded) to write code that lets 
-- use solve some narrow problem (hence domain-specific) particularly
-- elegantly

