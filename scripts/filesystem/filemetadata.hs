#!/usr/bin/env stack runghc

-- real world haskell P/257
-- use System.Posix and System.Win32 to retrieve platform specific
-- file metadata

import System.Directory

demoGetPermissions :: IO ()
demoGetPermissions = do
  print "//// demo getPermissions()"
  getPermissions "/var/tmp" >>= print
  getPermissions "/dev" >>= print
  isWritable "/dev" >>= print
  isWritable "." >>= print
  where
    isWritable :: FilePath -> IO Bool
    isWritable filename = do
      perm <- getPermissions filename
      -- P/257 
      -- see how to "extract" the perm field from the value returned 
      -- by getPermissions() 
      -- writable :: Permissions -> Bool
      return $ writable perm

demoGetModificationTime :: IO ()
demoGetModificationTime = do
  -- returns UTCTime
  getModificationTime "/var/tmp" >>= print

-- demoGetFileSize
-- see Find / BetterPredicate.hs
-- note that this function returns Maybe Integer

main :: IO ()
main = do
  demoGetPermissions
  demoGetModificationTime
