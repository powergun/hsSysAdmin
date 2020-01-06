{-# LANGUAGE OverloadedStrings #-}

module SqliteSimple.SelectMemory (demo) where

import           Database.SQLite.Simple
import           SqliteSimple.Utils

demo :: IO ()
demo = do
  conn <- open ":memory:"
  execute_ conn "CREATE TABLE test1 (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO test1 (t) VALUES ('test string')"
  rows <- query_ conn "SELECT t FROM test1" :: IO [Only String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (Only "test string") (head rows)
  rows <- query_ conn "SELECT id,t FROM test1" :: IO [(Int, String)]
  assertEqual "int,string" (1, "test string") (head rows)
  -- Add another row
  execute_ conn "INSERT INTO test1 (t) VALUES ('test string 2')"
  rows <- query_ conn "SELECT id,t FROM test1" :: IO [(Int, String)]
  assertEqual "row count" 2 (length rows)
  assertEqual "int,string" (1, "test string") (rows !! 0)
  assertEqual "int,string" (2, "test string 2") (rows !! 1)
  [Only r] <- query_ conn "SELECT NULL" :: IO [Only (Maybe Int)]
  assertEqual "nulls" Nothing r
  [Only r] <- query_ conn "SELECT 1" :: IO [Only (Maybe Int)]
  assertEqual "nulls" (Just 1) r
  [Only r] <- query_ conn "SELECT 1.0" :: IO [Only Double]
  assertEqual "doubles" 1.0 r
  [Only r] <- query_ conn "SELECT 1.0" :: IO [Only Float]
  assertEqual "floats" 1.0 r
  close conn
