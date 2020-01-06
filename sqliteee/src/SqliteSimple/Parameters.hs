{-# LANGUAGE OverloadedStrings #-}

module SqliteSimple.Parameters (demo) where

import           Database.SQLite.Simple
import           SqliteSimple.Utils

demo :: IO ()
demo = do
  conn <- open ":memory:"
  execute_ conn "CREATE TABLE testparams (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "CREATE TABLE testparams2 (id INTEGER, t TEXT, t2 TEXT)"
  [Only i] <- query conn "SELECT ?" (Only (42 :: Int))  :: IO [Only Int]
  assertEqual "select int param" 42 i
  execute conn "INSERT INTO testparams (t) VALUES (?)" (Only ("test string" :: String))
  rows <- query conn "SELECT t FROM testparams WHERE id = ?" (Only (1 :: Int)) :: IO [Only String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (Only "test string") (head rows)
  execute_ conn "INSERT INTO testparams (t) VALUES ('test2')"
  [Only row] <- query conn "SELECT t FROM testparams WHERE id = ?" (Only (1 :: Int)) :: IO [Only String]
  assertEqual "select params" "test string" row
  [Only row] <- query conn "SELECT t FROM testparams WHERE id = ?" (Only (2 :: Int)) :: IO [Only String]
  assertEqual "select params" "test2" row
  [Only r1, Only r2] <- query conn "SELECT t FROM testparams WHERE (id = ? OR id = ?)" (1 :: Int, 2 :: Int) :: IO [Only String]
  assertEqual "select params" "test string" r1
  assertEqual "select params" "test2" r2
  [Only i] <- query conn "SELECT ?+?" [42 :: Int, 1 :: Int] :: IO [Only Int]
  assertEqual "select int param" 43 i
  [Only d] <- query conn "SELECT ?" [2.0 :: Double] :: IO [Only Double]
  assertEqual "select double param" 2.0 d
  [Only f] <- query conn "SELECT ?" [4.0 :: Float] :: IO [Only Float]
  assertEqual "select double param" 4.0 f
  close conn
