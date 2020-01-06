{-# LANGUAGE OverloadedStrings #-}

module SqliteSimple.OpenMemory (demo) where

import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data TestField = TestField Int String
                 deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

demo :: IO ()
demo = do
  conn <- open ":memory:"
  execute_ conn "CREATE TABLE test (id INTEGER PRIMARY KEY, str text)"
  execute conn "INSERT INTO test (str) VALUES (?)"
    (Only ("from memory" :: String))
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  close conn
