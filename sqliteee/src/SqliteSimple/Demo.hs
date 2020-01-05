{-# LANGUAGE OverloadedStrings #-}
module SqliteSimple.Demo (demo) where

import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

-- create db:
-- sqlite3 /var/tmp/test.db "CREATE TABLE test (id INTEGER PRIMARY KEY, str text);INSERT INTO test (str) VALUES ('test string');"

demo :: IO ()
demo = do
  conn <- open "/var/tmp/test.db"
  execute conn "INSERT INTO test (str) VALUES (?)"
    (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  close conn
