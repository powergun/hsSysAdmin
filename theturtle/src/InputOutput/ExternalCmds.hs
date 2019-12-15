{-# LANGUAGE OverloadedStrings #-}

module InputOutput.ExternalCmds
  ( demo
  , demoAwsCliCmd
  ) where

-- source
-- http://hackage.haskell.org/package/turtle-1.5.15/docs/Turtle-Tutorial.html

import           Control.Monad.Trans     (liftIO)
import           Data.Aeson
import           Data.ByteString.Builder (toLazyByteString)
import           Data.Text.Encoding      (encodeUtf8Builder)
import           Turtle

demo :: IO ()
demo = do
  -- inshell
  -- :: Text    -- Command line
  -- -> Shell Line  -- Standard input to feed to program
  -- -> Shell Line  -- Standard output produced by program
  -- inshell is vulnerable!!
  stdout (inshell "ls ./src" empty)
  --    ^stdout                ^stdin
  stdout (inshell "awk '{ print $1 }'" "123 456")

  let in' = inshell "ls" empty
      out' = inshell "md5sum" in'
  stdout out'

-- pure processor function
process :: Line -> Line
process = id

-- IO consumer function
consume :: Line -> IO ()
consume l = do
  print "// iddqd"
  print . lineToText $ l

-- option 1.
-- use the "Shell" construct
-- https://stackoverflow.com/questions/57584340/haskell-turtle-getting-text-output-from-inshellwitherr
-- NOTE! main() function must evoke this function with sh():
-- sh runAwsCli
runAwsCli :: Shell ()
runAwsCli = do
  out <- inshell "aws sts get-caller-identity" empty
  let out' = process out
  liftIO $ consume out
  return ()

-- option 2.
-- use procStrict() that returns IO (extcode, text)
-- NOTE! I need to use a conversion function to convert the return
-- text to an Aeson-friendly lazy bytestring, read:
-- https://www.reddit.com/r/haskell/comments/5x4ufh/aeson_text_strict_lazy_bytestrings/
-- (answer from baerion)
decode'' :: FromJSON a => Text -> Maybe a
decode'' = decode . toLazyByteString . encodeUtf8Builder

-- see: hsDataMunging/jsonmunging/Parsing/Applicative
data Credential = Credential { account :: String } deriving (Show)
instance FromJSON Credential where
  parseJSON = withObject "Credential" $ \o -> Credential <$> o .: "Account"

procAwsCli :: IO ()
procAwsCli = do
  (extcode, t) <- procStrict "aws" ["sts", "get-caller-identity"] empty
  print (decode'' t :: Maybe Value)
  print (decode'' t :: Maybe Credential)

demoAwsCliCmd :: IO ()
demoAwsCliCmd = procAwsCli
