{-# LANGUAGE OverloadedStrings #-}

module InputOutput.ExternalCmds (demo) where

-- source
-- http://hackage.haskell.org/package/turtle-1.5.15/docs/Turtle-Tutorial.html

import Turtle

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
