{-# LANGUAGE OverloadedStrings #-}

{-
see what the pragma is needed:
https://stackoverflow.com/questions/37894987/couldnt-match-expected-type-text-with-actual-type-char
-}

module Main (main) where

import Turtle

{-
Most of the commands in this library do not actually invoke an
external shell or program. Instead, they indirectly wrap other
Haskell libraries that bind to C code.
-}
main = do
  let cmd = "false"
  x <- shell cmd empty
  case x of
    ExitSuccess   -> return ()
    ExitFailure n -> die (cmd <> " failed with exit code: " <> repr n)


