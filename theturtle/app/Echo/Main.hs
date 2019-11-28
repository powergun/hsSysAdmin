{-# LANGUAGE OverloadedStrings #-}

import qualified Turtle as TU
import qualified Data.Text.IO as TIO

{-
I'm stuck on this for a while:
why `stack run echo` does not process contents read from stdin but 
instead is stalled?

do this:
echo -n "asd" | stack run echo

test program with using Turtle library
main = getContents >>= print

the version below uses TIO hence requiring additional convertion
to Turtle's Line type;
-}
main :: IO ()
main = do
  t <- TIO.getContents
  case (TU.textToLine t) of
    Just l -> TU.echo l
    _ -> return ()
  