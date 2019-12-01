module Main (main) where

import qualified InputOutput.StdinStdout
import qualified InputOutput.RWFiles
import qualified InputOutput.ExternalCmds

{-
to test:

echo -n "asd" | stack run io
-}
main :: IO ()
main = do
  InputOutput.StdinStdout.demo
  InputOutput.RWFiles.demo
  InputOutput.ExternalCmds.demo
