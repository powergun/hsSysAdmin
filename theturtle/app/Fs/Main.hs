module Main (main) where

import qualified Filesystem.DateTime
import qualified Filesystem.Stream

main :: IO ()
main = do
  Filesystem.DateTime.demo
  Filesystem.Stream.demo

