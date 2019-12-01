module Filesystem.DateTime (demo) where

{-
source:
http://hackage.haskell.org/package/turtle-1.5.15/docs/Turtle-Tutorial.html

Note, the result is in UTC
-}

import Turtle

demo :: IO ()
demo = print =<< datefile =<< pwd
