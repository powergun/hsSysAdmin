#!/usr/bin/env stack runghc

-- real world haskell P/256
import System.FilePath

-- takeExtension ""
-- ""

-- takeExtension "asd"
-- ""

-- take extension "asd/as/bsd.a.b.c.sd"
-- "sd"

demoReplaceExt :: IO ()
demoReplaceExt = do
  -- replaceExtension is purely a FilePath (string) op
  print $ replaceExtension "/var/tmp/sut/bb.cow" ".1337"

main :: IO ()
main = do
  demoReplaceExt

