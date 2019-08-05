#!/usr/bin/env stack runghc
import           System.Process

-- redo video series
-- in the video createProcess is called along with shell() function

main :: IO ()
main = do
  createProcess $ shell "echo 'there is cow' >/var/tmp/sut/dd"
  return ()
