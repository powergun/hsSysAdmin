{-# LANGUAGE NoImplicitPrelude #-}

module GetCurrentTimeRIO (demo) where

import           Prelude  (putStrLn)
import           RIO
import           RIO.Time (getCurrentTime)

demo :: IO ()
demo = runRIO () $ do
  t <- getCurrentTime
  liftIO . putStrLn . show $ t
