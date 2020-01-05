module AddOffsetCurrentTime (demo) where

import           Data.Time.Clock

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
  fmap (addUTCTime (offset * 24 * 3600)) $ getCurrentTime

demo :: IO ()
demo = do
  let dt = offsetCurrentTime 1231232
  return ()
