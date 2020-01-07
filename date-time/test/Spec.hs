
import qualified AddOffsetCurrentTime
import qualified GetCurrentTimeRIO
import qualified TestAgeRestriction

main :: IO ()
main = do
  TestAgeRestriction.demo

  AddOffsetCurrentTime.demo
  GetCurrentTimeRIO.demo
