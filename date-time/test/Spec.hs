
import qualified AddOffsetCurrentTime
import qualified TestAgeRestriction

main :: IO ()
main = do
  TestAgeRestriction.demo

  AddOffsetCurrentTime.demo
