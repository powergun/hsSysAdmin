
import qualified DemoHSpec
import qualified DemoTasty24Days
import qualified DemoTastyOfficial
import qualified ExtendHspec.Demo

main :: IO ()
main = do
  -- DemoHSpec.demo
  -- DemoTasty24Days.demo
  -- DemoTastyOfficial.demo
  ExtendHspec.Demo.demo
