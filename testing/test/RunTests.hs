
import qualified DemoTasty24Days
import qualified DemoTastyOfficial
import qualified DemoHSpec

main :: IO ()
main = do
  DemoHSpec.demo
  DemoTasty24Days.demo
  DemoTastyOfficial.demo