
import qualified QuickCheckBasics
import qualified QuickCheckRandGen
import qualified Variant

main :: IO ()
main = do
  QuickCheckBasics.runTests
  QuickCheckRandGen.runTests
  Variant.main
