import qualified InputOutput.ExternalCmds
import qualified InputOutput.Grep
import qualified Streams.Main
import qualified StrFormatting.TrivialFormat

main :: IO ()
main = do
  -- InputOutput.ExternalCmds.demoAwsCliCmd
  -- StrFormatting.TrivialFormat.demo
  -- Streams.Main.main

  InputOutput.Grep.demo
