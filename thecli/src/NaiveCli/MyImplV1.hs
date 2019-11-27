module NaiveCli.MyImplV1 where

import qualified Options.Applicative as OA

{-
name is a value-argument that can not be omitted;
if a strOption is not optional, it must be provided by the command line:

Missing: --name ARG

Usage: <interactive> [--active] --name ARG [--code ARG]
-}
data Options = Options
  { active :: Bool
  , name   :: String
  , code   :: Maybe String
  }

parseOptions :: OA.Parser Options
parseOptions = Options
  <$> (OA.switch $ OA.long "active")
  <*> (OA.strOption $ OA.long "name")
  <*> (OA.optional $ OA.strOption $ OA.long "code")

{-
to test in ghci
-}
parseCLI :: IO Options
parseCLI = OA.execParser parserInfo
  where
    parserInfo :: OA.ParserInfo Options
    parserInfo = OA.info (OA.helper <*> parseOptions) (OA.header "idkfa")

parseStrings :: [String] -> OA.ParserResult Options
parseStrings args = OA.execParserPure OA.defaultPrefs parserInfo args
  where
    parserInfo :: OA.ParserInfo Options
    parserInfo = OA.info (OA.helper <*> parseOptions) (OA.header "idkfa")

demo :: IO ()
demo = do
  print "iddqd"
