module NaiveCli.MyImplV3 (main) where

import Data.Bifunctor (first)
import Control.Exception (IOException, try)
import Data.Bool (bool)
import Data.Char (toUpper)
import Options.Applicative (Parser, switch, strOption, optional, long,
  execParser, helper, header, info)

main :: IO ()
main = runProgram =<< parseCLI

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    } deriving Show

runProgram :: Options -> IO ()
runProgram opts = print =<< handlers opts

handlers :: Options -> IO String
handlers opts = (return . handleCapitalize opts
              . handleExcited opts)
              =<< getText opts

handleExcited :: Options -> String -> String
handleExcited opts = bool id ("!@#$!$ " ++) (oExcited opts)

handleCapitalize :: Options -> String -> String
handleCapitalize opts = bool id (map toUpper) (oCapitalize opts)

parseCLI :: IO Options
parseCLI = execParser (info (helper <*> parseOptions) (header "nospoon"))

parseOptions :: Parser Options
parseOptions = Options
             <$> (switch $ long "capitalize")
             <*> (switch $ long "excited")
             <*> (switch $ long "stdin")
             <*> (optional $ strOption $ long "file")

getText :: Options -> IO String
getText opts = bool (getTextFromFile opts) getContents (oStdIn opts)

getTextFromFile :: Options -> IO String
getTextFromFile opts = maybe (return "file://") return fnm
  where
    fnm :: Maybe String
    fnm = oFileToRead opts

defaultText = "iddqd"

readFileSafe :: FilePath -> IO (Either String String)
readFileSafe filename = do
  let doRead :: FilePath -> IO (Either IOException String)
      doRead = try . readFile
  ret <- doRead filename
  return $ first show ret