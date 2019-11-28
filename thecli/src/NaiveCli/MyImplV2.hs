module NaiveCli.MyImplV2 (main) where

import           Control.Exception   (IOException, try)
import           Data.Bifunctor      (first)
import           Data.Bool           (bool)
import           Data.Char           (toUpper)
import           Options.Applicative (Parser, execParser, header, helper, info,
                                      long, optional, strOption, switch)

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
handlers opts = do
  -- use <$> below because getText' returns IO (Either String String)
  -- instead of Either String String;
  -- for the latter case I can simply use application operator $ or paren
  -- in this case I can use a separate step to extract the inner value
  -- or use applicative op
  t <- either id id <$> getText' opts
  let t' = ( handleCapitalize opts
           . handleExcited opts ) t
  return t'

handleCapitalize :: Options -> String -> String
handleCapitalize opts = bool id (map toUpper) (oCapitalize opts)

handleExcited :: Options -> String -> String
handleExcited opts = bool id ("!!!! " ++) (oExcited opts)

getText :: Options -> IO String
getText opts = bool (readFileOrDefault opts) readStdin (oStdIn opts)

getText' :: Options -> IO (Either String String)
getText' opts = bool (maybe doDefault doReadFile fnMaybe) doReadStdin (oStdIn opts)
  where
    fnMaybe :: Maybe FilePath
    fnMaybe = oFileToRead opts
    doDefault :: IO (Either String String)
    doDefault = return $ Right "/usr/bin/env perl"
    doReadFile :: FilePath -> IO (Either String String)
    doReadFile fn = do
      r <- readFileSafe' fn
      return $ first show r
    doReadStdin :: IO (Either String String)
    doReadStdin = do
      r <- readStdin
      return $ Right r

readStdin :: IO String
readStdin = getContents

readFileOrDefault :: Options -> IO String
readFileOrDefault opts =
  case (oFileToRead opts) of
    Just filename -> readFileSafe filename
    _             -> return "there is default"

readFileSafe' :: FilePath -> IO (Either IOException String)
readFileSafe' = try . readFile

readFileSafe :: FilePath -> IO String
readFileSafe filename = do
  let read' :: IO (Either IOException String)
      read' = try . readFile $ filename
  ret <- read'
  case ret of
    Left exc -> do
      print $ show exc
      return "there is default"
    Right t -> return t

parseCLI :: IO Options
parseCLI = execParser (info (helper <*> parseOptions) (header "IDDQD"))

parseOptions :: Parser Options
parseOptions = Options
             <$> (switch $ long "capitalize")
             <*> (switch $ long "excited")
             <*> (switch $ long "stdin")
             <*> (optional $ strOption $ long "file")
