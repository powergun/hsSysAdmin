{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module BetterCli.MyImplV2
  ( main
  )
where

import           Data.Bifunctor                 ( first )
import           Data.Char                      ( toUpper )
import           Data.Bool                      ( bool )
import           Options.Applicative
import           Control.Monad.Except
import           Control.Exception
import           Control.Monad.Reader

data Options = Options
  { oCapitalize :: Bool
  , oExcited    :: Bool
  , oStdIn      :: Bool
  , oFileToRead :: Maybe String
  }
data AppError = IOError String IOException deriving (Show)
type AppConfig = MonadReader Options
newtype App a = App {
  runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving ( Functor, Applicative, Monad
           , AppConfig
           , MonadError AppError
           , MonadIO
           )

main :: IO ()
main = runProgram =<< parseCLI

runProgram :: Options -> IO ()
runProgram opts =
  either renderError return =<< runExceptT (runReaderT (runApp run) opts)

renderError :: AppError -> IO ()
renderError (IOError msg e) = do
  print $ "An error has occurred at: " ++ msg
  print . show $ e

run :: App ()
run = liftIO . print =<< handleExcited =<< handleCapitalize =<< getText

handleExcited :: AppConfig m => String -> m String
handleExcited s = bool (return s) (return $ "$$$$$ " ++ s) =<< asks oExcited

handleCapitalize :: AppConfig m => String -> m String
handleCapitalize s =
  bool (return s) (return $ map toUpper s) =<< asks oCapitalize

getText :: App String
getText = bool getTextFromOptions getTextFromStdin =<< asks oStdIn

getTextFromStdin :: App String
getTextFromStdin = liftIO getContents

getTextFromOptions :: App String
getTextFromOptions = maybe defaultText getTextFromFile =<< asks oFileToRead

defaultText :: App String
defaultText = return "default://"

getTextFromFile :: FilePath -> App String
getTextFromFile filename = do
  r <- liftIO $ readFileSafe filename
  either throwError return r

readFileSafe :: FilePath -> IO (Either AppError String)
readFileSafe filename =
  first (IOError "readFile") <$> (try . readFile $ filename)

parseCLI :: IO Options
parseCLI = execParser (info (helper <*> parseOptions) (header "better"))

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch
          (long "capitalize" <> short 'c' <> help
            "to capitalize the input string"
          )
    <*> switch
          (long "excited" <> short 'e' <> help
            "to add a special prefix to the input string"
          )
    <*> switch (long "stdin" <> short 's' <> help "to take input from STDIN")
    <*> (optional $ strOption
          (  long "file"
          <> short 'f'
          <> help
               "to provide the input filename (this is discarded when --stdin is presented)"
          )
        )

