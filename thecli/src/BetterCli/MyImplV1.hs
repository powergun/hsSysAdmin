{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module BetterCli.MyImplV1
  ( main
  )
where

import           Data.Bifunctor                 ( first )
import           Data.Bool                      ( bool )
import           Data.Char                      ( toUpper )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , runReaderT
                                                , MonadIO
                                                , asks
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                , MonadError
                                                , throwError
                                                )
import           Control.Exception              ( IOException
                                                , try
                                                )
import           Control.Monad.Trans            ( liftIO )
import qualified Options.Applicative           as OA

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    }

type AppConfig = MonadReader Options -- Reader
data AppError = IOError String IOException
              deriving (Show) -- Error
-- App inherits the power from:
-- the usual suspects; reader; error; io
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
-- run = do
--   s <- getText
--   s <- handleCapitalize s
--   s <- handleExcited s
--   liftIO $ print s
run = liftIO . print =<< handleExcited =<< handleCapitalize =<< getText

getText :: App String
getText = bool getTextFromOptions getTextFromStdin =<< asks oStdIn

getTextFromStdin :: App String
getTextFromStdin = do
  -- do any logging; but a better approach is to use a separate
  -- WriterT layer for logging
  -- liftIO $ print "== read input from io =="
  liftIO getContents

getTextFromOptions :: App String
getTextFromOptions = maybe defaultText getTextFromFile =<< asks oFileToRead

defaultText :: App String
defaultText = return "default://"

getTextFromFile :: FilePath -> App String
getTextFromFile filename =
  either throwError return =<< first (IOError "readFileSafe") <$> liftIO
    (readFileSafe filename)

readFileSafe :: FilePath -> IO (Either IOException String)
readFileSafe = try . readFile

handleCapitalize :: AppConfig m => String -> m String
handleCapitalize s = bool s (map toUpper s) <$> asks oCapitalize

handleExcited :: AppConfig m => String -> m String
handleExcited s = bool s ("$!#@(&#!@)( " ++ s) <$> asks oExcited

parseCLI :: IO Options
parseCLI =
  OA.execParser $ OA.info (OA.helper <*> parseOptions) (OA.header "better cli")

parseOptions :: OA.Parser Options
parseOptions =
  Options
    <$> (OA.switch $ OA.long "capitalize")
    <*> (OA.switch $ OA.long "excited")
    <*> (OA.switch $ OA.long "stdin")
    <*> (OA.optional $ OA.strOption $ OA.long "file")
