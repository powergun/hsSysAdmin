{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module BetterCli.MyImplV1 (main) where

import Data.Bool (bool)
import Data.Char (toUpper)
import Control.Monad.Reader ( MonadReader, ReaderT, runReaderT
                            , MonadIO, asks
                            )
import Control.Monad.Except (ExceptT, runExceptT, MonadError)
import Control.Exception (IOException)
import Control.Monad.Trans (liftIO)
import qualified Options.Applicative as OA

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    }

type AppConfig = MonadReader Options -- Reader
data AppError = IOError IOException
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
runProgram opts = either (print . show) return 
                =<< runExceptT (runReaderT (runApp run) opts)

run :: App ()
run = do
  s <- getText
  s <- handleCapitalize s
  s <- handleExcited s
  liftIO $ print s

getText :: App String
getText = return "setmap q3dm3"

handleCapitalize :: AppConfig m => String -> m String
handleCapitalize s = bool s (map toUpper s) <$> asks oCapitalize

handleExcited :: AppConfig m => String -> m String
handleExcited s = bool s ("$!#@(&#!@)( " ++ s) <$> asks oExcited

parseCLI :: IO Options
parseCLI = OA.execParser
         $ OA.info (OA.helper <*> parseOptions) (OA.header "better cli")

parseOptions :: OA.Parser Options
parseOptions = Options
             <$> (OA.switch $ OA.long "capitalize")
             <*> (OA.switch $ OA.long "excited")
             <*> (OA.switch $ OA.long "stdin")
             <*> (OA.optional $ OA.strOption $ OA.long "file")