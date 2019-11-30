{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
Experiment with adding another layer for logging
This compiles but I must change the implementation to account for
addition lifting...
-}

module BetterCli.MyExperiment (main) where

import qualified Control.Exception    as E
import           Control.Monad.Except
import           Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Bifunctor       as BF
import qualified Data.Bool            as B
import qualified Data.Char            as C
import           Options.Applicative

-- types

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    }

type AppConfig = MonadReader Options
data AppError = IOError E.IOException
type Logger = MonadWriter [String]

newtype App a = App {
    runApp :: WriterT [String] (ReaderT Options (ExceptT AppError IO)) a
} deriving ( Monad, Functor, Applicative  -- usual suspects
           , AppConfig  -- Reader
           , Logger 
           , MonadIO  -- IO
           , MonadError AppError  -- Except
           )

-- program

main :: IO ()
main = return ()
