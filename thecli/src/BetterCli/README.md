# Path of Evolution

## sketch out the cli program skeleton

the purpose of `main`, `runProgram`, `parseCLI`

design the cli interface in the `Options` type

```haskell
module BetterCli.MyImplV1 (main) where

import Data.Char (toUpper)

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    }

main :: IO ()
main = runProgram =<< parseCLI

runProgram :: Options -> IO ()
runProgram opts = do
  s <- getText
  s <- handleCapitalize s
  print s

getText :: IO String
getText = return "setmap q3dm3"

handleCapitalize :: String -> IO String
handleCapitalize s = return $ map toUpper s

parseCLI :: IO Options
parseCLI = return $ Options True True False Nothing
```

## develop monad transformer stack

define the role on each layer of the stack:

reader layer; exception layer; io layer

then develop the "role-carrier" for each layer:

reader layer to use MonadReader, which is played by AppConfig;

exception layer to use ExceptT, wrapped by AppError

finally, the `App` stack

note how the `deriving` statement mirrors the above decisions

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BetterCli.MyImplV1 (main) where

import Data.Char (toUpper)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, MonadIO)
import Control.Monad.Except (ExceptT, runExceptT, MonadError)
import Control.Exception (IOException)
import Control.Monad.Trans (liftIO)

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    }

type AppConfig = MonadReader Options -- Reader
data AppError = IOError IOException deriving (Show) -- Error
newtype App a = App {
  runApp :: ReaderT Options (ExceptT AppError IO) a
  -- App inherits the power from:
  -- the usual suspects; reader; error; io
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
  liftIO $ do
    s <- getText
    s <- handleCapitalize s
    print s

getText :: IO String
getText = return "setmap q3dm3"

handleCapitalize :: String -> IO String
handleCapitalize s = return $ map toUpper s

parseCLI :: IO Options
parseCLI = return $ Options True True False Nothing
```

## implement `parseCLI` (boilerplate code)

parseCLI (based on `Options.Applicative` package) is more or less
boilerplate code

but luckily this package reduce amount of boilerplate code I have
 to write

```haskell
parseCLI :: IO Options
parseCLI = OA.execParser $ OA.info (OA.helper <*> parseOptions) (OA.header "better cli")

parseOptions :: OA.Parser Options
parseOptions = Options
             <$> (OA.switch $ OA.long "capitalize")
             <*> (OA.switch $ OA.long "excited")
             <*> (OA.switch $ OA.long "stdin")
             <*> (OA.optional $ OA.strOption $ OA.long "file")
```

## develop handler functions to utilise the monad transformer stack

there are a few changes in the design:

handler function runs in the Reader context - this can be observed at:

- the function type is `AppConfig m => String -> m String`, meaning
within the function body, it has access to the AppConfig data, aka
`Options`
- the impl calls `asks oCapitalize` to retrieve the boolean variable
parsed from command line, however in order to satisfy the return
type, I have to use fmap to extract the actual boolean, because `asks`
returns a `ReaderT r m a`

the data provider function `getText` also runs in the Reader context,
I use `return` to inject the raw string into the context

the `run` function no longer needs to `lift` every statement;
it only `liftIO` the last print statement.

```haskell
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
  liftIO $ print s

getText :: App String
getText = return "setmap q3dm3"

handleCapitalize :: AppConfig m => String -> m String
handleCapitalize s = bool s (map toUpper s) <$> asks oCapitalize

parseCLI :: IO Options
parseCLI = OA.execParser $ OA.info (OA.helper <*> parseOptions) (OA.header "better cli")

parseOptions :: OA.Parser Options
parseOptions = Options
             <$> (OA.switch $ OA.long "capitalize")
             <*> (OA.switch $ OA.long "excited")
             <*> (OA.switch $ OA.long "stdin")
             <*> (OA.optional $ OA.strOption $ OA.long "file")
```
