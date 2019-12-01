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

## develop `getText` function using the new exception-handling mechanism

it all starts from `readFileSafe` which encapsulates the exception
in an `Either` value;

the `Either` value is temporarily "unpacked" in order for `throwError`
to work (and to stop the monad bind chain gracefully);

the effect is ultimately bubbled up to the top-most level as another
`Either` value - that encapsulates my own error type `AppError` - to
be printed out to stderr

### getText: use App to encapsulate Options-passing and IO

```haskell
getText :: App String
getText = bool getTextFromOptions (liftIO getContents) =<< asks oStdIn

getTextFromOptions :: App String
getTextFromOptions = return "file://not-implemented"
```

`getTextFromOptions` is to further extract the filename argument
from Options; for the moment it returns a hardcoded string

Note, there is no longer exception handling logic on this level
(no `either`) -- this is a major benefit of using stack

### implement `getTextFromOptions`

follow the `App String` pattern (meaning the given function runs
inside the App context and returns a String value), expand the
logic of getTextFromOptions to

- getTextFromFile
- defaultText

```haskell
getTextFromOptions :: App String
getTextFromOptions = maybe defaultText getTextFromFile =<< asks oFileToRead

defaultText :: App String
defaultText = return "default://"

getTextFromFile :: FilePath -> App String
getTextFromFile filename = return "file://asdasd"
```

Note, this is based on the `maybe` machinary, that calls a function
returning hardcoded value in the case of `Nothing` or calls a function
that takes the `Just` value;

Also note, **I'm not concerned of exception handling at this stage**

`defaultText` does not need to wrap the value in `Right`, since
any result at any level, when not coming from throwError, is
considered successful

### implement `getTextFromFile` with exception handling

in addition to use `App` to encapsulate Options-passing and IO,
it also wraps up failure, previously managed by `Either`

```haskell
getTextFromFile :: FilePath -> App String
getTextFromFile filename =
  either throwError return =<< first IOError <$> liftIO (readFileSafe filename)

readFileSafe :: FilePath -> IO (Either IOException String)
readFileSafe = try . readFile
```

when `readFileSafe` encounters an IOException, we use AppError's
IOError data ctor to bubble this exception up.

Note, throwError does not take the raw exception value IOException;

## top-level logic

`run` function can be written in a fancy bind form:

```haskell
run = liftIO . print =<< handleExcited =<< handleCapitalize =<< getText
```

compose `liftIO . print` is to lift IO operation up to the App monad,
so that it because a single function runs in the App context, taking
the value returned by the handlers
