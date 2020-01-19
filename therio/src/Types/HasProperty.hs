module Types.HasProperty (demo) where

import           Data.Bool (bool)
import           System.IO (hPutStrLn, stdout)

type Logger = IO String
type Context = [String]

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogger  :: !Logger
  , appContext :: !Context
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

data Debugger = Debugger
  { dbgLogger  :: !Logger
  , dbgContext :: !Context
  }

class HasLogger a where
  logFunc :: a -> Logger

class HasContext a where
  processContext :: a -> Context

instance HasLogger App where
  logFunc = appLogger
instance HasLogger Debugger where
  logFunc = dbgLogger
instance HasContext App where
  processContext = appContext
instance HasContext Debugger where
  processContext = dbgContext

info :: (HasLogger a, HasContext a) => a -> IO ()
info x = do
  s <- logFunc x
  let c = processContext x
  hPutStrLn stdout $ s ++ (unwords c)

critical :: (HasLogger a, HasContext a) => a -> IO ()
critical x = do
  s <- logFunc x
  let c = processContext x
  hPutStrLn stdout $ "CRITICAL: " ++ s ++ (unwords c)

demo :: IO ()
demo = do
  let app = App { appLogger = (return "in app!")
                , appContext = ["<app>"]
                , appOptions = Options True
                }
      debugger = Debugger { dbgLogger = (return "in debugger...")
                          , dbgContext = ["debugger..."]
                          }
  bool (info app) (critical app) (optionsVerbose . appOptions $ app)
  info debugger

