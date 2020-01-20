{-# LANGUAGE NoImplicitPrelude #-}

module Types.HasPropertyLens (demo) where

import           RIO
import           System.IO (hPutStrLn, stdout)

data App = App
  { appName   :: !String
  , appHandle :: !Handle
  }

class HasHandle env where
  handleL :: Lens' env Handle

instance HasHandle Handle where
  handleL = id

instance HasHandle App where
  handleL = lens appHandle (\x y -> x { appHandle = y })

info :: (HasHandle a) => a -> String -> IO ()
info x s = do
  let h = view handleL x
  hPutStrLn h s

demo :: IO ()
demo = do
  let hdl = stdout
      app = App "foobar" stdout
      app' = set handleL stderr app
  info hdl "from naked handle"
  info app $ (appName app) ++ ": from app handle"
  info app' $ (appName app) ++ ": from app (stderr) handle"
