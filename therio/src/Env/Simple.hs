{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Env.Simple (demo) where

import           Prelude   (putStrLn)
import           RIO
import           System.IO (hPutStrLn, stderr)

demoPrintToDefault :: IO ()
demoPrintToDefault = do
  let code = "IDDQD"
      useCode = ask >>= liftIO . putStrLn
      useCodePlus = ask >>= liftIO . putStrLn . ("seta " ++ )
  runRIO code $ do
    useCode
    useCodePlus

data AppConf = AppConf
  { confName :: !String
  , confCode :: !String
  }

demoPrintToHandle :: IO ()
demoPrintToHandle = do
  let conf = AppConf "e1m1" "IDKFA"
      useCode = asks confName >>= liftIO . (hPutStrLn stderr)
      useCodePlus = asks confCode >>= liftIO . (hPutStrLn stderr) . ("seta " ++ )
  runRIO conf $ do
    useCode
    useCodePlus

demo :: IO ()
demo = do
  demoPrintToDefault
  demoPrintToHandle
