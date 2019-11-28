module NaiveCli.MyImplV1 (main) where

import qualified Data.Bool as DB
import qualified Data.Char as C
import qualified Options.Applicative as OA

main :: IO ()
main = runProgram =<< parseCLI

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    } deriving Show

parseCLI :: IO Options
parseCLI = OA.execParser $ withInfo parseOptions "Naive"
  where
    withInfo parser text = OA.info (OA.helper <*> parser) (OA.header text)

parseOptions :: OA.Parser Options
parseOptions = Options 
             <$> (OA.switch $ OA.long "capitalize")
             <*> (OA.switch $ OA.long "excited")
             <*> (OA.switch $ OA.long "stdin")
             <*> (OA.optional $ OA.strOption $ OA.long "file")

runProgram :: Options -> IO ()
runProgram o = print =<< (handlers o <$> getSource o)

handlers :: Options -> String -> String
handlers opts = handleCapitalize opts
              . handleExcited opts

handleCapitalize :: Options -> String -> String
handleCapitalize opts = DB.bool id (map C.toUpper) (oCapitalize opts)

handleExcited :: Options -> String -> String
handleExcited opts = DB.bool id ("|||| " ++) (oExcited opts)

getSource :: Options -> IO String
getSource opts = DB.bool (readFileOrDefault opts) readStdin (oStdIn opts)

readStdin :: IO String
readStdin = getContents

readFileOrDefault :: Options -> IO String
readFileOrDefault opts = case (oFileToRead opts) of
                            Just filename -> readFile filename
                            _ -> readDefault

readDefault :: IO String
readDefault = return "idnoclip"

