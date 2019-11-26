
module NaiveCli.App (main) where

import qualified Control.Exception   as E
import qualified Data.Bifunctor      as BF
import qualified Data.Bool           as B
import qualified Data.Char           as C

-- require: optparse-applicative
import           Options.Applicative

{-
builtin help message generator:


stack run naive -- --help

File Fun

Usage: naive [--capitalize] [--excited] [--stdin] [--file ARG]

Available options:
  -h,--help                Show this help text
-}

-- types

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    } deriving Show

-- program

main :: IO ()
main = runProgram =<< parseCLI

runProgram :: Options -> IO ()
runProgram o =
    putStr =<< ( handleExcitedness o
               . handleCapitalization o <$> getSource o)

-- data retrieval and transformation

{-
either :: (a -> c) -> (b -> c) -> Either a b -> c ???
-}
getSource :: Options -> IO String
getSource o = B.bool
              -- false: load from file or default content
              (either id id <$> loadContents o)
              -- true: load from stdin
              getContents
              $ oStdIn o

handleCapitalization :: Options -> String -> String
handleCapitalization o = B.bool
                         -- false: return input as it is
                         id
                         -- true: character-wise capitalization
                         (map C.toUpper)
                         $ oCapitalize o

handleExcitedness :: Options -> String -> String
handleExcitedness o = B.bool
                      -- false: return input as it is
                      id
                      -- true: modify the input
                      (":LOL " ++)
                      $ oExcited o

defaultContent :: String
defaultContent = "there is a cow\n"

{-
MY NOTE:
maybe :: b -> (a -> b) -> Maybe a -> b ??
-}
loadContents :: Options -> IO (Either String String)
loadContents o =
    maybe defaultResponse readFileFromOptions $ oFileToRead o
  where
    readFileFromOptions f = BF.first show <$> safeReadFile f
    defaultResponse = return $ Right defaultContent

-- CLI parsing

{-
MY NOTE:
test in ghci (make sure Options type is showable)
:l NaiveCli.App
parseCLI
-}
parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "File Fun")
  where
    withInfo :: Parser Options -> String -> ParserInfo Options
    withInfo opts h = info (helper <*> opts) $ header h

{-
MY NOTE:
see http://hackage.haskell.org/package/optparse-applicative-0.15.1.0/docs/Options-Applicative.html
for `switch`, `long` and their usage pattern
-}
parseOptions :: Parser Options
parseOptions = Options
    <$> (switch $ long "capitalize")
    <*> (switch $ long "excited")
    <*> (switch $ long "stdin")
    <*> (optional $ strOption $ long "file")

-- safer reading of files

safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile
