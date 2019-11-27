
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
https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:either
Case analysis for the Either type. If the value is Left a, apply the
first function to a; if it is Right b, apply the second function to b.
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
https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:maybe
The maybe function takes a default value, a function, and a Maybe value.
If the Maybe value is Nothing, the function returns the default value.
Otherwise, it applies the function to the value inside the Just and
returns the result.
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

{-
Either.try:
https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#v:try

Similar to catch, but returns an Either result which is (Right a)
if no exception of type e was raised, or (Left ex) if an exception
of type e was raised and its value is ex. If any other type of
exception is raised than it will be propogated up to the next
enclosing exception handler.
-}
safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile
