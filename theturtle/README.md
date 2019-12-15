# Write CLI program using the Turtle package

source:

<http://hackage.haskell.org/package/turtle-1.5.15/docs/Turtle-Tutorial.html>

## App/Echo

a small echo program `echo -n "asd" | stack run echo`

<http://hackage.haskell.org/package/turtle-1.5.15/docs/Turtle-Tutorial.html>

## App/Fs

Filesystem utlities

## How to evoke a json-returning program (like aws-cli) and parse the json blob

see: [./src/InputOutput/ExternalCmds.hs](./src/InputOutput/ExternalCmds.hs)

This is common theme in my daily work. There are two parts:

### How to evoke an external program and process its text output

#### option 1. use inshell (or other variants) + pure processing function

notice how the main function ( IO () ) use `sh runMain`:

> Run a Shell to completion, discarding any unused values

```haskell
process :: Line -> Line
consumer :: Shell Line -> ()
runMain :: Shell ()
main :: IO ()
main = sh runMain
```

#### option 2. use procStrict (or other variants)

procStrict provdes `Text` object. But Text data is not compatible
with Aeson

### How to decode Text in Aeson

source: <https://www.reddit.com/r/haskell/comments/5x4ufh/aeson_text_strict_lazy_bytestrings/>

require a conversion function; the rest is standard Aeson plumbing
