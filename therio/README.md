# Using the RIO package - the standard library

## fpcomplete RIO tutorial

source: <https://tech.fpcomplete.com/haskell/library/rio>

> Teams can easily spend significant time making basic architectural and library decisions on a new project. And for those relatively new to the Haskell ecosystem, it's all too easy to make choices with unknown costs, with those costs only showing themselves later.
> The overall goal of the documentation here is to help avoid these kinds of situations, by providing opinionated, well tested advice. The rio library is our best shot at codifying large parts of that advice.

### Logging and Beware of character encoding trap

In addition to `logInfo`, there are also `logDebug`, `logWarn`, `logError`, and others.

hPutStrLn blurs the line between machine-facing logging and human-facing logging;
for the latter, Utf8builder is always preferred;

without overloaded strings, logInfo is not compatible with String

```haskell
hs> import RIO
hs> runSimpleApp $ logInfo "Hello World!"

<interactive>:2:24: error:
    • Couldn't match expected type ‘Utf8Builder’
                  with actual type ‘[Char]’
    • In the first argument of ‘logInfo’, namely ‘"Hello World!"’
      In the second argument of ‘($)’, namely ‘logInfo "Hello World!"’
      In the expression: runSimpleApp $ logInfo "Hello World!"
hs>
```

log level control:

`RIO_VERBOSE=1 stack test`; otherwise it skips logDebug

### Use `Has*` Typeclass to model interface compatibility

it feels much more natural this way; it feels like Rust's trait requirement;

see: `src/Types/HasProperty.hs`; see also hsDesignPattern;

### Use Lens with `Has*` Typeclass to provide getter/setter mechanism

see `src/Types/HasPropertyLens.hs`; see also the exercise provided by
the tutorial: `src/Types/HelloALiceSmith.hs`

the pattern is `view <property> <value>` for getter;

`<new_value> = set <property> <property_value> <old_value>` for setter;

see the exericse for how to use `view`, `local` and `set` (and the super
compact `over`) to streamline the getting-setting process
