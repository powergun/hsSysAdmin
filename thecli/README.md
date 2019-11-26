# The Cli Stack

source:

https://thoughtbot.com/blog/refactoring-to-a-monad-transformer-stack

## use `stack run` properly

had some issue with `stack run` before... writting this to remind
me how to do it properly;

in `package.yaml`, the `executables` group contains all the exe
targets, `foo`, `naive` etc...

run `stack run <target name>` to execute these targets;

command args are passed behind `<target name>`

example: `stack run foo`, this should print out iddqd

## The naive cli

the initial version of the cli program, exhibiting an anit-pattern
of passing around options, doing excessive exception-handling etc

## The better cli

the final version of the cli program, use monad reader transformer
to share the option (the "environment"); use ExceptT;

the resulting framework supports robust exception handling and extendability
