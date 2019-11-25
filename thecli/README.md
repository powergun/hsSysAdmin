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
