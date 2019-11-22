# All about testing in Haskell

source:

[Monday morning haskell](https://mmhaskell.com/testing-1)

the official example works - but I must make the dependencies are set up correctly

http://hackage.haskell.org/package/tasty

this also provides a working example of a tasty test suite:

https://ocharles.org.uk/posts/2013-12-03-24-days-of-hackage-tasty.html

## HUnit

`package.yaml`

dependency:

```text
HUnit, tasty, tasty-hunit, tasty-smallcheck, tasty-quickcheck
```

see `test/DemoHUnit.hs`
