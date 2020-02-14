# All about testing in Haskell

## How to run single test target on stack

`stack test <local package>:<target>`; example: hsPerf/monitoring

## Testing

source:

[Monday morning haskell](https://mmhaskell.com/testing-1)

the official example works - but I must make the dependencies are set up correctly

<http://hackage.haskell.org/package/tasty>

this also provides a working example of a tasty test suite:

<https://ocharles.org.uk/posts/2013-12-03-24-days-of-hackage-tasty.html>

## Tasty

is a framework that wraps HUnit, QuickCheck and SmallCheck;

is preferred by some (see 24 days haskell)

`package.yaml`

dependency:

```text
HUnit, tasty, tasty-hunit, tasty-smallcheck, tasty-quickcheck
```

see `test/DemoHUnit.hs`

### Tasty & QuickCheck

inspired by a discussion in the fp school channel;

problem: given a custom data structure (commonly a tree-like structure
that is recursive), how to randomly generate instances of such data
for testing?

see: src/QuickCheckRandGen.hs for the tree-like data structure, the
test subject;

see: test/DemoQuickCheckRandGen.hs for the tasty-quickcheck setup,
including:

- implement an instance of QuickCheck.Arbitray for the tree type
- create property-based test function
- print the random tree values for a proof of concept
- do the rest of the plumping work and create the main() function

source: <https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators>

## HSpec

see `test/DemoHSpec.hs` for how to define context - this can improve
test clarity

### Extend Hspec

#### Implement new predicate `shouldXX`

source: <https://alpmestan.com/posts/2014-06-18-testing-attoparsec-parsers-with-hspec.html>

MY NOTE: I always wonder how one can work efficiently with a seemingly
tiny collection of predicate expressions Hspec offers out-of-box. There
is another package `hspec-expectations` that contain more predicate;
search on hoogle for `should`

MY NOTE: while writing this, I was doing push-ups for hsInterview/da
therefore I implemented two mini examples representing the real-world
situation:

- readMaybe based safe-fail scenario: signal failure using `Nothing`;
  the test case use `shouldBe Nothing` to verify the sad-path

- readEither based safe-fail-with-detail scenario: signal failure using
  error types; each type can optionally contain details about the failure;
  the test case can either use `shouldBe <error value ctor>` to verify
  the exact cause of failure; or implement a helper function that converts
  the error value to a short token/string then use string-comparison
