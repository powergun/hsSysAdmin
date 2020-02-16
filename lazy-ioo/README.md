# Lazy IO

## Discover Lazy IO, High Perf Haskell P/215

P/215

> Lazy I/O allows pure functions to be interleaved with I/O actions
> arbitrarily

```haskell
main = do
  writeFile "file.txt" "old"
  old <- readFile "file.txt"
  writeFile "file.txt" "new" -- EXPLODE!!!
  putStrLn old -- readFile is evaluated because its result is used
```

> That would happen on the last line. But `readFile` does set a lock
> on the file in the Runtime System immediately, leading to `writeFile`
> failing.
