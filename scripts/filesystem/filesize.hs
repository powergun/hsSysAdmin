-- see Find / BetterPredicate.hs for the safer implementation

-- this impl is based on :
-- System.IO hFileSize, which returns the size in bytes of 
-- an open file
-- real world haskell P/259

-- this function throws an exception if an entry is not a plain 
-- file or could not be opened (perhaps due to insufficient 
-- permissions) 

-- why a safer version matters?
-- see P/260 for the reasoning
-- I had seen how file-handle exhaustion caused hard to debug issue 
-- in the past....
