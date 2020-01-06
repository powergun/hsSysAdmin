module SqliteSimple.Utils where

import           Data.Bool (bool)

assertEqual :: (Eq a, Eq a) => String -> a -> a -> IO ()
assertEqual label x y = bool (error "") (return ()) (x == y)
