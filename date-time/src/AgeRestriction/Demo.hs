module AgeRestriction.Demo where

import           Data.Time.Calendar
import           Data.Time.Clock

ageRestriction = 20

canIRegister :: Integer -> Int -> Int -> IO Bool
canIRegister year month day = do
  today <- (getCurrentTime >>= return . utctDay)
  case fromGregorianValid year month day of
    Nothing  -> return False
    Just dob -> return $ (addGregorianYearsClip ageRestriction dob) `isBefore` today
  where
    isBefore lhs rhs = diffDays lhs rhs <= 0
