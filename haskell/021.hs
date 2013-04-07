-----------------------------------------------------------------
-- Project Euler, Problem 021
-- Amicable numbers
-- Answer. 31626
-----------------------------------------------------------------

module Problem021 where

import Data.List

d n = sum $ filter (\i ->  n `mod` i == 0) [1..(n-1)]

haveAmicablePair x = (d x /= x) && ((d $ d x) == x)

amicables m = filter haveAmicablePair [1..m]

main = do
  print $ sum $ amicables 10000
