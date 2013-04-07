------------------------------------------------------------------
-- Project Euler, Problem 025
-- 1000-digit Fibonacci number
-- Answer. 4782
------------------------------------------------------------------

module Problem025 where

import Data.List

fibgen :: [Integer] -> [Integer]
fibgen xs = let next = (xs !! 0) + (xs !! 1)
            in next : xs

fibs :: [Integer]
fibs = head $ dropWhile (\xs -> (length $ show $ head xs) < 1000) $ (iterate fibgen [1,1])

--------------------
-- alternative
--------------------


main = do
  print $ length fibs