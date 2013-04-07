-------------------------------------------------------------------------
-- Project Euler, Problem 029
-- Distinct Powers
-- answer. 
-------------------------------------------------------------------------

module Problem029 where

import Problem007

factorWith :: Int -> Int -> Int
factorWith n p = if (n `mod` p) /= 0 then 0
                 else 1 + (factorWith (n `div` p) p)

primesTo :: Int -> [Int]
primesTo n = takeWhile (< n) primes


main = do
  print 0