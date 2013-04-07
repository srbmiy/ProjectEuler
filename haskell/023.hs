------------------------------------------------------------
-- Project Euler, Probrem 023
-- No-abundant sums
-- Aanswer.
------------------------------------------------------------

module Problem023 where

import Data.List

d :: Integer -> Integer
d n = let xs = filter (\x -> n `mod` x == 0) [1..(n-1)]
      in sum xs

isPrime n = (d n == 1)

isAbundant :: Integer -> Bool
isAbundant n = if (even n) && (isAbundant $ div n 2) then True
               else (n < d n)

abundants :: [Integer]
abundants = filter isAbundant [1..28123]

sumOfAbundants :: [Integer]
sumOfAbundants = takeWhile (<= 28123) $ sort $ [x + y | x <- abundants, y <- abundants ]

nonAbundantSums :: Integer
nonAbundantSums = (28124*28123 `div` 2) - sum sumOfAbundants

main = do
  print $ nonAbundantSums