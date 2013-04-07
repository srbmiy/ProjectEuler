-------------------------------------------------------------
-- Project Euler, Problem 014
-- Longest Collatz sequence
-- answer. 837799
-------------------------------------------------------------

module Problem014 where

import Data.List

collatzLength :: Integer -> Int
collatzLength n = if n == 1 then 0
                  else if even n then (collatzLength (div n 2)) + 1
                       else (collatzLength (3*n + 1)) + 1

theMaximum :: (Integer -> Int) -> [Integer] -> (Integer, Int)
theMaximum f ls = let acc = (\x -> \y -> if snd x > snd y then x else y)
                      lst = map (\n -> (n, f n)) ls
                  in foldl' acc (1,1) lst

main = do
  let m = theMaximum collatzLength [1..1000000]
  print m
