------------------------------------------------------------------
-- Project Euler, Problem 026
-- Repiprocal cycles
-- Answer. 983
------------------------------------------------------------------


module Problem026 where

import Data.List (foldl')

{- Point.
A number 1/d has minimum e of length of the repiprocal cycle is the first number e such that (10^e - 1)/d is integral or of form y/2^p5^q.
-}

seqof9 :: [Integer]
seqof9 = [(10^n - 1) | n <- [1..]]

repiprocal :: Integer -> (Integer, Int)
repiprocal x = let l = length $ takeWhile (\n -> n `mod` x /= 0) seqof9
               in (x, l)

primes :: [Integer]
primes = filter (\x -> all (\i -> x `mod` i /= 0) [2..(x-1)]) [2..]

primeswithout25 :: [Integer]
primeswithout25 = takeWhile (< 1000) $ filter (\n -> (n /= 2 && n /= 5)) primes

main = do
  let cyclens = map repiprocal primeswithout25
  let maxnum = foldl' (\p0 -> \p1 -> if (snd $ p1) > (snd $ p0) then p1 else p0) (3,1) cyclens
  print $ maxnum