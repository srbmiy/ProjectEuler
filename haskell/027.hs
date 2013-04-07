------------------------------------------------------------------
-- Project Euler, Problem 027
-- Quadratic Primes
-- Answer. -59231
------------------------------------------------------------------

module Problem027 where

import Data.List (foldl')

primes = filter (\x -> all (\i -> x `mod` i /= 0) [2..(x-1)]) [2..1000]

primess = (map (\x -> -x) $ reverse primes) ++ primes

isPrime :: Integer -> Bool
isPrime n = 
  if n==0         then True
  else if n==1    then False
  else if n < 0   then isPrime (- n)
  else let m = floor $ sqrt $ fromIntegral n
       in all (\i -> n `mod` i /= 0) [2..m]

quadraticForm :: Integer -> Integer -> Integer -> Integer
quadraticForm a b n = n*n + a*n + b

consPrimes :: Integer -> Integer -> [Integer]
consPrimes a b = takeWhile isPrime $ map (quadraticForm a b) [0..]

maxLenPrimes :: Integer -> Integer -> Int
maxLenPrimes a b = length $ consPrimes a b

main = do
  print $ foldl' (\p0 -> \p1 -> if (snd p0 < snd p1) then p1 else p0) (0,0) [(a*b, (maxLenPrimes a b)) | a <- [-999..999], b <- primess ]