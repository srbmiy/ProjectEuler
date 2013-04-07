------------------------------------------------------------------------
-- Project Euler, Problem 007  ans. 104743
------------------------------------------------------------------------

module Problem007 where

-- (foo n) is the n+1-st prime number in the natural order
foo :: Int -> Int
foo n = primes !! n

-- (primes) is an infinitude of prime numbers
primes :: [Int]
primes = filter ptest [2..]

-- ptest is a criterion whether an integer is prime or not
ptest :: Int -> Bool
ptest x = let m = div x 2
          in if any (\i -> mod x i == 0) [2..m] then False
             else True
