---------------------------------------------------------------
-- Project Euler, Problem 10 ans. 142913828922
---------------------------------------------------------------

module Problem010 where

import Data.List

foo:: Integer -> Integer
primeSum:: Integer -> Integer -> Integer
isNotPrime:: Integer -> Bool

foo n = foldl' primeSum 0 [2..n]

primeSum a p = if isNotPrime p then a else a + p

isNotPrime n = any (\i -> mod n i == 0) [2..m] where
            m = floor $ sqrt $ fromInteger n

main = do 
       print $ foo 10