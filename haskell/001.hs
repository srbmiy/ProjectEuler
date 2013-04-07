--------------------------------------------
-- Project Euler, Problem 001
-- Summing up divisor of 3, 5
-- answer: 234168
--------------------------------------------

module Problem001 where

import Data.List

acc :: Int -> Int-> Int
acc n x = if (mod x 3 == 0 || mod x 5 == 0) then n+x else n

main:: IO()
main = do
       print $ foldl' acc 0 [0..1000]
