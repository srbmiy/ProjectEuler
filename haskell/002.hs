--------------------------------------------------------------------
-- Project Euler, Problem 002, ans. 4613732
--------------------------------------------------------------------

module Problem002 where

import Data.List

fibss:: [[Int]]
-- inversed-fibs
fibss = iterate fibgen [2,1] where
  fibgen xs = let x0 = xs !! 0
                  x1 = xs !! 1
                  x2 = x0 + x1
              in x2:xs

main = do
       let xs = last (takeWhile (\xs -> head xs < 4000000) fibss)
           ys = filter (\n -> mod n 2 == 0) xs
       print $ sum ys
