--------------------------------------------------------------------------
-- Project Euler, Problem 006 ans. 25164150
--------------------------------------------------------------------------

module Problem006 where

main = do
  let n = 100
  let m0 = (div (n*(n+1)) 2)^2
  let m1 = div (n*(n+1)*(2*n + 1)) 6
  print (m0 - m1)