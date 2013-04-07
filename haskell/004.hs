-------------------------------------------------------------------------
-- Project Euler, Problem004 ans. 906609
-------------------------------------------------------------------------

module Problem004 where

isPalindromic:: Integer -> Bool
isPalindromic num = (show num) == (reverse $ show num)

prodList::[Integer]
-- a list of products of 3-digit numbers.
prodList = [ i*j | i <- [100..999], j <- [100..999] ]

main = do
  let pals = filter isPalindromic prodList
      p = maximum pals
  print p
