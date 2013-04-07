------------------------------------------------------------------------
-- Project Euler, Problem 003
-- Largest Prime Factor
-- answer. 6857
------------------------------------------------------------------------

module Problem003 where

import Data.List

isNotPrime:: Integer -> Bool
isNotPrime n = any (\i -> mod n i == 0) [2..m] where
            m = floor $ sqrt $ fromInteger n

reduceWith:: Integer -> Integer -> Integer
reduceWith num a = if (mod num a /= 0) then num
                   else let xs = iterate (\x -> div x a) num
                            ys = takeWhile (\n -> mod n a == 0) xs
                        in  div (last ys) a

lastReduction:: Integer -> Integer -> (Bool, Integer)
lastReduction num a = if (num /= 1) && (reduceWith num a == 1) then (True, a)
                        else (False, 0)

reduce:: Integer -> Integer -> Integer
reduce num p = let pair = lastReduction num p
                   isLast = fst pair
                   fact = snd pair
                   red = reduceWith num p
               in if isLast then fact else reduce red (p+1)

main = do
  -- let p = reduce 120 2
  let p = reduce 600851475143 2
  putStrLn $ (show p) ++ "is Prime?: " ++ (show . not . isNotPrime $ p)
  print p