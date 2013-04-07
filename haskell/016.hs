--------------------------------------------------------------------
-- Project Euler, Problem 016 ans. 1366
--------------------------------------------------------------------

module Problem016 where

import Data.Char
import Data.List

generate:: Int -> Int -> Int
generate n0 n1 = if n0 >= 5 then 1 + (mod (2*n1) 10)
                                 else mod (2*n1) 10

genSeqPower0:: [Int] -> Int -> Maybe Int
genSeqPower0 xs n
  | n == 0           = Just $ generate 0 (head xs)
  | n == (length xs) = if last xs <= 4 then Nothing
                                              else Just 1
  | n < (length xs)  = let n0 = xs !! (n-1)
                           n1 = xs !! n
                       in Just $ generate n0 n1
  | otherwise        = Nothing

genSeqPower:: [Int] -> [Int]
genSeqPower xs = let justyss = map (genSeqPower0 xs) [0..]
                     justys = takeWhile (\y -> y /= Nothing) justyss
                     ys = map (maybe 0 id) justys
                 in ys

seqPower::Int -> [Int]
-- 2^n with digits
seqPower n = foldl' (\xs -> \i -> genSeqPower xs) [1] [1..n]

main = do
  let theSeq = seqPower 1000
  putStrLn $ "2^1000= " ++ (show theSeq)
  putStrLn $ "Sum of the digits of 2^1000= " ++ (show $ sum theSeq)
