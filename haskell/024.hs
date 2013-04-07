----------------------------------------------------------------
-- Project Euler, Problem 024
-- Lexicographic permutations
-- Answer. 2783915460
----------------------------------------------------------------

module Problem024 where

import Data.List

fact n = if n == 1 then 1 else n * fact (n - 1)

factorials :: [Integer]
factorials = map fact [1..]


permLexList0 :: (([Integer], [Integer]),Integer) -> (([Integer], [Integer]), Integer)
permLexList0 pp  = let p = fst pp
                       val = snd pp
                       exp = fst p
                       res = snd p
                       e = last $ takeWhile (<= val) factorials
                       ls = takeWhile (<= val) [e*i | i <- [1..]]
                       nextval = val - (last ls)
                       num = length ls
                       nextnum = res !! num
                       nextexp = exp ++ [nextnum]
                       nextres = filter (\x -> x /= nextnum) res
                  in if val == 0 then ((exp++res, []), 0)
                     else ((nextexp, nextres), nextval)

permLexList :: [Integer] -> Integer -> [Integer]
permLexList xs n = let ys = iterate permLexList0 (([],xs),n)
                       zs = dropWhile (\pp -> not $ null $ snd $ fst pp) ys
                   in fst $ fst $ head zs

main = do
  print $ permLexList [0..9] 999999
