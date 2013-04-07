----------------------------------------------------------------------
-- Project Euler, Problem 020
----------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Problem020 where

import Data.Char


type Digits = [Int]
type Size = Int

class Stackable s a where
  init :: s a
  isNull :: s a -> Bool
  pop :: s a -> Maybe a
  push :: a -> s a -> s a
  delete :: s a -> s a


data Stack a = NULL | Stack { stkhead :: a,
                              stktail :: Stack a}

instance (Eq a) => Eq (Stack a) where
  NULL == NULL               = True
  NULL == Stack h t          = False
  Stack h t == NULL          = False
  Stack h1 t1 == Stack h2 t2 = (h1 == h2)&&(t1 == t2)

instance (Show a) => Show (Stack a) where
  show NULL = "NULL"
  show (Stack h t) = (show h) ++ ":" ++ (show t)


instance (Eq a) => Stackable Stack a where
  init = NULL
  isNull stk = (stk == NULL)
  pop stk = case stk of
    NULL -> Nothing
    _    -> Just (stkhead stk)
  push x stk = Stack x stk
  delete stk = case stk of
    NULL -> NULL
    _    -> stktail stk


-----------------------------------------------------------------------
-----------------------------------------------------------------------



class StCalc s b where
  initial :: b -> s b
  getStat :: s b -> b
  getValue :: s b -> b
  calculate :: s b -> (b -> b -> s b) -> s b
  setStat :: s b -> b -> s b

data StD a = StD {state:: a, calc :: a}

instance StCalc StD Digits where
  initial x = StD [] x
  getStat (StD s xs) = s
  getValue (StD s xs) = xs
  calculate (StD s xs) f = f s xs
  setStat (StD s xs) t = StD t xs

intToMyDigits:: Int -> Digits
intToMyDigits n = map digitToInt $ reverse $ show n

myDigitsToInt:: Digits -> Int
myDigitsToInt xs = foldl atoi 0 (reverse xs) where
  atoi x b = (x*10)+b


overflow:: Size -> Digits -> Int -> Digits
-- overflow is the overflowed-value of mul*ns
overflow f ns mul = let n = myDigitsToInt ns
                        val = div (n*mul) (10^f)
                    in  intToMyDigits val


  
genSeqPower0:: Size -> (StD Digits)  -> Digits -> Int -> Int -> Digits
genSeqPower0 f ns xs mul n
  -- xs is the original input, ns is the overflowed data
  | n == 0           = overflow f [(head xs)] mul
  | n < (length xs)  = let ns = (take f) . (drop (maximum [0, (n-f)])) $ xs
                           ms = overflow f ns mul
                       in ms
  | otherwise        = []

{-
genSeqPower:: [Int] -> [Int]
genSeqPower xs = let justyss = map (genSeqPower0 xs) [0..]
                     justys = takeWhile (\y -> y /= Nothing) justyss
                     ys = map (maybe 0 id) justys
                 in ys
-}
