--------------------------------------------------------------------------
-- Project Euler, Problem005 ans. 232792560
--------------------------------------------------------------------------

module Problem005 where

import Data.List

divFilter:: [Integer] -> Integer -> [Integer]
divFilter xs n = filter (\x -> mod x n == 0) xs

divs:: Integer -> [Integer]
divs n = divFilter [1..] n

prodList:: [Integer]
prodList = [i0*i1*i2*i3*i4*i5*i6*i7 | i0<- divs 2, i1<- divs 3, i2<- divs 5, i3<- divs 7, i4<- divs 11, i5<- divs 13, i6<- divs 17, i7<- divs 19 ]

fltredList n = foldl' divFilter prodList [1..n]

main = do
  putStrLn $ "val=20 -> " ++ (show $ head $ fltredList 20)