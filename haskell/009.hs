-------------------------------------------------------------
-- Project Euler, Problem 009 ans.[200, 375, 425], 31875000
-------------------------------------------------------------

module Problem009 where

import Data.Char
import Data.List

foo :: Int -> [Int]
ffoth :: Int -> [[Int]]
ttrd :: Int -> [Int] -> Bool
ssnd :: Int -> [[Int]]

-- the first Pythagoranian triple a,b,c s.t. a+b+c==n
foo n = head $ ffoth n

-- Pythagoranian triples a,b,c s.t. a+b+c==n
ffoth n = filter (ttrd n) $ ssnd n

-- Pythagoranian test
ttrd n [i, j, k] = if i*i + j*j == k*k then True else False

-- collecting triples a,b,c s.t. a+b+c==n
ssnd n = [[i, j, k] | i<-[1..n], j<-[1..n], k<-[1..n], (i<=j && j<=k && i + j + k == n) ]


main = do
       let triple = foo 1000
       print $ triple
       print $ (triple !! 0)*(triple !! 1)*(triple !! 2)
