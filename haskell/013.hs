------------------------------------------------------------
-- Project Euler, Problem 013 ans. 5537376230
------------------------------------------------------------

module Problem013 where

import Data.Char
import Data.List
import System.IO

pickFrom:: [[a]] -> Int -> [a]
pickFrom yss n = map (\xs -> (xs !! n)) yss


main = do
  xs <- readFile "file013"
  let xss = lines xs
      lh = (length $ head xss) -1
      yss = map (pickFrom xss) [0..lh]
      iss = map (\xs -> map (toInteger . digitToInt) xs) yss
      is = map sum $ take 13 iss
      s = foldl' (\x -> \b -> (x*10)+b ) 0 is
      ps = take 10 $ show s
  print is
  putStrLn ps
  