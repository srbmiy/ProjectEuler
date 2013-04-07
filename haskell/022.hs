-------------------------------------------------------
-- Project Euler 022
-- Names scores
-- Answer. 871198282
-------------------------------------------------------

module Problem022 where

import Data.List
import Data.List.Split
import Data.Char
import System.IO

alphord :: Char -> Integer
alphord ch = (toInteger $ ord ch) - 96

scoreName :: String -> Integer
scoreName xs = sum $ map alphord xs

countScores :: (Integer, Integer) -> String -> (Integer, Integer)
countScores p xs = let resl = fst p
                       count = snd p
                       nextResl = resl + (count * scoreName xs)
                       nextCount = count + 1
                   in (nextResl, nextCount)


main = do
  xs <- readFile "names022.txt"
  let xxs = map toLower $ filter (\c -> c /= '"') xs
  let ys = endBy "," xxs
  let sorteds = sort ys
  print $ take 2 ys
  print $ scoreName $ head ys
  print $ foldl' countScores (0,1) sorteds