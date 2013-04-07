--------------------------------------------------------------------------
-- Project Euler, Problem 017 ans. 21124
--------------------------------------------------------------------------

module Problem017 where

import Data.Char
import Data.List


toDecDigitZero:: Char -> String
toDecDigitZero ch = case ch of '0' -> ""
                               '1' -> "one"
                               '2' -> "two"
                               '3' -> "three"
                               '4' -> "four"
                               '5' -> "five"
                               '6' -> "six"
                               '7' -> "seven"
                               '8' -> "eight"
                               '9' -> "nine"
                               _   -> "NaN"

toDecDigitTeen:: Char -> String
toDecDigitTeen ch = case ch of '0' -> "ten"
                               '1' -> "eleven"
                               '2' -> "twelve"
                               '3' -> "thirteen"
                               '4' -> "fourteen"
                               '5' -> "fifteen"
                               '6' -> "sixteen"
                               '7' -> "seventeen"
                               '8' -> "eighteen"
                               '9' -> "nineteen"

toDecDigitTen:: Char -> String
toDecDigitTen ch = case ch of '0' -> ""
                              '1' -> ""
                              '2' -> "twenty"
                              '3' -> "thirty"
                              '4' -> "forty"
                              '5' -> "fifty"
                              '6' -> "sixty"
                              '7' -> "seventy"
                              '8' -> "eighty"
                              '9' -> "ninety"
                             

toDecEn:: Int -> String
toDecEn n = let ns = show n
                ms = (take (3-(length ns)) $ repeat '0') ++ ns
                h = ms !! 0
                t = ms !! 1
                z = ms !! 2
            in if (t == '0')&&(z == '0') then (toDecDigitZero h) ++ "hundred"
               else if (h == '0')&&(t == '1') then toDecDigitTeen z
               else if (h /= '0')&&(t == '1') then (toDecDigitZero h) ++ "hundredand" ++ (toDecDigitTeen z)
               else if (h == '0') then (toDecDigitTen t) ++ (toDecDigitZero z)
               else (toDecDigitZero h) ++ "hundredand" ++ (toDecDigitTen t) ++ (toDecDigitZero z)

main = do
  let xs = (map toDecEn [1..999]) ++ ["onethousand"]
  let num = foldl' (\n -> \s -> n + (length s)) 0 xs
  -- mapM_ putStrLn xs -- test o.k.
  print num