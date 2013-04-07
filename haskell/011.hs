----------------------------------------------------------------
-- Project Euler, Problem 011 
----------------------------------------------------------------

module Problem011 where

import IO
import Data.Char
import Data.List

fromDecToInt:: String -> Int
fromDecToInt ds = let fig = length ds
                      pro = (\f -> \d -> d * (10^f))
                      is = map digitToInt ds
                  in zipWith pro (reverse [0..fig]) is

