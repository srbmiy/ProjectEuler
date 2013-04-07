--------------------------------------------------------------------
-- Poject Euler, Problem 019 ans. 171
--------------------------------------------------------------------

module Problem019 where

import Data.List

type Year = Int
type Month = Int
type Day = Int

data Calender = Date {year:: Year, month:: Month, day:: Day} deriving (Show, Eq)
--instance Ord Calender where
--  Date y0 m0 d0 < Date y1 m1 d1
--    | y0 < y1 = True
--    | (y0 == y1)&&(m0 < m1) = True
--    | (m0 == m1)&&(d0 < d1) = True
--    | otherwise = False


data Week = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Show, Eq)

succWeek::Week -> Week
succWeek week = case week of Sun -> Mon
                             Mon -> Tue
                             Tue -> Wed
                             Wed -> Thu
                             Thu -> Fri
                             Fri -> Sat
                             Sat -> Sun

leapModifier:: Year -> Bool
-- If the year is a leap year then True else False
leapModifier y = (mod y 4 == 0)&&(mod y 100 /= 0) || (mod y 400 == 0)

makeMonth:: Year -> Month -> [Calender]
makeMonth y m = let days = \d -> Date y m d
                in if elem m [1,3,5,7,8,10,12] then map days [1..31]
                   else if elem m [4,6,9,11] then map days [1..30]
                        else if (m == 2)&&(leapModifier y) then map days [1..29]
                             else if m == 2 then map days [1..28]
                                                 else []

makeMonthsFrom:: Month -> Year -> [[Calender]]
makeMonthsFrom m y = map (makeMonth y) [m..12]

makeYearsFrom:: Month -> Year -> [[Calender]]
makeYearsFrom m y = concat $ (makeMonthsFrom m y) : map (makeMonthsFrom 1) [(y+1)..]


nextWeekAMonth:: (Week, Int) -> [Calender] -> (Week, Int)
-- Together count Sundays
nextWeekAMonth x cal = let theday = length cal
                           rem = mod theday 7
                           week = fst x
                           count = snd x
                           nextCount = if week == Sun then (count+1) else count
                           nextWeek = (iterate succWeek week) !! rem
                       in (nextWeek, nextCount)

solve:: Calender -> Week -> Calender -> (Week, Int)
solve cal0 week cal1 = let year0 = year cal0
                           mon0 = month cal0
                           cals = makeYearsFrom mon0 year0
                           fromcal0Tocal1 = takeWhile (\c -> head c /= cal1) cals
                       in foldl' nextWeekAMonth (week,0) fromcal0Tocal1

main = do
  let cal0 = Date 1901 1 1
  let cal1 = Date 2000 12 1
  let wk = Mon
  let x = solve cal0 wk cal1
  putStrLn $ (show cal1) ++ " is " ++ (show $ fst x)
  putStrLn $ "Sunday appears at first a month " ++ (show $ snd x) ++ " times."
