---------------------------------------------------------------------------
-- Project Euler, Problem 008 ans. 40824 
---------------------------------------------------------------------------

module Problem008 where

-- max of foo1
foo2 :: Int -> Int
foo2 n = maximum (foo1 n)

-- products of five consecutive numbers in the list
foo1 :: Int -> [Int]
foo1 n = map (hoge (llist n)) [0..((ln n)-5)]

-- Integer to list of digits
llist :: Int -> [Int]
llist n = if n < 10 then [n] else (llist (div n 10)) ++ [mod n 10]

-- length
ln :: Int -> Int
ln n = length (llist n)

-- product of five consecutinves in the n-th list
hoge :: [Int] -> Int -> Int
hoge ns n = (ns !! n)*(ns !! n+1)*(ns !! n+2)*(ns !! n+3)*(ns !! n+4)