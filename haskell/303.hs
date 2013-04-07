-- Project Euler No.303

dem x = sum $ map (\n -> div (f n) n) [1..x]

f n = head $ filter (\n -> all (\i -> i<=2) (llist n)) $ g n

g n = filter (\i -> mod i n == 0) [1..]

llist :: Int -> [Int]
llist n = if n < 10 then [n] else (llist (div n 10)) ++ [mod n 10]



