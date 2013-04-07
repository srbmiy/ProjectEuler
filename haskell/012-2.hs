-- Project Euler No.12 ans. 

fff x = filter (\i -> mod x i == 0) [1..x]
ggg x n i = if any (\j -> i==j*x) [1..(div n (length $ fff x))] then False else True 
hoge0 n xs = filter (ggg (head xs) n) xs
hoge 0 n xs = xs
hoge m n xs = hoge0 n (hoge (m-1) n xs)
foo n xs = if head xs == head (hoge0 n xs) then head xs else foo n (hoge0 n xs)

-- head $ foo 5 (map (\n -> div (n*(n+1)) 2) [1..])


 