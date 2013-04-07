-- Project Euler No.12 ans. 


hoge2 n m 0 = if and [(length (trifact m) <= n), (length (trifact (2*m)) > n)] then m
              else hoge2 n (2*m) 0 
hoge2 n m k = if and [(length (trifact ((hoge2 n m (k-1)) + m)) <= n), (length (trifact ((hoge2 n m (k-1)) + 2*m)) > n)] then (hoge2 n m (k-1)) + m
              else hoge2 n (2*m) k

-- factors of the first number of trifacts > n
foo :: Int -> [Int]
foo n = head $ filter (\s -> length s > n) $ map trifact [1..]

-- factors of triangle numbers
trifact :: Int -> [Int]
trifact n = fact $ div (n*(n+1)) 2

-- factors of the number n
fact :: Int -> [Int]
fact n = filter (\x -> mod n x == 0) [1..n]

