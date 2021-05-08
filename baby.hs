double x = x + x
mysum x y = double x + double y
foo x = if x > 100 then x else x*2

uppers st = [c | c <- st, c `elem` ['A'..'Z']]

factors n = [x | x <- [1..n], n `mod` x == 0]

prime n = factors n == [1,n]

primes n = [x | x <- [2..n], prime x]

perfects n = [x | x <- [2..n], sum (factors x) == 2*x]

replicate n a = [a | _ <- [1..n]]

pyths n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a*a + b*b == c*c]

