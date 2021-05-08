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

dot xs ys = [a*b | (a,b) <- zip xs ys]

factorial n = product [1..n]

factorialR 0 = 1
factorialR (n) = (n) * factorialR (n-1)

merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x == y   = x : y : merge xs ys 
                    | x < y    = x : merge xs (y:ys)
                    |otherwise = y : merge ys (x:xs)
					
halve :: [a] -> ([a],[a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst (halve xs))) (msort (snd (halve xs)))

-- dec2int [2,3,4,5] = 2345
-- [Int] --> Int
dec2intR xs = sum [ w * b | (w,b) <- zip weights (reverse xs)] where weights = iterate (*10) 1

dec2int xs = foldl (\x y -> 10 * x + y) 0 xs

unfold p h t x | p x = []
               |otherwise h x : unfold p h t (t x)