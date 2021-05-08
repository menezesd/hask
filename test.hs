threshold :: Int -> [Int] -> [Int]
threshold n xs = map (\x -> if x > n then x else 0) xs


prefixAdd :: [Int] -> [Int]
prefixAdd xs = foldr (\x xs -> x : map (+ x) xs) [] xs



prefixAddR :: [Int] -> [Int]
prefixAddR xs = aux 0 xs
   where
     aux n [] = []
     aux n (x:xs) = (n+x) : aux (n+x) xs


thresholdR :: Int -> [Int] -> [Int]
thresholdR n [] = []
thresholdR n (x:xs) = 
  if x > n then x : thresholdR n xs 
           else 0 : thresholdR n xs