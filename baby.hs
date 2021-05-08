double x = x + x
mysum x y = double x + double y
foo x = if x > 100 then x else x*2

uppers st = [c | c <- st, c `elem` ['A'..'Z']]