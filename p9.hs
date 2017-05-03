tuples :: [(Int, Int)]
tuples = [(x, y) | x <- [1..1000], y <- [1..1000]]

pyth (a, b) = let c = floor $ sqrt $ fromIntegral (a*a + b*b) in
    if a*a + b*b == c*c then Just (a,b,c) else Nothing

p9 = filter pred $ map pyth tuples
    where pred (Just (a,b,c)) = a+b+c == 1000
          pred Nothing = False
