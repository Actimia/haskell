anyT :: [Bool] -> Bool
anyT = foldl (||) False

isPrime :: Int -> Bool
isPrime x = not $ anyT $ map (\y -> mod x y == 0) [2..(floor $ sqrt $ fromIntegral x)]

p10 x = sum $ filter isPrime [2..x]
