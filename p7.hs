
anyT :: [Bool] -> Bool
anyT = foldl (||) False

isPrime :: Int -> (Int, Bool)
isPrime x = (x, not $ anyT $ map (\y -> mod x y == 0) [2..(floor $ sqrt $ fromIntegral x)])


--primes :: [Int]
primes = filter pred $ map isPrime [2..]
    where pred (num, prime) = prime
