import Data.List

divisors n = filter (isDivisor n) [1..n `div` 2 + 1]

divisors' n = (1:) $ nub $ concat [ [x, div n x] | x <- [2..limit], rem n x == 0 ]
     where limit = (floor.sqrt.fromIntegral) n

isDivisor a b = mod a b == 0

abundant n = n < (sum $ divisors' n)

abundants = filter abundant [1..28123]
