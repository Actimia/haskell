import Data.List

triangle x = sum [1..x]

primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps

divisors' x = filter isdiv [1..x]
    where
        isdiv n = mod x n == 0

divisors n = product $ map ((+1) . length) (group $ primeFactors n)

p12 lim = head $ dropWhile pred $ map triangle [1..]
    where
        pred x = divisors x < lim
