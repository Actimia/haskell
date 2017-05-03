d n = sum $ divisors n

divisors n = filter (isDivisor n) [1..n-1]

isDivisor a b = mod a b == 0


amicable n = let b = d n in n /= b && n == d b


p21 lim = sum $ filter amicable [2..lim]
