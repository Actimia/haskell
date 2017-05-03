import Data.Function.Memoize
import Data.List
import Data.Ord

collatz' :: Int -> [Int]
collatz' 1 = [1]
collatz' n = n : (collatz $ next n)
    where
        next n
            | odd n = 3 * n + 1
            | otherwise = div n 2

collatz = memoize collatz'

p14 :: Int -> [Int]
p14 lim = maximumBy (comparing length) $ map collatz [1..lim]
