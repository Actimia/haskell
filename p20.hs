import Data.Char

fact n = foldl (*) 1 [2..n]

p20 = sum $ map digitToInt $ show $ fact 100
