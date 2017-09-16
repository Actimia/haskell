import Data.List

fibs = map fst $ (1,0): fibs' (1, 0)
    where 
        fibs' (prev, prevprev) = x: fibs' x where x = (prev+prevprev, prev)

indexedfibs = zip [1..] fibs

p25 = find pred indexedfibs
    where
        pred (_, value) = value > 10^999