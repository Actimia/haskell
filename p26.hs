import Data.List
import Data.Maybe
import Data.Ord

digits 0 _ = []
digits p q = qr : digits (rem*10) q
    where qr@(_, rem) = quotRem p q 


cyclelen q = transform $ filter (isJust . snd) $ map func $ zip [1..] digits'
    where
        digits' :: [(Int, Int)]
        digits' = digits 1 q
        func :: (Int, (Int, Int)) -> (Int, Maybe Int)
        func (i, qr) = (i, elemIndex qr $ take (i-1) digits')
        transform :: [(Int, Maybe Int)] -> Int
        transform [] = 0
        transform ((a, b):_) = a - (fromJust b) - 1

p26 = maximumBy (comparing snd) $ zip [1..] $ map cyclelen $ [1..1000]