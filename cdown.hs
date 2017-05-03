import System.Random
import Data.Maybe
import Data.List

small = take (n*maxnum) $ cycle [1..maxnum]
    where
        n = 2
        maxnum = 10
big = [25, 50, 75, 100]


numbers n = take n small ++ take (total - n) big
    where total = 6

doSolve :: [Int] -> Int -> [Maybe ([Int], String)]
doSolve nums target = filter isJust $ map solve' $ permutations nums
    where solve' perm@(n:ns) = fmap (\solution -> (perm, solution)) $ solve target n ns []

solve :: Int -> Int -> [Int] -> [Char] -> Maybe [Char]
solve target cur [] ops
    | target == cur = Just ops
    | otherwise = Nothing
solve target cur (x:xs) ops
    | target == cur = Just ops
    | otherwise = listToMaybe . catMaybes $ map solve' (possible cur x)
        where
            solve' :: (Int, Char) -> Maybe [Char]
            solve' (cand, op) = solve target cand xs (op:ops)

possible :: Int -> Int -> [(Int, Char)]
possible a b = catMaybes [add' a b, sub' a b, mul' a b, div' a b]
    where
        add' a b = Just (a + b, '+')
        sub' a b
            | a > b = Just (a - b, '-')
            | otherwise = Nothing
        -- dont do mul/div with 1?
        mul' a b = Just (a * b, '*')
        div' a b
            | mod a b == 0 = Just (div a b, '/')
            | otherwise = Nothing
