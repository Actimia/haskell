import Data.Maybe
import Data.List


possible :: [(Int, String)] -> [(Int, String)]
possible [a,b] = catMaybes [add' a b, sub' a b, mul' a b, div' a b, rem1 a b, rem2 a b]
    where
        add' (a,asol) (b, bsol) = Just (a + b, "(" ++ asol ++ " + " ++ bsol ++ ")")
        sub' (a,asol) (b, bsol)
            | a > b = Just (a - b, "(" ++ asol ++ " - " ++ bsol ++ ")")
            | otherwise = Nothing

        mul' a (1,_) = Just a
        mul' (1,_) b = Just b
        mul' (a,asol) (b, bsol) = Just (a * b, "(" ++ asol ++ " * " ++ bsol ++ ")")

        div' a (1,_) = Just a
        div' (1,_) b = Just b
        div' (a,asol) (b, bsol)
            | mod a b == 0 = Just (div a b, "(" ++ asol ++ " / " ++ bsol ++ ")")
            | otherwise = Nothing
        rem1 a _ = Just a
        rem2 _ b = Just b


ppSolve xs x = mapM_ putStrLn $ nub $ doSolve xs x

doSolve :: [Int] -> Int -> [String]
doSolve xs target = solve target [(x, show x) | x <- reverse $ sort xs]

solve :: Int -> [(Int, String)] -> [String]
solve target [(x, solution)]
    | x == target = [solution]
    | otherwise = []
solve target xs@((x,solution):_)
    | x == target = [solution]
    | otherwise = concat $ map (solve target) $ gen xs

gen :: [(Int, String)] -> [[(Int, String)]]
-- gen xs = [poss : (take n xs ++ drop (n+2) xs)
--            | n <- [0..(length xs-2)],
--              poss <- possible (take 2 $ drop n xs)]

--gen xs = map (\x -> possible x : (xs \\ x)) $ combinations 2 xs
gen xs = concat [[poss : (xs \\ x) | poss <- possible x] | x <- combinations 2 xs]

pick n k = div (fact n) ((fact k) * (fact (n-k)))
    where
        fact 0 = 1
        fact n = product [1..n]


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']
