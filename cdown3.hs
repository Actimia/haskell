import Data.Maybe
import Data.List
import Control.Monad

data Node = Add Node Node | Sub Node Node | Mul Node Node | Div Node Node | Term Int deriving (Show)

eval :: Node -> Maybe Int
eval (Term i) = Just i
eval (Add a b) = add' (eval a) (eval b)
    where
        add' (Just a) (Just b) = Just (a + b)
        add' _ _ = Nothing
eval (Sub a b) = sub' (eval a) (eval b)
    where
        sub' (Just a) (Just b)
            | a > b = Just (a - b)
            | otherwise = Nothing
        sub' _ _ = Nothing
eval (Mul a b) = mul' (eval a) (eval b)
    where
        mul' (Just a) (Just b) = Just (a * b)
        mul' _ _ = Nothing
eval (Div a b) = div' (eval a) (eval b)
    where
        div' (Just a) (Just b)
            | mod a b == 0 = Just (div a b)
            | otherwise = Nothing
        div' _ _ = Nothing


app :: [a -> b -> c] -> a -> b -> [c]
app [] _ _  = []
app (f:fs) a b = (f a b):(app fs a b)

makeTrees :: [Int] -> [Node]
makeTrees [] = []
makeTrees (a:[]) = [Term a]
makeTrees (a:b:[]) = app [Add, Sub, Div, Mul] (Term a) (Term b)
makeTrees xs = concat $ map makeTrees' $ divisions xs
    where makeTrees' (r, l) = app [Add, Sub, Div, Mul] (makeTrees r) (makeTrees l)

divisions xs = map divisions' [1..(length xs) - 1]
    where divisions' n = (take n xs, drop n xs)


allTrees :: [Int] -> [Node]
allTrees xs = concat $ map makeTrees $ permutations xs

solve :: [Int] -> Int -> [Node]
solve nums target = filter pred $ allTrees nums
    where pred x = case eval x of Just a -> a == target
                                  Nothing -> False
