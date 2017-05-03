
import Data.Maybe
import Data.List

fact n = product [2..n]

data RPN = Add | Sub | Mul | Div | Term Int deriving (Show)

evalS :: String -> Maybe Int
evalS = eval . parse

parse :: String -> [RPN]
parse = parse' . words
    where
        parse' :: [String] -> [RPN]
        parse' [] = []
        parse' ("+":rest) = Add:(parse' rest)
        parse' ("-":rest) = Sub:(parse' rest)
        parse' ("*":rest) = Mul:(parse' rest)
        parse' ("/":rest) = Div:(parse' rest)
        parse' (int:rest) = (Term (read int)):(parse' rest)


eval :: [RPN] -> Maybe Int
eval expr = eval' expr []
    where
        eval' :: [RPN] -> [Int] -> Maybe Int
        -- No ops left, either just a value or error
        eval' [] []                     = Nothing
        eval' [] (x:[])                 = Just x
        eval' [] (x:stack)              = Nothing

        -- ops require atleast 2 arguments on the stack
        eval' (Add:rest) (a:b:stack)    = eval' rest (a+b:stack)
        eval' (Add:_)    (_:[])         = Nothing
        eval' (Add:_)    ([])           = Nothing

        eval' (Sub:rest) (b:a:stack)
            | a > b                     = eval' rest (a - b:stack)
            | otherwise                 = Nothing
        eval' (Sub:_)   (_:[])          = Nothing
        eval' (Sub:_)   []              = Nothing

        eval' (Mul:rest) (a:b:stack)    = eval' rest (a*b:stack)
        eval' (Mul:_)   (_:[])          = Nothing
        eval' (Mul:_)   []              = Nothing

        eval' (Div:rest) (b:a:stack)
            | mod a b == 0              = eval' rest (div a b:stack)
            | otherwise                 = Nothing
        eval' (Div:_) (_:[])            = Nothing
        eval' (Div:_) []                = Nothing

        eval' ((Term num):rest) stack   = eval' rest (num:stack)

-- för alla permutationer av input inkl längdvariationer n!
-- och alla permutationer av operatorer ((n-1)! styck)
-- append ()
