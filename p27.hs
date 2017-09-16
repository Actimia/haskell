
module Main where

import Data.List
import Data.Maybe
import Data.Ord
rmax = 100
range = [-rmax..rmax]

anyT :: [Bool] -> Bool
anyT = foldl (||) False

isPrime :: Int -> Bool
isPrime x = not $ anyT $ map (\y -> mod x y == 0) [2..(floor $ sqrt $ fromIntegral x)]

gen (a,b) = (length $ takeWhile isPrime $ map f [0..], a*b)
    where 
        f n = n*n + a * n + b

p27 = maximumBy (comparing fst) $ map gen xs
    where
        xs = [(a,b) | a <- range, b <-range]


main = putStrLn $ show p27