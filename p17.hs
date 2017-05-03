


numtostr :: Int -> String
numtostr n
    | n >= 1000 = "onethousand" ++ numtostr (n-1000)
    | n >=100 && mod n 100 == 0 = numtostr (quot n 100) ++ "hundred"
    | n >= 100 = numtostr (quot n 100) ++ "hundredand" ++ numtostr (n- ((quot n 100) * 100))
    | n >= 90 = "ninety" ++ numtostr (n-90)
    | n >= 80 = "eighty" ++ numtostr (n-80)
    | n >= 70 = "seventy" ++ numtostr (n-70)
    | n >= 60 = "sixty" ++ numtostr (n-60)
    | n >= 50 = "fifty" ++ numtostr (n-50)
    | n >= 40 = "forty" ++ numtostr (n-40)
    | n >= 30 = "thirty" ++ numtostr (n-30)
    | n >= 20 = "twenty" ++ numtostr (n-20)
    | n == 19 = "nineteen"
    | n == 18 = "eighteen"
    | n == 17 = "seventeen"
    | n == 16 = "sixteen"
    | n == 15 = "fifteen"
    | n == 14 = "fourteen"
    | n == 13 = "thirteen"
    | n == 12 = "twelve"
    | n == 11 = "eleven"
    | n == 10 = "ten"
    | n == 9 = "nine"
    | n == 8 = "eight"
    | n == 7 = "seven"
    | n == 6 = "six"
    | n == 5 = "five"
    | n == 4 = "four"
    | n == 3 = "three"
    | n == 2 = "two"
    | n == 1 = "one"
    | otherwise = ""

p17 lim = sum $ map (length . numtostr) [1..lim]
