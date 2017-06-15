module Sol017 (run)
  where
onesLen :: Int -> String
onesLen 1  = "one"
onesLen 2  = "two"
onesLen 3  = "three"
onesLen 4  = "four"
onesLen 5  = "five"
onesLen 6  = "six"
onesLen 7  = "seven"
onesLen 8  = "eight"
onesLen 9  = "nine"
onesLen 10 = "ten"
onesLen 11 = "eleven"
onesLen 12 = "twelve"
onesLen 13 = "thirteen"
onesLen 14 = "fourteen"
onesLen 15 = "fifteen"
onesLen 16 = "sixteen"
onesLen 17 = "seventeen"
onesLen 18 = "eighteen"
onesLen 19 = "nineteen"
onesLen _ = [] -- NULL

tensLen :: Int -> String
tensLen x
    | x < 20 = onesLen x
    | otherwise = tens (div x 10) ++ onesLen (mod x 10)
    where
        tens 2 = "twenty" -- twenty
        tens 3 = "thirty" -- thirty
        tens 4 = "forty" -- forty
        tens 5 = "fifty" -- fifty
        tens 6 = "sixty" -- sixty
        tens 7 = "seventy" -- seventy
        tens 8 = "eighty" -- eighty
        tens 9 = "ninety" -- ninety
        tens _ = [] -- NULL

numLen :: Int -> String
numLen 1000 = "onethousand"
numLen x
    | x >= 100 && (x `mod` 100 == 0) = numLen (div x 100) ++ "hundred"
    | x >= 100 = numLen (div x 100) ++ "hundredand" ++ tensLen (mod x 100)
    | otherwise = tensLen $ mod x 100

run :: IO()
-- main = putStrLn $ foldl join "" [numLen i | i <- [1..1000]]
--     where
--         join a b = a ++ ['\n'] ++ b
run = print $ sum [length $ numLen i | i <- [1..1000]]
