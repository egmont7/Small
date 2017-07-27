module Sol004 (run)
  where

isPalindrome :: [Char] -> Bool
isPalindrome x = if (length x) < 2
                   then True
                 else if (head x) == (last x)
                   then isPalindrome (take ((length x)-2) $ drop 1 x)
                 else False

biggestPalindrome :: Int -> Int
biggestPalindrome n = maximum [x*y | x <- [1..n], y <- [1..n] , isPalindrome $ show (x*y)]

run :: IO()
run = print $ biggestPalindrome 1000
