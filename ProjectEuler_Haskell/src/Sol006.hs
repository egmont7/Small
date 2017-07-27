module Sol006 (run)
  where

sumOfSquares :: Integer -> Integer
sumOfSquares i = sum [x^2 | x <- [1..i]]

squareOfSums :: Integer -> Integer
squareOfSums i = sum [1..i] ^2

difference :: Integer -> Integer
difference i = squareOfSums i - sumOfSquares i

run :: IO()
run = print $ difference 100
