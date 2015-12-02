

sumOfSquares :: Integer -> Integer
sumOfSquares i = sum [x^2 | x <- [1..i]]

squareOfSums :: Integer -> Integer
squareOfSums i = (sum [x | x <- [1..i]])^2

difference :: Integer -> Integer
difference i = (squareOfSums i) - (sumOfSquares i)

main :: IO()
main = putStrLn $ show $ difference 100
