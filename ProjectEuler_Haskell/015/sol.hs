factorial :: Integer -> Integer
factorial n = product [1..n]

countPaths :: Integer -> Integer -> Integer
countPaths n m = (factorial (n+m)) `div` ((factorial n) * (factorial m))

main :: IO()
main = putStrLn $ show $ countPaths 20 20
