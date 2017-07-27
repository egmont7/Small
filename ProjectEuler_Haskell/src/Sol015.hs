module Sol015 (run)
  where
import Util (factorial)

countPaths :: Integer -> Integer -> Integer
countPaths n m = factorial (n+m) `div` factorial n * factorial m

run :: IO()
run = print $ countPaths 20 20
