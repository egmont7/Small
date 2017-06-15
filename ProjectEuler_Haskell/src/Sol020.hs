module Sol020 (run)
  where
import Util (factorial)

sumOfDigits :: String -> Int
sumOfDigits "" = 0
sumOfDigits (c:s) = read [c] + sumOfDigits s

run :: IO()
run = print $ sumOfDigits $ show $ factorial 100
