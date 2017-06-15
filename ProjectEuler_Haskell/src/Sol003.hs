module Sol003 (run)
  where

isPrime :: Integer -> Bool
isPrime i = isPrime' i 2
  where
    isPrime' i c
      | i == 1         = False
      | i == 2         = True
      | i `mod` c == 0 = False
      | c == (i-1)     = True
      | otherwise     = isPrime' i (c+1)

sqrt' :: Integer -> Integer
sqrt' x = floor $ sqrt $ fromIntegral x

primeFactors :: Integer -> [Integer]
primeFactors x = primeFactors' x 2
  where
    primeFactors' x i
      | i == sqrt' x                   = [x]
      | ((x `mod` i) == 0) && isPrime i = i:primeFactors' (x `div` i) 2
      | otherwise                     = primeFactors' x (i+1)

run :: IO()
run = (print . maximum . primeFactors) 600851475143
