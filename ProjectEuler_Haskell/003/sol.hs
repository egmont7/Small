

isPrime :: Integer -> Bool
isPrime i = isPrime' i 2

isPrime' :: Integer -> Integer -> Bool
isPrime' i c = if i == 1              then False
               else if i == 2         then True
               else if i `mod` c == 0 then False
               else if (c == (i-1))   then True
               else isPrime' i (c+1)

sqrt' :: Integer -> Integer
sqrt' x = floor $ sqrt $ fromIntegral x

primeFactors :: Integer -> [Integer]
primeFactors x = primeFactors' x 2

primeFactors' :: Integer -> Integer -> [Integer]
primeFactors' x i = if i == sqrt' x
                      then [x]
                    else if ((x `mod` i) == 0) && (isPrime i)
                      then i:(primeFactors' (x `div` i) 2)
                    else
                      primeFactors' x (i+1)

-- filter' :: (Int, Bool) -> Bool
-- filter' a = snd a

-- primes = [fst x | x <- filter filter' (zip [1..10000] (map isPrime [1..10000]))]



main :: IO()
main = putStrLn $ show $ maximum $ primeFactors 600851475143
