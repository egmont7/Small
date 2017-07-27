module Sol007 (run)
  where

isPrime :: Integer -> Bool
isPrime i = isPrime' i 2 (floor $ sqrt $ fromIntegral i )

isPrime' :: Integer -> Integer -> Integer -> Bool
isPrime' 1 _ _= False
isPrime' 2 _ _= True
isPrime' i c s= if i `mod` c == 0 then False
                else if (c >= s)  then True
                else isPrime' i (c+1) s

ithPrime :: Integer -> Integer
ithPrime 1 = 2
ithPrime n = ithPrime' 1 3 n

-- ithPrime' :: (Integer a) => a -> a -> a -> a
ithPrime' :: Integer -> Integer -> Integer -> Integer
ithPrime' i x n = if isPrime x then
                    if (i+1)==n then x
                    else ithPrime' (i+1) (x+2) n
                  else ithPrime' i (x+2) n


run :: IO()
run = print $ ithPrime 10001
-- main = putStrLn $ show $ [(i, isPrime i) | i<-[2..20]]
