module Util (divides, factorial, fibbonacci,
             eSieve, sumOfDivisors, sumOfDivisorsPrime)
  where

divides :: Integral t => t -> t -> Bool
divides d n = (n `mod` d) == 0

factorial :: Integral t => t -> t
factorial n = product [1..n]

fibbonacci :: Integral t => [t]
fibbonacci = 1 : 1 : zipWith (+) fibbonacci (tail fibbonacci )

sieve :: Integral t => t -> [t] -> [t]
sieve _ [] = []
sieve i (n:l)
  | i `divides` n = sieve i l
  | otherwise     = n:sieve i l


eSieve :: Integral t => t -> [t]
eSieve i
  | i<=1     = []
  | otherwise = 2:eSieve' [3,5..i]
  where
    eSieve' [] = []
    eSieve' (j:l)
      | j > i*i = j:l
      | otherwise = j:eSieve' (sieve j l)

-- Repeatedly divide n by p, return p^m
-- where m is the number of factors of p in n.
repDiv :: Integral t => t -> t -> t
repDiv n p
  | m==0 = p*repDiv d p
  | otherwise = 1
  where
    (d,m) = divMod n p

-- See http://mathschallenge.net/library/number/sum_of_divisors
sumOfDivisorsPrime :: Integral t => t -> [t] -> t
sumOfDivisorsPrime _ [] = -1
sumOfDivisorsPrime n (p:primeList)
  | p*p<=n && j /= 1 = (j*p - 1) `div` (p-1) * sumOfDivisorsPrime (n `div` j) primeList
  | p*p<=n         = sumOfDivisorsPrime n primeList
  | n>1           = n+1
  | otherwise     = 1
  where
    j = repDiv n p

sumOfDivisors :: Integral t => t -> t
sumOfDivisors n = sumOfDivisorsPrime n (eSieve $ (ceiling . sqrt . fromIntegral) n)
