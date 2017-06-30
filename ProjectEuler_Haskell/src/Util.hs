{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -O2 #-}
module Util (divides, factorial, fibbonacci, isPrime,
             eSieve, sumOfDivisors, sumOfDivisorsPrime,
             splitOn)
  where
import Data.Array.Unboxed (Ix, UArray, (!), (//), array, assocs)

divides :: Integral t => t -> t -> Bool
divides d n = (n `mod` d) == 0

factorial :: Integral t => t -> t
factorial n = product [1..n]

fibbonacci :: Integral t => [t]
fibbonacci = 1 : 1 : zipWith (+) fibbonacci (tail fibbonacci )

isPrime :: Integral t => t -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = checkFactors 2
  where
    checkFactors f
      | f > limit     = True
      | f `divides` n = False
      | otherwise     = checkFactors (f+1)
    limit = ceiling $ sqrt $ fromIntegral n

-- sieve of eratosthenes
eSieve :: forall t. (Ix t, Integral t) => t -> [t]
eSieve m = sieve 3 (array (3,m) [(i,odd i) | i<-[3..m]] :: UArray t Bool)
    where
    sieve :: t -> UArray t Bool -> [t]
    sieve p a
       | p*p > m   = 2 : [i | (i,True) <- assocs a]
       | a!p       = sieve (p+2) $ a//[(i,False) | i <- [p*p, p*p+2*p..m]]
       | otherwise = sieve (p+2) a

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

sumOfDivisors :: (Ix t, Integral t) => t -> t
sumOfDivisors n = sumOfDivisorsPrime n (eSieve $ (ceiling . sqrt . fromIntegral) n)

splitOn :: Char -> String -> [String]
splitOn split = splitOn' ""
  where
    splitOn' :: String -> String -> [String]
    splitOn' "" "" = []
    splitOn' match ""  = [match]
    splitOn' match (c:t)
      | c==split   = match:splitOn' "" t
      | otherwise = splitOn' (match++[c]) t
