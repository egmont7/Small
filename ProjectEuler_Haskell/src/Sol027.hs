module Sol027 (run)
  where

import Util (eSieve)
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (member)

primeLength :: Int -> Int -> Set.Set Int -> Int
primeLength a b primes = primeLength' 0
  where
    primeLength' n
      | k `member` primes = primeLength' (n+1)
      | otherwise  = n
      where
        k = n*n + a*n + b


run :: IO ()
run = do
  let range = 1000
      max = 2*range*range + range
      primes = Set.fromList $ map fromIntegral (eSieve max :: [Int])
  print $ maximum [(primeLength a b primes, (a,b), a*b) | a <- [-range..range-1], b <- [-range..range]]
