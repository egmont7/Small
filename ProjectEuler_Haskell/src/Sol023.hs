module Sol023 (run)
  where

import qualified Data.Set as Set
import qualified Math.Combinat.Sets as Sets
import qualified Util

maxNoSum :: Integer
maxNoSum = 28123

pairs :: [t] -> [[t]]
pairs = Sets.combine 2

sqrtInt :: Integral t => t -> t
sqrtInt = ceiling . sqrt . fromIntegral

abundantNumbers :: [Integer]
abundantNumbers = [i | i <- [1..maxNoSum], isAbundant i primes]
  where
    isAbundant n primes = Util.sumOfDivisorsPrime n primes > 2*n
    primes = Util.eSieve $ sqrtInt maxNoSum

abundantSums :: [Integer] -> Set.Set Integer
abundantSums abNums = (Set.fromList . pairSum) (pairs abNums)
  where
    pairSum :: [[Integer]] -> [Integer]
    pairSum l = filter (<=maxNoSum) $ map (\ (e1:e2:_) -> e1+e2) l

run :: IO ()
run = do
  let sums = abundantSums abundantNumbers
      nonAbundantSums = [i | i<-[1..maxNoSum],  Set.notMember i sums]
  putStrLn $ "Found " ++ show (length abundantNumbers) ++ " abundant Numbers."
  putStrLn ("Sum of all numbers not composable from the sum of two abundant numbers: "
    ++ show (sum nonAbundantSums))
