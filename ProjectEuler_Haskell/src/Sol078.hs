{-# LANGUAGE ScopedTypeVariables #-}
module Sol078 (run)
  where
import Data.Array
import Data.List

-- My Crappy Solution
pentas :: forall t. Integral t => t -> [t]
pentas n = penta' 1
  where
    penta' :: t -> [t]
    penta' i
      | penta_pos>n = []
      | penta_neg>n = [penta_pos]
      | otherwise   = penta_pos:penta_neg:penta' (i+1)
      where
        penta_pos = i*(3*i-1) `div` 2
        penta_neg = -i*(-3*i-1) `div` 2

sumMod :: [Int] -> Int -> Int
sumMod [] _ = 0
sumMod (e:l) m = (e + sumMod l m) `mod` m

sumIdx :: [Int] -> [Int] -> [Int] -> Int -> Int
sumIdx list indices signs modulo = sumIdx' (zip [0,1..] list) indices signs
  where
    sumIdx' [] _ _ = 0
    sumIdx' _ [] _ = 0
    sumIdx' ((curr_idx,elem):l_elem) (idx:l_idx) (sign:l_sign)
      | idx == curr_idx = (sign*elem + sumIdx' l_elem l_idx l_sign) `mod` modulo
      | otherwise      = sumIdx' l_elem (idx:l_idx) (sign:l_sign)

p :: Int -> [Int] -> Int
p n l
  | n < 0     = 0
  | n == 0     = 1
  | otherwise = sumIdx l (map (subtract 1) $ pentas n) (cycle [1,1,-1,-1]) 1000000


genPs :: Int -> [Int]
genPs n = genPs' 0 []
  where
    genPs' i l
      | i==n = l
      -- | nextP==0 = nextP:l
      | nextP==0 = [i]
      | otherwise = genPs' (i+1) (nextP:l)
      where nextP = p i l


-- Credit to flesmes for a much better solution

-- n such that P(n) is divisible by m
searchDivisible :: Int -> Int -> Maybe Int
searchDivisible lim m = elemIndex 0 $ elems $ pFunctionArray lim m

-- Values of the partition function P modulus m
-- from P(0) to P(limit)
pFunctionArray :: Int -> Int -> Array Int Int
pFunctionArray lim modulus = arr
    where arr = let pFunction 0 = 1
                    pFunction n = sumMod $
                                  map term $
                                  takeWhile ((<= n) . number) pentagonal
                        where term (s,m) = s * (arr ! (n-m))
                              number = snd
                    pentagonal = [ (sign (k-1), (k*(3*k-1)) `div` 2) |
                                   k <- concatMap (\i -> [i,-i]) [1..] ]
                    sign n = (-1) ^ (n `mod` 2)
                 in listArray (0,lim) [pFunction i | i <- [0..lim]]
                        where sumMod = foldr (\x a -> (x + a) `mod` modulus) 0
run :: IO()
run = print $ searchDivisible 60000 1000000
-- run = print $ genPs 60000
