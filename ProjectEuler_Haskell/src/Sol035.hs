module Sol035 (run)
  where

import Data.Array (listArray, (!), (//))
-- import Data.Set ()
import Util (eSieve)

upperLim = 1000000 :: Int

rotations :: Int -> [Int]
rotations n = [read(drop i dec ++ take i dec) | i<-[1..length dec - 1]]
  where dec = show n

sols :: [Int]
sols = [p | p <- primes, all (\ r -> r<upperLim && primeLookup!r) (rotations p)]
  where
    primeLookup = listArray (0,upperLim) (repeat False) // zip primes (repeat True)
    primes = eSieve upperLim

run :: IO ()
run = do
  let theSols = sols
  putStr "Here Are the Numbers: "
  print theSols
  putStr "And here's how many there are: "
  print $ length theSols
