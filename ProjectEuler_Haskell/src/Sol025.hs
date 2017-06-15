module Sol025 (run)
  where

import qualified Util

fibbonacciLength :: [Int]
fibbonacciLength = map (length . show) Util.fibbonacci

firstToSize :: Int -> (Int, Int)
firstToSize n = firstToSize' 0
  where
    firstToSize' :: Int -> (Int, Int)
    firstToSize' i
      | l >= n     = (l, i+1)
      | otherwise = firstToSize' (i+1)
      where
        l = fibbonacciLength !! i

run :: IO ()
run = print $ firstToSize 1000
