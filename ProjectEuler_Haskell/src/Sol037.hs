module Sol037 (run)
  where

import Util (isPrime)
import Data.List (last)
import Data.Set (fromList, intersection, (\\))

fromAddingRight :: [Int]
fromAddingRight = addRight [2, 3, 5, 7]
  where
    addRight [] = []
    addRight (cand:cands)
      | isPrime cand && endsProperly = cand:addRight newCands
      | isPrime cand                = addRight newCands
      | otherwise                   = addRight cands
      where
        newCands = appendRight cand 1:appendRight cand 3:
                   appendRight cand 7: appendRight cand 9: cands
        endsProperly = last (show cand) `notElem` ['1', '9']
    appendRight a b = a*10 + b

fromAddingLeft :: [Int]
fromAddingLeft = addLeft [3, 7]
  where
    addLeft [] = []
    addLeft (cand:cands)
      | isPrime cand && specPrimeHead  = cand:addLeft cands
      | isPrime cand && endsProperly   = cand:addLeft newCands
      | isPrime cand                  = addLeft newCands
      | otherwise                     = addLeft cands
      where
        newCands = appendLeft cand 1:appendLeft cand 2:
                   appendLeft cand 3:appendLeft cand 5:
                   appendLeft cand 7:
                   appendLeft cand 9: cands
        endsProperly = head (show cand) `notElem` ['1','9']
        specPrimeHead = head (show cand) `elem` ['2','5']
    appendLeft a b = read (show b ++ show a)

run :: IO ()
run = do
  let right = fromList fromAddingRight
      left = fromList fromAddingLeft
      truncatable = intersection left right \\ fromList [2,3,5,7]
  putStrLn "The truncatable primes are: "
  print truncatable
  putStr "The sum is: "
  print $ sum truncatable
