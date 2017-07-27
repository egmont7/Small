module Sol041 (run)
  where

-- Note: Nine numbers cannot be done (1+2+3+4+5+6+7+8+9=45 => always dividable by 3)
--       Eight numbers cannot be done (1+2+3+4+5+6+7+8=36 => always dividable by 3)

import Util (isPrime)
import Data.List (permutations)

pandigitals :: Int -> [String]
pandigitals l = concat [permutations (take i "123456789") | i<-[2..l]]

run :: IO ()
run = do
  let pandigs = pandigitals 7
  print $ maximum $ filter isPrime (map read pandigs)
