{--# LANGUAGE ScopedTypeVariables #--}
module Sol034 (run)
  where

import Data.Array (listArray, (!))
import Util (factorial)

sols :: [Int]
sols = filter check [3..7*(facts!'9')]
  where
    check i = i == sum [facts ! d | d<-show i]
    facts = listArray ('0','9') [factorial i | i<-[0..9]]

run :: IO ()
run = do
  let theSols = sols
  print theSols
  print $ sum theSols
