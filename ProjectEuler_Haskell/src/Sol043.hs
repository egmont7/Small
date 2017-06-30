module Sol043 (run)
  where

import Data.List (intercalate)
import Data.Array (listArray, (!))
import Util (divides)

import Text.Printf (printf)

selectElem :: Int -> [a] -> (a, [a])
selectElem n l = (e, before++after)
  where
    (before, e:after) = splitAt n l

pickOne :: (a,a,a) -> [a] -> [((a,a,a), [a])]
pickOne _ [] = []
pickOne (e1,e2,_) rem = [((c,e1,e2), r) | (c,r)<-picks]
  where
     picks = [selectElem i rem | i<-[0..length rem-1]]

subDivPandigs :: [Int]
subDivPandigs = map (read . reverse . drop 3) (subDivPandigs' 0 ('0','0','0') "0123456789")
  where
    subDivPandigs' i val@(e1,e2,e3) digits
      | i==10 = [[e3,e2,e1]]
      | test i val = concat [map (e3:) (subDivPandigs' (i+1) val' digits')
                              | (val', digits')<-pickOne val digits]
      | otherwise  = []
    test n (e1, e2, e3) = (testDivs ! n) `divides` x
        where
          x = read [e1,e2,e3] :: Int
          testDivs = listArray (0,9) [1,1,1,17,13,11,7,5,3,2]

run :: IO ()
run = do
  let pandigs = subDivPandigs
  printf "Found %d numbers satisfying the requirements. Here they are:\n" (length pandigs)
  putStrLn $ intercalate ", " (map show pandigs)
  printf "The sum is %d\n" (sum pandigs)
