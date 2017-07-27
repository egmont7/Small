{--# LANGUAGE ScopedTypeVariables #--}
module Sol033 (run)
  where
import Data.Ratio ((%), denominator)

candidates :: [((Int, Int), (Int, Int))]
candidates = concat [candsSharing i | i <- [1..9]]
  where
    candsSharing :: Int -> [((Int, Int), (Int, Int))]
    candsSharing d = concat [[((d+10*i, d+10*j), (i, j)),
                              ((10*d+i, d+10*j), (i, j)),
                              ((d+10*i, 10*d+j), (i, j)),
                              ((10*d+i, 10*d+j), (i, j))] | i <-[1..9], j<-[1..9]]

isCurious :: ((Int, Int), (Int, Int)) -> Bool
isCurious ((n1,d1),(n2,d2)) = n1<d1 && n1%d1 == n2%d2

run :: IO ()
run = print $ denominator $ product $ map (\(_,(n,d)) -> n%d) $ filter isCurious candidates
