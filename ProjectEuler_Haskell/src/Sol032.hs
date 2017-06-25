{--# LANGUAGE ScopedTypeVariables #--}
module Sol032 (run)
  where
import Data.List (permutations, splitAt)
import qualified Data.Set as Set

triples :: String -> [(Int,Int,Int)]
triples l = concatMap triples' (permutations l)
  where
    n = length l
    triples' perm = [split i j perm | (i,j)<-[(1,4),(2,3)]]
    split i j s = (read a, read b, read c)
      where
        (a, tmp) = splitAt i s
        (b, c)   = splitAt j tmp


matches :: [(Int, Int, Int)] -> Int
matches l = sum $ Set.fromList [p | (_,_,p) <- l]

run :: IO ()
run = print $ matches $ filter (\(a,b,c) -> a*b==c) (triples "123456789")
