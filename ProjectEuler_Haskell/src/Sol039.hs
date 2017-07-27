module Sol039 (run)
  where
import Data.List (group, sort)
import Control.Arrow ((&&&))

sides :: Int -> [(Int,(Int,Int,Int))]
sides p = [(a+b+h,(b,a,h))| a<-[1..p-2], b<-[1..a], h<-[max a b..min (p-a-b) (a+b)]]

trianglesTo :: Int -> [(Int,(Int,Int,Int))]
trianglesTo p = filter isTriangle $ sides p
  where
    isTriangle (_,(a,b,h)) = a*a+b*b==h*h

run :: IO ()
run = do
  let triangles = trianglesTo 1000
      perimeters = [p | (p,_) <- triangles]
      (maxCount, maxPerim) = maximum $ map (length &&& head) $ (group . sort) perimeters
  putStrLn $ "There are "++show maxCount++" instances w/ perimeter "++show maxPerim++"."
