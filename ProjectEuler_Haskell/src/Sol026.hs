module Sol026 (run)
  where

import Data.List (elemIndex)

elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' e l = map2Num $ elemIndex e l
  where
    map2Num Nothing = -1
    map2Num (Just idx) = idx

longDiv :: Int -> Int
longDiv den = longDiv' 10 []
  where
    longDiv' :: Int -> [Int] -> Int
    longDiv' 0 rems   = 0
    longDiv' rem rems
      | idx == -1  = longDiv' (10*m) (rem:rems)
      | otherwise =  idx + 1
      where
        (d,m) = divMod rem den
        idx = elemIndex' rem rems

mapZip :: (a->b) -> [a] -> [(b, a)]
mapZip f l = zip (map f l) l

run :: IO ()
run = do
  let (len, maxNum) = maximum $ mapZip longDiv [2..999]
  putStrLn $ "The longest repeating period is " ++ show len ++ " for the number " ++ show maxNum ++ "."
