module Sol038 (run)
  where

import Data.Set (size, fromList, (\\))

isPandigital :: String -> Bool
isPandigital = (==9) . size . (\\ fromList ['0']) . fromList

genPandig :: Int -> String
genPandig d
  | length pandig == 9 = pandig
  | otherwise         = ""
  where
    pandig = pandig' 1 9
    pandig' i l
      | l <= 0     = ""
      | otherwise = s++pandig' (i+1) (l-length s)
      where
        s = show (i*d)

run :: IO ()
run = do
  let pandigitals = filter (\(d,_) -> d/="" && isPandigital d) [(genPandig i, i) | i<-[1..10000]]
      maxPandig = maximum [(read pandig::Int,i) | (pandig, i)<-pandigitals]
  putStrLn $ "Found "++show (length pandigitals)++" Pandigitals."
  putStrLn $ "From among them, "++show maxPandig++" is the biggest!"
