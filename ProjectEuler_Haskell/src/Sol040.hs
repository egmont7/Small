module Sol040 (run)
  where

champernowne :: String
champernowne = champ 1
  where
    champ i = show i ++ champ (i+1)

run :: IO ()
run = do
  let digits = [read [champernowne !! (pred 10^i:: Int)] :: Int| i<-[0..6]]
  print digits
  print $ product digits
