module Sol030 (run)
  where


pow = 5

digits :: (Integral t, Read t, Show t) => t -> [t]
digits n = [read [d] | d <- show n]

equates :: Int -> Bool
equates n = n == sum (map (^pow) (digits n))


run :: IO ()
run = do
  let nums = [n | n<-[10..10^(pow+1)], equates n]
  putStr "The numbers are "
  print nums
  putStr "The Sum is "
  print $ sum nums
