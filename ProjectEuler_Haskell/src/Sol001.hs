module Sol001 (run)
  where

multOf :: Int-> Int -> Bool
multOf number base = (number `mod` base) == 0

listOfMults :: Int -> [Int]
listOfMults top = [a | a <- [1..(top-1)], multOf a 3 || multOf a 5]

run :: IO ()
run = (print . sum . listOfMults) 1000
