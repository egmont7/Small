import Text.Printf

multOf :: Int-> Int -> Bool
multOf number base = (number `mod` base) == 0

listOfMults :: Int -> [Int]
listOfMults top = [a | a <- [1..(top-1)], (multOf a 3) || (multOf a 5)]

main :: IO()
-- main = (putStrLn . toStr . sum . listOfMults) 10
main = (putStrLn . show . sum . listOfMults) 1000
