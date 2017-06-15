module Sol002 (run)
  where

limit = 4000000
fibbo :: Int -> Int -> [Int]
fibbo a b = if a < limit
            then a:(fibbo b (a+b))
            else []

fibboSum :: Int
fibboSum = sum [ item | item <- fibbo 1 2, even item]

run :: IO()
run = putStrLn (show fibboSum)
