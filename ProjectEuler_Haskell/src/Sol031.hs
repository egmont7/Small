module Sol031 (run)
  where

-- Naive recursive solution (could improve w/ memoization, but fast enough for £2)
n :: Int -> [Int] -> Int
n 0 _ = 1  -- Found a solution
n _ [] = 0 -- Out of coins to try
n t (d:r) = sum [n (t-d*i) r | i <-[0..t`div`d]]

run :: IO ()
run = print $ n 200 [200,100,50,20,10,5,2,1]
