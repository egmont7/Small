module Sol028 (run)
  where

run :: IO ()
run = print $ 1 + sum [4*n*n - 6*n + 6 | n <- [3,5..1001]]
