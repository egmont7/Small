module Sol036 (run)
  where

import Text.Printf (printf)

isDualPalindrome :: Int -> Bool
isDualPalindrome i = isPalindrome decimal && isPalindrome binary
  where
    isPalindrome l = l == reverse l
    decimal = printf "%d" i :: String
    binary = printf "%b" i :: String


run :: IO ()
run = do
  let dualPalindromes = [i | i<-[1..1000000], isDualPalindrome i]
  print dualPalindromes
  print $ sum dualPalindromes
