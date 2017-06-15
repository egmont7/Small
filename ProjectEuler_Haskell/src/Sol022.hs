module Sol022 (run)
  where

import System.IO (openFile, IOMode(..), hGetContents, hClose)
import Data.List (sort)
import Data.Char (isAlpha, ord)


value :: (Integer, String) -> Integer
value (i, s) = (i+1) * alphaValue s
  where
    alphaValue "" = 0
    alphaValue (c:str) = if isAlpha c then
                           toInteger (1 + ord c - ord 'A') + alphaValue str
                         else
                           alphaValue str

enumerate :: [t] -> [(Integer, t)]
enumerate = enumerate' 0
  where
    enumerate' _ [] = []
    enumerate' i (e:l) = (i,e):enumerate' (i+1) l

splitOn :: Char -> String -> [String]
splitOn split = splitOn' ""
  where
    splitOn' :: String -> String -> [String]
    splitOn' "" "" = []
    splitOn' match ""  = [match]
    splitOn' match (c:t)
      | c==split   = match:splitOn' "" t
      | otherwise = splitOn' (match++[c]) t

run :: IO ()
run = do
  handle <- openFile "./022/p022_names.txt" ReadMode
  contents <- hGetContents handle
  let
    -- enumList = take 2 $ (enumerate . sort) (splitOn ',' contents)
    enumList = (enumerate . sort) (splitOn ',' contents)
  -- putStr $ show enumList
  print $ sum $ map value enumList
  hClose handle
