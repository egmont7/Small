module Sol042 (run)
  where

import System.IO (openFile, IOMode(..), hGetContents)
import Data.Char (ord)
import Data.Set (Set, fromList, member)
import Text.Printf (printf)

import Util (splitOn)

trianglesTo :: Int -> Set Int
trianglesTo n = fromList [(i*(i+1)) `div` 2 | i<-[1..max]]
  where
    max = ceiling $ (sqrt(1+8*fromIntegral n)-1)/2

getWords :: String -> IO [String]
getWords filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ map (tail . init) $ splitOn ',' contents

word2Value :: String -> Int
word2Value s = sum $ map (\ c -> ord c - ord 'A' +1) s

run :: IO ()
run = do
  theWords <- getWords "src/data/p042_words.txt"
  let wordValues = [(word2Value w, w) | w<- theWords]
      tris = trianglesTo $ fst $ maximum wordValues
      triangleWords = filter (\(val, _) -> val `member` tris) wordValues
  printf "Found %d triangular words. Here they are: \n" (length triangleWords)
  mapM_ (printf "%s, ") [w | (_,w)<-triangleWords]
  putStrLn ""
