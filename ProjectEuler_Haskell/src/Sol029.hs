module Sol029 (run)
  where
import qualified Data.Set as Set

range = 100 :: Integer

run :: IO ()
run = print $ Set.size $ Set.fromList [a^b | a<-[2..range], b<-[2..range]]
