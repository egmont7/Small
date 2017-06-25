module Main
  where

import Options.Applicative
import Data.Semigroup ((<>))

import qualified Data.Time as Time
import qualified Data.Map as Map

import qualified Sol001
import qualified Sol002
import qualified Sol003
import qualified Sol004
import qualified Sol005
import qualified Sol006
import qualified Sol007
import qualified Sol015
import qualified Sol017
import qualified Sol018
import qualified Sol020
import qualified Sol022
import qualified Sol023
import qualified Sol025
import qualified Sol026
import qualified Sol027
import qualified Sol028
import qualified Sol029
import qualified Sol030
import qualified Sol031
import qualified Sol032
import qualified Sol033
import qualified Sol034
import qualified Sol067
import qualified Sol078

sol :: Int -> IO ()
sol n = runSol theSol
  where
    runSol (Just s) = s
    runSol Nothing  = putStrLn "Solution not implemented. :("
    theSol = Map.lookup n sols
    sols = Map.fromList [(-1, error "please specify a solution to run")
                       , (1 , Sol001.run)
                       , (2 , Sol002.run)
                       , (3 , Sol003.run)
                       , (4 , Sol004.run)
                       , (5 , Sol005.run)
                       , (6 , Sol006.run)
                       , (7 , Sol007.run)
                       , (15, Sol015.run)
                       , (17, Sol017.run)
                       , (18, Sol018.run)
                       , (20, Sol020.run)
                       , (22, Sol022.run)
                       , (23, Sol023.run)
                       , (25, Sol025.run)
                       , (26, Sol026.run)
                       , (27, Sol027.run)
                       , (28, Sol028.run)
                       , (29, Sol029.run)
                       , (30, Sol030.run)
                       , (31, Sol031.run)
                       , (32, Sol032.run)
                       , (33, Sol033.run)
                       , (34, Sol034.run)
                       , (67, Sol067.run)
                       , (78, Sol078.run)
                        ]

newtype SolChoice = SolChoice
  { choice      :: Int
  }

parser :: Parser SolChoice
parser = SolChoice
      <$> argument auto
          ( help "Which solution to run"
         <> showDefault
         <> value (-1)
         <> metavar "Solution Choice" )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "A Set of work-in-progress solutions for project euler")

greet :: SolChoice -> IO ()
greet (SolChoice choice) = do
  putStrLn $ "Running solution for problem " ++ show choice ++ "."
  putStrLn $ replicate 80 '-'
  start <- Time.getCurrentTime
  sol choice
  stop <- Time.getCurrentTime
  putStrLn $ replicate 80 '-'
  putStrLn $ "Finished. Solution took " ++ show (Time.diffUTCTime stop start)

