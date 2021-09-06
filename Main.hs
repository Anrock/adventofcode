module Main where

import System.Environment
import System.Exit
import Data.String.Interpolate
import Control.Monad

import qualified Year2015.Day1.Solution
import qualified Year2015.Day2.Solution
import qualified Year2015.Day3.Solution
import qualified Year2015.Day4.Solution
import qualified Year2015.Day5.Solution
import qualified Year2015.Day6.Solution
import qualified Year2015.Day7.Solution
import qualified Year2015.Day8.Solution
import qualified Year2015.Day9.Solution
import qualified Year2015.Day10.Solution
import qualified Year2020.Day5.Solution

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) (die "Not enough arguments")

  let [year, day] = args
      input = readFile [i|Year#{year}/Day#{day}/input|]

  case year of
    "2015" -> case day of
      "1" -> input >>= Year2015.Day1.Solution.solve
      "2" -> input >>= Year2015.Day2.Solution.solve
      "3" -> input >>= Year2015.Day3.Solution.solve
      "4" -> input >>= Year2015.Day4.Solution.solve
      "5" -> input >>= Year2015.Day5.Solution.solve
      "6" -> input >>= Year2015.Day6.Solution.solve
      "7" -> input >>= Year2015.Day7.Solution.solve
      "8" -> input >>= Year2015.Day8.Solution.solve
      "9" -> input >>= Year2015.Day9.Solution.solve
      "10" -> input >>= Year2015.Day10.Solution.solve
      _   -> putStrLn [i|Day #{day} not solved in year #{year}|]
    "2020" -> case day of
      "5" -> input >>= Year2020.Day5.Solution.solve
      _   -> putStrLn [i|Day #{day} not solved in year #{year}|]
    _      -> putStrLn [i|No solutions for year #{year}|]
