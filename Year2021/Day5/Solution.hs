{-# LANGUAGE  TupleSections #-}
module Year2021.Day5.Solution (solve) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, sepEndBy, errorBundlePretty, between)
import Text.Megaparsec.Char (eol, string, space1, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Function ((&))

import Data.Map qualified as M

type Parser = Parsec Void String

type Point = (Int, Int)
type Line = (Point, Point)

solve :: String -> IO ()
solve input = parse (parseLine `sepEndBy` eol) "" input
            & either
                (putStrLn . errorBundlePretty)
                (print . length . M.filter (>= 2) . M.fromListWith (+) . fmap (,1 :: Int) . concatMap lineToPoints)

parseLine :: Parser Line
parseLine = (,) <$> point <* between space1 space1 (string "->") <*> point
    where point :: Parser (Int, Int)
          point = (,) <$> decimal <* char ',' <*> decimal

lineToPoints :: Line -> [Point]
lineToPoints ((x1, y1), (x2, y2)) =
  let xs = count x1 x2
      ys = count y1 y2
  in if x1 == x2 || y1 == y2
  then [ (x, y) | x <- xs, y <- ys ] else zip xs ys

count :: (Ord a, Enum a) => a -> a -> [a]
count a b | a < b = enumFromTo     a          b
          | a > b = enumFromThenTo a (pred a) b
          | otherwise = [a]
