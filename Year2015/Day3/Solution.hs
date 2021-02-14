module Year2015.Day3.Solution where

import qualified Data.Set as S

solve :: String -> IO ()
solve input = do
  print (part1 input)
  print (part2 input)

part1 :: String -> Int
part1 = length . S.fromList . moves

part2 :: String -> Int
part2 input = length . S.fromList $ (moves . odds $ input) ++ (moves . evens $ input)

evens :: String -> String
evens is = map snd $ filter (even . fst) (zip [0..] is)

odds :: String -> String
odds is = map snd $ filter (odd . fst) (zip [0..] is)

moves :: String -> [(Int, Int)]
moves = scanl move (0, 0)

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '^' = (x, y + 1)
move (x, y) '>' = (x + 1, y)
move (x, y) 'v' = (x, y - 1)
move (x, y) '<' = (x - 1, y)
