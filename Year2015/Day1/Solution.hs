module Year2015.Day1.Solution where

solve :: String -> IO ()
solve input = do
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 = foldr (\s f -> if s == '(' then f + 1 else f - 1) 0

part2 :: String -> Int
part2 = go 0 1
  where
    go :: Int -> Int -> String -> Int
    go (-1) index _ = index - 1
    go fl index ('(':is) = go (fl + 1) (index + 1) is
    go fl index (')':is) = go (fl - 1) (index + 1) is
