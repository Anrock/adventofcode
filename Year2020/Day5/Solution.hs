module Year2020.Day5.Solution where

solve :: String -> IO ()
solve input = print $ maximum ids
  where passes = words input
        ids = calcId . seatCoord <$> passes
        calcId (r, c) = r * 8 + c

seatCoord :: String -> (Int, Int)
seatCoord input = case foldl bisect ([0..127], [0..7]) input of
                    ([row], [column]) -> (row, column)
                    (rows, columns) -> error $ "Coords didn't converge " <> show rows <> " " <> show columns

bisect :: ([Int], [Int]) -> Char -> ([Int], [Int])
bisect (rows, columns) sym =
  case sym of
    'F' -> (low rows, columns)
    'B' -> (high rows, columns)
    'L' -> (rows, low columns)
    'R' -> (rows, high columns)
    unk -> error $ "Unexpected symbol" <> [unk]
  where high i = drop (half i) i
        low  i = take (half i) i
        half i = length i `div` 2
