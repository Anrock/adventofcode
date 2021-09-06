module Year2015.Day10.Solution (solve) where

import Data.List (group)

solve :: String -> IO ()
solve input = do
    print $ length $ iterate lookAndSay input !! 40
    print $ length $ iterate lookAndSay input !! 50

lookAndSay :: String -> String
lookAndSay input = foldr (\g acc -> say g ++ acc) "" (group input)
  where say i = show (length i) ++ [head i]
