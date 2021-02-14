module Year2015.Day5.Solution where

import Data.List (group, isPrefixOf)

solve :: String -> IO ()
solve input = do
  print (part1 input)
  print (part2 input)

part1 :: String -> Int
part1 = length . filter isNice1 . lines

isNice1 :: String -> Bool
isNice1 str = hasVowels str && hasDoubleLetter str && noBadStrings str
  where hasVowels str = length (filter (`elem` "aeiou") str) >= 3
        hasDoubleLetter = any (\g -> length g >= 2) . group
        noBadStrings str = noSubstr "ab" str && noSubstr "cd" str && noSubstr "pq" str && noSubstr "xy" str
        noSubstr _ [] = True
        noSubstr sub str = not (sub `isPrefixOf` str) && noSubstr sub (tail str)

part2 :: String -> Int
part2 = length . filter isNice2 . lines

isNice2 :: String -> Bool
isNice2 str = hasRepeatingPair str && hasRepeatBetween str

hasRepeatingPair :: String -> Bool
hasRepeatingPair [] = False
hasRepeatingPair [_] = False
hasRepeatingPair xs = contains (take 2 xs) (drop 2 xs) || hasRepeatingPair (tail xs)

contains :: String -> String -> Bool
contains _ [] = False
contains x y = x `isPrefixOf` y || contains x (tail y)

hasRepeatBetween :: String -> Bool
hasRepeatBetween (a:b:c:cs) = a == c || hasRepeatBetween (b:c:cs)
hasRepeatBetween _ = False
