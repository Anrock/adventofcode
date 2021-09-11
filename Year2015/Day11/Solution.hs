module Year2015.Day11.Solution (solve) where
import Data.List (group)

solve :: String -> IO ()
solve initial = do
  let pws = do first <- nextPassword initial
               second <- nextPassword first
               pure (first, second)
  print pws

nextPassword :: String -> Maybe String
nextPassword pw = let (exhausted, candidate) = naiveIncrement pw in
                    if satisfies candidate
                    then Just candidate
                    else if exhausted then Nothing else nextPassword candidate

satisfies :: String -> Bool
satisfies p = hasStraight p && noForbiddenLetters p && hasPairs p

noForbiddenLetters :: String -> Bool
noForbiddenLetters p = 'i' `notElem` p && 'o' `notElem` p && 'l' `notElem` p

hasPairs :: String -> Bool
hasPairs p = length allPairs >= 2 && pairsUnique allPairs
  where allPairs = filter (\g -> length g == 2) (group p)
        pairsUnique xs = head xs `notElem` tail xs

hasStraight :: String -> Bool
hasStraight p@(x:y:z:_) = y == succ x && z == succ y || hasStraight (tail p)
hasStraight _ = False

naiveIncrement :: String -> (Bool, String)
-- base case
naiveIncrement [] = (False, [])
-- general single char
naiveIncrement [l] = let overflow = l == 'z' in (overflow, [if overflow then 'a' else succ l])
-- general string
naiveIncrement (x:xs) =
  let (overflow, xs') = naiveIncrement xs
      (overflow', [x']) = if overflow then naiveIncrement [x] else (False, [x])
  in (overflow', x':xs')
