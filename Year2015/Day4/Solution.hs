module Year2015.Day4.Solution where

import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as B

solve :: String -> IO ()
solve input = do
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 = brute (\i -> take 5 i == "00000") 0

part2 :: String -> Int
part2 = brute (\i -> take 6 i == "000000") 0

brute :: (String -> Bool) -> Int -> String -> Int
brute p i s = if p . show . md5 . B.pack $ s <> show i then i else brute p (succ i) s

