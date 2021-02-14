module Year2015.Day2.Solution where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void (Void)

solve :: String -> IO ()
solve input = do
  case parse giftExtensions "" input of
    Right extensions -> print (part1 extensions) >> print (part2 extensions)
    Left e -> putStrLn (errorBundlePretty e)

type Parser = Parsec Void String
type Extension = (Int, Int, Int)

giftExtensions :: Parser [Extension]
giftExtensions = sepEndBy (do
  l <- decimal
  char 'x'
  w <- decimal
  char 'x'
  h <- decimal
  pure (l, w, h)) eol

part1 :: [Extension] -> Int
part1 extensions = sum (fmap wrapping extensions)
  where surfaceAreas :: Extension -> [Int]
        surfaceAreas (l, w, h) = [l * w, w * h, h * l]
        boxSurface :: [Int] -> Int
        boxSurface surfaces = sum (fmap (2*) surfaces)
        slack :: [Int] -> Int
        slack surfaces = minimum surfaces
        wrapping :: (Int, Int, Int) -> Int
        wrapping gift = let s = surfaceAreas gift in boxSurface s + slack s

part2 :: [Extension] -> Int
part2 extensions = sum (fmap ribbon extensions)
  where ribbon :: Extension -> Int
        ribbon e = bow e + wrap e
        bow :: Extension -> Int
        bow (l,w,h) = l * w * h
        wrap :: Extension -> Int
        wrap (l,w,h) = minimum [perimeter l w, perimeter w h, perimeter h l]
        perimeter :: Int -> Int -> Int
        perimeter a b = 2 * a + 2 * b
