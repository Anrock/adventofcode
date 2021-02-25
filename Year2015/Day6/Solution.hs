{-# LANGUAGE TupleSections #-}
module Year2015.Day6.Solution (solve) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Functor (($>))
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Array

solve :: String -> IO ()
solve input = case parse (parseAction `sepEndBy` eol) "" input of
  Left e -> error $ errorBundlePretty e
  Right actions -> do
    print (part1 actions)
    print (part2 actions)

data Action = TurnOn Rect | TurnOff Rect | Toggle Rect deriving (Eq, Show)
type Position = (Int, Int)
type Rect = (Position, Position)
type Lights = Array Position Int

type Parser = Parsec Void String

initialLights :: Lights
initialLights = Data.Array.listArray ((0, 0), (999, 999)) (repeat 0)

parseAction :: Parser Action
parseAction = do
  t <- parseType
  space1
  p1 <- parsePosition
  space1
  _ <- string "through"
  space1
  p2 <- parsePosition
  pure $ t (p1, p2)

parseType :: Parser (Rect -> Action)
parseType = try (string "turn on" $> TurnOn) <|> string "turn off" $> TurnOff <|> string "toggle" $> Toggle

parsePosition :: Parser Position
parsePosition = do
  d1 <- decimal
  _ <- char ','
  d2 <- decimal
  pure (d1, d2)

rectToIx :: Rect -> [Position]
rectToIx ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

part1 :: Traversable t => t Action -> Int
part1 actions = countLights $ apply1 actions initialLights

apply1 :: Traversable t => t Action -> Lights -> Lights
apply1 actions lights = foldl go lights actions
  where go :: Lights -> Action -> Lights
        go l action = case action of
          TurnOn rect -> l // fmap (, 1) (rectToIx rect)
          TurnOff rect -> l // fmap (, 0) (rectToIx rect)
          Toggle rect -> l // fmap (\pos -> (pos, toggle (l ! pos))) (rectToIx rect)
            where toggle 0 = 1
                  toggle 1 = 0

countLights :: Lights -> Int
countLights = length . filter (== 1) . elems

part2 :: Traversable t => t Action -> Int
part2 actions = countBrightness $ apply2 actions initialLights

apply2 :: Traversable t => t Action -> Lights -> Lights
apply2 actions lights = foldl go lights actions
  where go :: Lights -> Action -> Lights
        go l action = case action of
          TurnOn rect -> l // fmap (\pos -> (pos, (l ! pos) + 1)) (rectToIx rect)
          TurnOff rect -> l // fmap (\pos -> (pos, let cur = (l ! pos) in if cur == 0 then 0 else cur - 1)) (rectToIx rect)
          Toggle rect -> l // fmap  (\pos -> (pos, (l ! pos) + 2)) (rectToIx rect)

countBrightness :: Lights -> Int
countBrightness = sum . elems

