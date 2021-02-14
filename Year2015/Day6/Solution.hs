module Year2015.Day6.Solution (solve) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Functor (($>))
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Array

solve :: String -> IO ()
solve input = print (part1 input)

data Action = TurnOn Rect | TurnOff Rect | Toggle Rect deriving (Eq, Show)
type Position = (Int, Int)
type Rect = (Position, Position)
type Lights = Array Position Bool

type Parser = Parsec Void String

initialLights :: Lights
initialLights = Data.Array.listArray ((0, 999), (0, 999)) (repeat False)

parseAction :: Parser Action
parseAction = do
  t <- parseType
  space1
  p1 <- parsePosition
  space1
  _ <- string "through"
  space1
  p2 <- parsePosition
  _ <- eol
  pure $ t (p1, p2)

parseType :: Parser (Rect -> Action)
parseType = try (string "turn on" $> TurnOn) <|> string "turn off" $> TurnOff <|> string "toggle" $> Toggle

parsePosition :: Parser Position
parsePosition = do
  d1 <- decimal
  _ <- char ','
  d2 <- decimal
  pure (d1, d2)

turnOn :: Lights -> Rect -> Lights
turnOn = undefined

turnOff :: Lights -> Rect -> Lights
turnOff = undefined

toggle :: Lights -> Rect -> Lights
toggle = undefined

part1 :: String -> Int
part1 input = case parse (some parseAction) "" input of
  Left e -> error $ errorBundlePretty e
  Right actions -> countLights $ apply actions initialLights

apply :: Traversable t => t Action -> Lights -> Lights
apply actions lights = foldl go lights actions
  where go :: Lights -> Action -> Lights
        go l action = case action of
          TurnOn rect -> lights // (fmap (\pos -> (True, pos)) rectToIx rect)
          TurnOff rect -> undefined
          Toggle rect -> undefined

rectToIx :: Rect -> [Position]
rectToIx ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

countLights :: Lights -> Int
countLights = length . filter (== True) . elems
