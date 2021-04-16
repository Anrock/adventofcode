module Year2015.Day7.Solution where

import Data.Word (Word16)

import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

import Polysemy
import Polysemy.Trace
import Polysemy.State

import qualified Data.Map as M
import Data.Bits ((.&.), shiftL, complement, shiftR, (.|.))

solve :: String -> IO ()
solve input = case parse (some parseWire) "" input of
  Left e -> error $ errorBundlePretty e
  Right wires -> do
    let part1WireMap = M.fromList wires
    part1Answer <- runM . evalState part1WireMap . traceToIO $ evalWire "a"
    print part1Answer
    let part2WireMap = M.insert "b" (Ident . Constant $ part1Answer) part1WireMap
    part2Answer <- runM . evalState part2WireMap . traceToIO $ evalWire "a"
    print part2Answer

data Input = FromWire String | Constant Word16 deriving (Eq, Show)
data Wire = And Input Input
          | Lshift Input Input
          | Not Input
          | Or Input Input
          | Rshift Input Input
          | Ident Input
          deriving (Eq, Show)

-- | * Parsing
type Parser = Parsec Void String

space :: Parser ()
space = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser ()
symbol s = L.symbol space s >> pure ()

parseWire :: Parser (String, Wire)
parseWire = do
  signal <- parseSignal
  symbol "->"
  wireId <- parseWireId
  pure (wireId, signal)

parseWireId :: Parser String
parseWireId = lexeme (some C.lowerChar)

parseInput :: Parser Input
parseInput = (FromWire <$> parseWireId) <|> (Constant <$> lexeme L.decimal)

parseSignal :: Parser Wire
parseSignal = choice $ try <$> [ parseNot, parseAnd, parseLshift, parseOr, parseRshift, parseIdent ]

parseIdent :: Parser Wire
parseIdent = Ident <$> parseInput

parseBinary :: String -> (Input -> Input -> Wire) -> Parser Wire
parseBinary s c = do
  x <- parseInput
  symbol s
  c x <$> parseInput

parseAnd :: Parser Wire
parseAnd = parseBinary "AND" And

parseLshift :: Parser Wire
parseLshift = parseBinary "LSHIFT" Lshift

parseRshift :: Parser Wire
parseRshift = parseBinary "RSHIFT" Rshift

parseOr :: Parser Wire
parseOr = parseBinary "OR" Or

parseNot :: Parser Wire
parseNot = symbol "NOT" >> Not <$> parseInput

-- | * Wire eval
type WireMap = M.Map String Wire

evalWire :: Members [Trace, State WireMap] r => String -> Sem r Word16
evalWire thisWireId = do
  gets (M.! thisWireId) >>= \case
    Ident x    -> evalUnaryOp thisWireId x id
    Not x      -> evalUnaryOp thisWireId x complement
    And x y    -> evalBinaryOp thisWireId x y (.&.)
    Lshift x y -> evalBinaryOp thisWireId x y (\a b -> a `shiftL` fromIntegral b)
    Rshift x y -> evalBinaryOp thisWireId x y (\a b -> a `shiftR` fromIntegral b)
    Or x y     -> evalBinaryOp thisWireId x y (.|.)

evalBinaryOp :: Members [Trace, State WireMap] r => String -> Input -> Input -> (Word16 -> Word16 -> Word16) -> Sem r Word16
evalBinaryOp wId x y op = do
  x' <- evalInput x
  y' <- evalInput y
  store wId (x' `op` y')

evalUnaryOp :: Members [Trace, State WireMap] r => String -> Input -> (Word16 -> Word16) -> Sem r Word16
evalUnaryOp wId x op = do
  x' <- evalInput x
  store wId (op x')

evalInput :: Members [Trace, State WireMap] r => Input -> Sem r Word16
evalInput (Constant c) = pure c
evalInput (FromWire wId) = do
  val <- evalWire wId
  store wId val

store :: Members [Trace, State WireMap] r => String -> Word16 -> Sem r Word16
store wId val = do
  modify $ M.insert wId (Ident . Constant $ val) 
  pure val
