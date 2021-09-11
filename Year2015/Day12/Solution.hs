module Year2015.Day12.Solution where

import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Scientific (toBoundedInteger)
import Data.Foldable
import Data.HashMap.Strict qualified as HM
import Data.Text (pack)

solve :: FilePath -> IO ()
solve inputFile = do
  input <- BS.readFile inputFile
  case decode input :: Maybe Value of
    Nothing -> error "Not a valid JSON in input"
    Just v -> do
      print $ foldValue (\v' acc -> acc + valueToNum v') 0 v
      print $ foldValue (\v' acc -> acc + withoutRed v') 0 v

foldValue :: (Value -> b -> b) -> b -> Value -> b
foldValue f acc n@(Number _) = f n acc
foldValue f acc a@(Array _) = f a acc
foldValue f acc o@(Object _) = f o acc
foldValue _ acc _ = acc

valueToNum :: Value -> Int
valueToNum (Array a) = foldr' (\v acc -> acc + valueToNum v) 0 a
valueToNum (Object o) = foldr' (\v acc -> acc + valueToNum v) 0 o
valueToNum (Number n) = case toBoundedInteger n of
                          Nothing -> error $ "Out of bounds integer" <> show n
                          Just n' -> n'
valueToNum _ = 0

withoutRed :: Value -> Int
withoutRed (Array a) = foldr' (\v acc -> acc + withoutRed v) 0 a
withoutRed (Object o') = if String (pack "red") `elem` HM.elems o'
                            then 0
                            else foldr' (\v acc -> acc + withoutRed v) 0 o'
withoutRed x = valueToNum x
