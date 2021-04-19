module Year2015.Day8.Solution where

solve :: String -> IO ()
solve input = do
    print (codeCharsCount - memoryCharsCount)
    print (encodedCharsCount - codeCharsCount)
    where codeCharsCount = sum $ length <$> lines input
          memoryCharsCount = sum $ memoryLength <$> lines input
          encodedCharsCount = sum $ (2 +) . length . encode <$> lines input

memoryLength :: String -> Int
memoryLength ('\"':xs) = memoryLength xs
memoryLength ('\\':'x':_:_:xs) = 1 + memoryLength xs
memoryLength ('\\':_:xs) = 1 + memoryLength xs
memoryLength (_:xs) = 1 + memoryLength xs
memoryLength [] = 0

encode :: String -> String
encode ('\"':xs) = '\\':'\"':encode xs
encode ('\\':xs) = '\\':'\\':encode xs
encode (x:xs) = x:encode xs
encode [] = []