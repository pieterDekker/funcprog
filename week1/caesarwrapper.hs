import Data.Char

code :: (Int -> Int -> Int) -> Int -> Char -> Char
code o key c
  | ord c >= 65 && ord c <= 90      = chr ((((ord c - 65) `o` key) `mod` 26) + 65)
  | otherwise                       = ' '

codeHelper :: (Int -> Int -> Int) -> Int -> Int -> String -> String
codeHelper _ _ _ [] = []
codeHelper f key idx (' ':str) = ' ': (codeHelper f key idx str)
codeHelper f key idx (c:str) = (code f (idx * key) c) : (codeHelper f key (idx+1) str)

cipherEncode :: Int -> String -> String
cipherEncode key str = codeHelper (-) key 1 str

cipherDecode :: Int -> String -> String
cipherDecode key str = codeHelper (+) key 1 str

wrapper :: String -> String
wrapper line
  | cmd == "ENCODE"  = cipherEncode key txt
  | cmd == "DECODE"  = cipherDecode key txt
  where
    str  = dropWhile (not.isAlpha) line
    cmd  = takeWhile isAlpha str
    tail = dropWhile (not.isDigit) str
    key = read (takeWhile isDigit tail)::Int
    txt = dropWhile (not.isAlpha) (dropWhile isDigit tail)

main =  print . wrapper =<< getLine
