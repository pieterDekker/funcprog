import Data.Char

en = (-)
de = (+)

encode :: Int -> Char -> Char
encode key c
  | ord c >= 65 && ord c <= 90      = chr ((((ord c - 65) - key) `mod` 26) + 65)
  | otherwise                       = ' '

decode :: Int -> Char -> Char
decode key c
  | ord c >= 65 && ord c <= 90      = chr ((((ord c - 65) + key) `mod` 26) + 65)
  | otherwise                       = ' '

code :: (Int -> Int) -> Int -> Int
code f c key
  | ord c >= 65 && ord c <= 90      = chr ((((ord c - 65) `f` key) `mod` 26) + 65)
  | otherwise                       = ' '

cipherEncode :: Int -> String -> String
cipherEncode key str = [encode key c | (key,c) <- (zip ([key, 2*key..(length str) * key]) str)]  

cipherDecode :: Int -> String -> String
cipherDecode key str = [decode key c | (key,c) <- (zip ([key, 2*key..(length str) * key]) str)]

cipher :: (Int -> Int) -> (Int -> Char -> Char) -> Int -> String -> String
cipher g f key str = [f key c | (key,c) <- (zip ([key, 2*key..(length str) * key]) str)] 
