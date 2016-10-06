options :: [Char] -> [Char]
options ('0':'0':[])  = "1"
options ('1':'1':[])  = "0"
options []            = "01"
options (x:xs)          = options xs
    
isTakuzu :: String -> Int -> Int -> Bool
isTakuzu [] ones zeros  = ones == zeros
isTakuzu (x:xs) ones zeros 
  | read [x] == 1         = isTakuzu xs (ones + 1) zeros
  | otherwise           = isTakuzu xs ones (zeros + 1)

recTakuzuStrings :: Int -> [Char] -> [[Char]]
recTakuzuStrings n s
  | n == 0 && isTakuzu s 0 0 = [s]
  | n == 0             = []
  | options s == "1"   = recTakuzuStrings (n - 1) (s ++ "1")
  | options s == "01"  = recTakuzuStrings (n - 1) (s ++ "0") ++ recTakuzuStrings (n - 1) (s ++ "1")
  | options s == "0"   = recTakuzuStrings (n - 1) (s ++ "0")

takuzuStrings :: Int -> [[Char]]
takuzuStrings n = recTakuzuStrings n ""
