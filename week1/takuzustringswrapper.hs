options :: [Char] -> [Char]
options ('0':'0':[])  = "1"
options ('1':'1':[])  = "0"
options []            = "01"
options (x:xs)          = options xs

recTakuzuStrings :: Integer -> [Char] -> [[Char]]
recTakuzuStrings n s
  | n == 0             = [s]
  | options s == "1"   = recTakuzuStrings (n - 1) (s ++ "1")
  | options s == "01"  = recTakuzuStrings (n - 1) (s ++ "0") ++ recTakuzuStrings (n - 1) (s ++ "1")
  | options s == "0"   = recTakuzuStrings (n - 1) (s ++ "0")

takuzuStrings :: Integer -> [[Char]]
takuzuStrings n = recTakuzuStrings n ""

wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Integer)

main =  print . wrapper . words =<< getLine
