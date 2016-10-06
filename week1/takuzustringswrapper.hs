checkTakuzity :: [Char] -> [Char]
checkTakuzity ls
  | length ls < 2   = "01"
  | last2 == "00"   = "1"
  | last2 == "01" ||
      last2 == "10" = "01"
  | last2 == "11"   = "0"
  where
    last2 = [ls !! (length ls - 2)] ++ [last ls]

recTakuzuStrings :: Integer -> [Char] -> [[Char]]
recTakuzuStrings n s
  | n == 0                = [s]
  | checkTakuzity s == "1"  = recTakuzuStrings (n - 1) (s ++ "1")
  | checkTakuzity s == "01" = recTakuzuStrings (n - 1) (s ++ "0") ++ recTakuzuStrings (n - 1) (s ++ "1")
  | checkTakuzity s == "0"  = recTakuzuStrings (n - 1) (s ++ "0")

takuzuStrings :: Integer -> [[Char]]
takuzuStrings n = recTakuzuStrings n ""


wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Integer)

main =  print . wrapper . words =<< getLine
