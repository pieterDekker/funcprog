checkTakuzity :: [Char] -> [Char]
checkTakuzity ls
  | length ls < 2   = "01"
  | last2 == "00"   = "1"
  | last2 == "01" ||
      last2 == "10" = "01"
  | last2 == "11"   = "0"
  where
    last2 = [ls !! (length ls - 2)] ++ [last ls]



recTakuzuStrings :: Int -> [Char] -> [[Char]]
recTakuzuStrings n s
  | n == 0                  = [s]
  | checkTakuzity s == "1"  = recTakuzuStrings (n - 1) (s ++ "1")
  | checkTakuzity s == "01" = recTakuzuStrings (n - 1) (s ++ "0") ++
                            recTakuzuStrings (n - 1) (s ++ "1")
  | checkTakuzity s == "0"  = recTakuzuStrings (n - 1) (s ++ "0")

takuzuStrings :: Int -> [[Char]]
takuzuStrings n = recTakuzuStrings n ""


last2Test :: [Char] -> [Char]
last2Test str = [str !! (length str - 2)] ++ [last str]
