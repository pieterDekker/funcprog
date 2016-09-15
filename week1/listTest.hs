import Data.Maybe

someLast :: [Char] -> Char
someLast string = last string

someScnLast :: [Char] -> Char
someScnLast string = string !! (length string - 2)

someLast2 :: [Char] -> [Char]
someLast2 string = [(someLast string)] ++ [(someScnLast string)]

testString :: String -> Bool
testString s = s == "Hoi"
  {-| s == "Hoi" = True
  | otherwise  = False-}
