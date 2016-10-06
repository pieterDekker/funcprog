import Data.Char

enc :: Char -> [Char]
enc c = [chr (ord c + 10)]


split :: String -> String
split str
  | length str > 0  = enc (head str) ++ split (tail str)
  | otherwise       = []

