--functions to generate the set of all strings with the takuzu property

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
  | options s == "01"  = recTakuzuStrings (n - 1) (s ++ "0") 
                      ++ recTakuzuStrings (n - 1) (s ++ "1")
  | options s == "0"   = recTakuzuStrings (n - 1) (s ++ "0")

takuzuStrings :: Int -> [[Char]]
takuzuStrings n = recTakuzuStrings n ""

--a function to test wether an incomplete takuzu string matches another 
--string for as far as the first string is complete

isTakuzuStringMatch :: String -> String -> Bool
isTakuzuStringMatch (x:xs) (y:ys) 
  | x == '.' || y == '.'  = isTakuzuStringMatch xs ys
  | not (x == y)          = False
  | x == y                = isTakuzuStringMatch xs ys
isTakuzuStringMatch [] []   = True

--attempts to match all strings from the given list of strings
--results in a list of all matches

matchStringSet :: String -> [String] -> [String]
matchStringSet x []            = []
matchStringSet x (y:ys) 
  | isTakuzuStringMatch x y = y : matchStringSet x ys
  | otherwise               = matchStringSet x ys

--fills all the incomplete spots in a string that can be inferred from 
--the complete parts

fillRow :: String -> String
fillRow (x:y:z:xs)
  | x == '0' && y == '0'  = x:(fillRow (y:'1':xs))
  | x == '1' && y == '1'  = x:(fillRow (y:'0':xs))
  | y == '0' && z == '0'  = '1':(fillRow (y:z:xs))
  | y == '1' && z == '1'  = '0':(fillRow (y:z:xs))
  | otherwise             = x:(fillRow (y:z:xs))
fillRow (x:y:[]) = x:y:[]

--fills all the incomplete spots in the given list of strings that 
--can be inferred from the complete parts of those strings

fillAllRows :: [String] -> [String]
fillAllRows (x:xs) = (fillRow x): (fillAllRows xs)
fillAllRows []     = []

--checks wether a string has all positions filled

isMatched :: String -> Bool
isMatched [] = True
isMatched (x:xs) = (not (x == '.'))  && (isMatched xs)

--checks wether the third string can be put below the first and the 
--second as in adding a row in a takuzu

isVerticallyPossible :: String -> String -> String -> Bool
isVerticallyPossible ('0':xs) ('0':ys) ('1':[]) = True
isVerticallyPossible ('0':xs) ('0':ys) ('1':zs) = isVerticallyPossible xs ys zs
isVerticallyPossible ('0':xs) ('0':ys) ('0':zs) = False

isVerticallyPossible ('1':xs) ('1':ys) ('0':[]) = True
isVerticallyPossible ('1':xs) ('1':ys) ('0':zs) = isVerticallyPossible xs ys zs
isVerticallyPossible ('1':xs) ('1':ys) ('1':zs) = False

isVerticallyPossible (_:xs)   (_:ys)   (_:zs)   = isVerticallyPossible xs ys zs
isVerticallyPossible []       []       []       = True

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _        = False

matchSet x = matchStringSet (fillRow x) (takuzuStrings (length x))

isCorrectTakuzu :: [String] -> Bool
isCorrectTakuzu []                                                        = True
isCorrectTakuzu (x:y:z:xs)
  | length (matchSet x) == 0                                              = False
  | isMatched x && (isVerticallyPossible x y z)                           = isCorrectTakuzu (y:z:xs)
  | isMatched x                                                           = False

  | otherwise                                                             = (foldr xor False [isCorrectTakuzu (match:y:z:xs) | match <- (matchSet x)])
isCorrectTakuzu (x:xs)
  | length (matchSet x) == 0                                              = False
  | isMatched x                                                           = isCorrectTakuzu xs
  | otherwise                                                             = (foldr xor False [isCorrectTakuzu (match:xs) | match <- (matchSet x)])



main =  print . isCorrectTakuzu .lines =<< getContents
