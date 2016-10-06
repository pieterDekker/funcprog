import Data.Char

{-     |5 1 2 + 4 * + 3 -|     -}

isNum :: String -> Bool
isNum [] = True
isNum (x:xs) = (isDigit x) && (isNum xs)

rpnEval ::  String -> Integer
rpnEval str = rpnHelper (words str) []

rpnHelper :: [String] -> [String] -> Integer
rpnHelper [] (x:_)            = read x
rpnHelper (x:xs) stck
  | isNum x                   = rpnHelper xs (x:stck)
rpnHelper ("+":xs) (x:y:stck) = rpnHelper xs (show ((+) (read y) (read x)) : stck)
rpnHelper ("-":xs) (x:y:stck) = rpnHelper xs (show ((-) (read y) (read x)) : stck)
rpnHelper ("*":xs) (x:y:stck) = rpnHelper xs (show ((*) (read y) (read x)) : stck)
rpnHelper ("/":xs) (x:y:stck) = rpnHelper xs (show (div (read y) (read x)) : stck)

