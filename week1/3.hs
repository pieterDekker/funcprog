import Data.Char

ctf :: Char -> (Int -> Int -> Int)
ctf '+' = (+)
ctf '-' = (-)
ctf '*' = (*)

isOp :: Char -> Bool
isOp c
  | c == '+'      = True
  | c == '-'      = True 
  | c == '*'      = True
  | otherwise     = False
{-
rpnEval :: [Char] -> Int
rpnEval str = rpnHelper reverse[c | c <- str, isNumOp c]
-}

rpnEval :: [Char] -> [Char]
rpnEval str = reverse[c | c <- str, isNumOp c]

rpnEval :: [Char] -> [Char] -> Int
rpnEval (x:xs) (ys)
  | isNumber x  = rpnEval xs (x:ys)
  | isOp x      = 

rpnEval [] _    = 0
