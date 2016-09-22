import Data.Char

ctf :: Int -> (Int -> Int -> Int)
ctf (-1) = div
ctf (-3) = (-)
ctf (-5) = (+)
ctf (-6) = (*)

rpnHelper :: [Char] -> [IntS] -> [Int]
rpnHelper (x:x2:xs) (ys)
  | isNum x && isNum x2 = 

rpnHelper (x:' ':xs) (y:ys) 
  | isNum x     = rpnHelper xs (y * 10 + i x) :ys
  | otherwise   = 
    where i a = ord a - ord '0'
    
rpnHelper (x:xs) (y:ys) = rpnHelper xs (y * 10 + i x) : ys
    where i a = ord a - ord '0'
    
rpnHelper [] ys = ys 

rpnEvalRec :: [Int] -> Int
rpnEvalRec (x:y:z:xs) = rpnEvalRec (((ctf z) x y) : xs)
rpnEvalRec (x:[]) = x

rpnEval :: [Char] -> Int
rpnEval xs = rpnEvalRec (rpnHelper ([x | x <- xs, not (isSpace x)]) [])

"355 65 +"


