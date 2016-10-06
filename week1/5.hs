rep :: Double -> Int -> [Double]
rep x 0 = []
rep x n = x : rep x (n - 1)

stripZeros :: [Double] -> [Double]
stripZeros (0:xs) = stripZeros xs
stripZeros xs     = xs

recMultList :: [Double] -> [Double] -> [Double]
recMultList (x:xs) ys = addList ([x * y | y <- ys] ++ rep 0 (length xs) ) (recMultList xs ys)
recMultList [] ys = []

multList :: [Double] -> [Double] -> [Double]
multList xs ys
  | diff > 0                = recMultList xs (rep 0 (abs diff) ++ ys)
  | diff < 0                = recMultList (rep 0 (abs diff) ++ xs) ys
  | otherwise               = recMultList xs ys
  where diff = length xs - length ys

subtrList :: [Double] -> [Double] -> [Double]
subtrList (x:xs) (y:ys) 
  | length xs == length ys  = (x - y) : (subtrList xs ys)
  | length xs > length ys   = x : (subtrList xs (y:ys))
  | otherwise               = y : (subtrList (x:xs) ys)
subtrList [] [] = []

addList :: [Double] -> [Double] -> [Double]
addList [] [] = []
addList [] ys = ys
addList xs [] = xs
addList (x:xs) (y:ys)
  | length xs == length ys  = (x + y) : (addList xs ys)
  | length xs > length ys   = x : (addList xs (y:ys))
  | otherwise               = y : (addList (x:xs) ys)

divTerms :: [Double] -> [Double] -> [Double]
divTerms num denom = ((head num) / (head denom)) : (rep 0 ((length num) - (length denom)))

recPolDiv :: [Double] -> [Double] -> [Double] -> ([Double],[Double])
recPolDiv num denom quotient
  | length denom > length num = (quotient, num)
  | otherwise                 = recPolDiv (stripZeros (subtrList num subtractor)) (denom) (addList quotient intermediateDividend)
  where 
    numHighestTerm        = head num : (rep 0 ((length num) - 1))
    denomHighestTerm      = head denom : (rep 0 ((length num) - 1))
    intermediateDividend  = divTerms numHighestTerm denomHighestTerm
    subtractor            = stripZeros (multList denom intermediateDividend)

highestTerm :: [Double] -> [Double]
highestTerm num = head num : (rep 0 ((length num) - 1))

polDiv :: [Double] -> [Double] -> ([Double], [Double])
polDiv num denom = recPolDiv num denom []

