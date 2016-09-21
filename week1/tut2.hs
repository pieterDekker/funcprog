{-tut2-}
or2, and2 :: [Bool] -> Bool

or2 xs = foldr (||) False xs
and2 xs = foldr (&&) True xs

elemNum, elemNum2 :: Integer -> [Integer] -> Int
elemNum _ []      = 0
elemNum i (x:xs)
  | i == x  = 1 + elemNum i xs
  | otherwise = elemNum i xs

 
elemNum2 i xs = length [x | x <- xs, x ==i]
