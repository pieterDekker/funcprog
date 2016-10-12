{-# OPTIONS_GHC -O2 #-}
-- compiled execution time for 100001 is 0.877s (about 27% better)

marked :: [Integer]
marked = fold $ map mark [1..]
    where
        fold ((x:xs):t) = x : (xs `union` fold t)
        mark i = map (calc i) [i..]
        calc i j = i + j + 2*i*j

union :: [Integer] -> [Integer] -> [Integer]
union a         []        = a
union []        b         = b
union (x:xs) (y:ys)
    | x < y     = x : union xs (y:ys)
    | x == y    = x : union xs ys
    | otherwise = y : union (x:xs) ys

removeComposites :: [Integer] -> [Integer] -> [Integer]
removeComposites [] _          = []
removeComposites (s:ss) []     = 2*s + 1 : removeComposites ss []
removeComposites (s:ss) (c:cs)
    | s == c    = removeComposites ss cs
    | s > c     = removeComposites (s:ss) cs
    | otherwise = 2*s + 1 : removeComposites ss (c:cs)

primes :: [Integer]
primes = 2:(removeComposites [1..] marked)

-- Do not change the following wrapper code
wrapper :: String -> [Integer]
wrapper input = take (read input::Int) primes

main =  print . wrapper =<< getLine
