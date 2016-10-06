join :: [Integer] -> [Integer] -> [Integer]
join (x:xs) (y:ys) 
  | x < y       = x : join xs (y:ys)
  | y < x       = y : join (x:xs) ys 
  | otherwise   = x : join xs ys

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

fibSeq :: [Integer]
fibSeq = drop 3 fib

cat :: Integer -> Integer
cat 0 = 1
cat 1 = 1
cat n = (foldr (*) 1 [ n + k | k <- [2..n]]) `div` (foldr (*) 1 [ k | k <- [2..n]])

catSeq :: [Integer]
catSeq = [cat i | i <- [2..]]

fibcats :: [Integer]
fibcats = 0:1:(join catSeq fibSeq)
