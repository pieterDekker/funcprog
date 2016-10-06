removables :: [Integer]
removables = [i + j + (2 * i * j) | j <- [1..], i <- [1..j]]

isRemovable :: Integer -> Bool
isRemovable n = n == head ( dropWhile (<n) removables )

primes :: [Integer]
primes = 2 : [(2 * n) + 1 | n <- [1..], not (isRemovable n)]

-- Do not change the following wrapper code
wrapper :: String -> [Integer]
wrapper input = take (read input::Int) primes

main =  print . wrapper =<< getLine
