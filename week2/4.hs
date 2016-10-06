removables :: [Integer]
removables = [i + j + (2 * i * j) | j <- [1..], i <- [1..j]]

isRemovable :: Integer -> Bool
isRemovable n = n == head ( dropWhile (<n) removables )

primes :: [Integer]
primes = 2 : [(2 * n) + 1 | n <- [1..], not (isRemovable n)]

--someRemovables = [4,7,10,13,16,19,22,25,28,31,12,17,22,27,32,37,42,47,52,24,31,38,45,52,59,66,73,40,49,58,67,76,85,94,60,71,82,93,104,115,84,97,110,123,136,112,127,142,157,144,161,178,180,199,220]

slc :: Int -> [(Int, Int)]
slc n = [(i, j) | i <- [1..n], j <- [i..n]]
