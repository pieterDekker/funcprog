import Data.List

removables :: [Integer]
removables = [i + j + (2 * i * j) | i <- [1..], j <- [1..], i <= j]

isRemovable2 :: Integer -> Bool
isRemovable2 n = elem n [i + j + (2 * i * j) | let n'=fromIntegral n,
                                               i<-[1..floor (sqrt (n' / 2))],
                                               let i' = fromIntegral i,
                                               j<-[i..floor( (n'-i')/(2*i'+1))]]

isRemovable :: Integer -> Bool
isRemovable n = n == head ( dropWhile (<2*n) removables )

primes :: [Integer]
primes = 2 : [(2 * n) + 1 | n <- [1..], not (isRemovable2 n)]

