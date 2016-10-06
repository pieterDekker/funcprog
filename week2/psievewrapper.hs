removables :: [Integer]
removables = [i + j + (2 * i * j) | j <- [1..], i <- [1..j]]

removList :: [Integer]
removList  = 4:7:removables

isRemovable :: Integer -> Bool
isRemovable n = elem n [i + j + (2 * i * j) | let n'=fromIntegral n,
                                               i<-[1..floor (sqrt (n' / 2))],
                                               let i' = fromIntegral i,
                                               j<-[i..floor( (n'-i')/(2*i'+1))]]
                                               
primes = 2 : [(2 * n) + 1 | n <- [1..], not (isRemovable n)]

-- Do not change the following wrapper code
wrapper :: String -> [Integer]
wrapper input = take (read input::Int) primes

main =  print . wrapper =<< getLine
