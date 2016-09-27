modExpSeq :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
modExpSeq b e m [] = modExpSeq b e m [1]
modExpSeq b 0 m xs = xs
modExpSeq b e m (x:xs) = modExpSeq b (e - 1) m ((mod (x * b) m) : x : xs)

getLastDigits :: Integer -> Int -> [Integer] -> [Integer]
getLastDigits n 0 xs = xs
getLastDigits 0 m xs = xs
getLastDigits n m xs = getLastDigits (n `div` 10) (m-1) ((n `mod` 10) : xs)

lastDigits :: Integer -> Int -> [Integer]
lastDigits n m = getLastDigits (sum (modExpSeq n n (10^(m + 5)) [])) m []

wrapper :: [String] -> [Integer]
wrapper (a:b:_) = lastDigits (read a::Integer) (read b::Int)

main =  print . wrapper . words =<< getLine
