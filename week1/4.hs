chop :: Integer -> Integer -> Integer
chop a m = a `mod` (10 ^ m)

lastDigit :: Integer -> Integer -> Integer
lastDigit n m = chop (sum [n^x | x <- [0,1..n]]) m
