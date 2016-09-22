  replace :: Integer -> Integer
  replace n
    | n == 0      = 0
    | otherwise   = ((mod n 10) ^ 2) + replace (div n 10)

  list = [4,16,37,58,89,145,42,20]

  isHappy :: Integer -> Bool
  isHappy n
    | n == 1       = True
    | n ` elem` list = False
    | otherwise   = isHappy (replace n)
    
  countHappyNumbers2 :: Integer -> Integer -> Integer
  countHappyNumbers2 a b = sum [1 | x <- [a..b], isHappy x]
