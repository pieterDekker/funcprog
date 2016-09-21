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

  countHappyNumbers :: Integer -> Integer -> Integer
  countHappyNumbers a b
    | a > b      = 0
    | isHappy a   = 1 + countHappyNumbers (a + 1) b
    | otherwise   = 0 + countHappyNumbers (a + 1) b
    
  countHappyNumbers2 :: Integer -> Integer -> Integer
  countHappyNumbers2 a b = sum [1 | x <- [a..b], isHappy x]

  countHappyNumbers3 :: Integer -> Integer -> Int
  countHappyNumbers3 a b = length [x | x <- [a..b], isHappy x]
