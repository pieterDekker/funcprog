  replace :: Integer -> Integer
  replace n
    | n == 0      = 0
    | otherwise   = ((mod n 10) ^ 2) + replace (div n 10)

  isHappy :: Integer -> Bool
  isHappy n
    | n == 1      = True
    | n == 4      = False
    | n == 16      = False
    | n == 37      = False
    | n == 58      = False
    | n == 89      = False
    | n == 145      = False
    | n == 42      = False
    | n == 20      = False
    | otherwise   = isHappy (replace n)

  countHappyNumbers :: Integer -> Integer -> Integer
  countHappyNumbers a b
    | a == b      = 0
    | isHappy a   = 1 + countHappyNumbers (a + 1) b
    | otherwise   = 0 + countHappyNumbers (a + 1) b
