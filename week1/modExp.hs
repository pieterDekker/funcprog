modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m 
  | (e `mod` 2) == 0  = (x b e m)
  | otherwise       = mod ((x b e m) * (mod b m)) m
    where x b e m = mod ((mod (modExp b (e `div` 2) m) m) * (mod (modExp b (e `div` 2) m) m)) m
