superZip :: (Int -> Int -> [Int]) -> [Int] -> [Int] -> [Int]
superZip f (x:xs) (y:ys) = (f x y) ++ (superZip f xs ys)

put :: Int -> Int -> [Int]
put x 0 = []
put x n = x : put x (n-1)

helpseq :: [Int]
helpseq = 1 : map (\n -> 3 - n) helpseq

selfre :: [Int]
selfre = 1 : 2 : 2 : superZip put helpseq (tail (tail selfre))
