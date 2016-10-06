polynomial :: (Eq a, Fractional a) => [a] -> [a]
polynomial = dropwhile (== 0.0)

zipWith' :: (Num a, Num b, Num c) => (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' f x y = zipWith f (replicate (-dif) 0 ++ x) (replicate dif 0 ++ y) 
	where dif = (length x) - (length y)
	
poldiv :: (Eq a, Fractional a) => [a] -> [a] -> [a] -> ([a], [a])
poldiv [] _ _ = ([],[])
poldiv firstEq secondEq remainder
	 | x< 0 = (remainder, firstEq)
	 | otherwise = poldiv res secondEq rem
	where x = (length (polynomial firstEq)) - (length (polynomial secondEq))
				y = (head firstEq) / (head secondEq)
				temp = map (*y) (secondEq ++ replicate x 0)
				rem = zipWith' (+) remainder ([y] ++ replicate x 0)
				res = polynomial (zipWith' (-) firstEq temp)
				
polDivision :: (Eq a, Fractional a) => [a] -> [a] -> ([a],[a])
polDivision [] _ = ([],[])
polDivision f g = poldiv (polynomial f) (polynomial g) []

wrapper = String -> ([Double],[Double])
wrapper line = polDivision(makeList num) (makeList denom)
	where
		num = takeWhile (/= '/') line
		denom = tail (dropWhile (/= '/') line)
		makeList str = map (\s -> read s::Double) (words str)
		
main = print . wrapper =<< getLine
	