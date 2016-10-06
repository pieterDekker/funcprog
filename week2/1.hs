primes :: [Integer]
primes  =  sieve [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p:xs)      =  p : sieve [x | x <- xs, x `mod` p /= 0]

isPrime :: Integer -> Bool
isPrime n             = n == head ( dropWhile (<n) primes )

primeFactors :: Integer -> [Integer] -> [Integer]
primeFactors n (x:xs)
  | x > n             = []
  | n `mod` x == 0    = x : primeFactors (n `div` x) (x:xs)
  | otherwise         = primeFactors n xs

compSieve :: [Integer] -> [(Integer,[Integer])]
compSieve (x:xs)
    | isPrime x     = compSieve xs
    | otherwise     = (x, primeFactors x primes) : compSieve xs

-- Insert your own code here.
composites :: [(Integer,[Integer])]
composites = compSieve [4..]


