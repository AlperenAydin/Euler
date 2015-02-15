-- A few functions to check if for primality
isqrt :: Int -> Int
isqrt n = findIntSqrt n
    where
      findIntSqrt x 
          | x*x > n   = findIntSqrt (x-1) 
          | otherwise = x


isPrime k = null [x| x<- [2 .. isqrt k] , k `mod` x == 0] 

--primes = [x | x<- [2..] , isPrime x]
primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
--
rmFactor :: Int -> Int -> Int
rmFactor k factor = if k `mod` factor == 0 
                    -- We divide the number by the factor if we can
                        then rmFactor (quot k factor) factor 
                    -- If we can't it means that the factor is removed
                        else k                                   



largestPrimeFactor :: Int -> Int
largestPrimeFactor x = shaveFactors x primes
    where
      shaveFactors x (fx:f)  
          | rmQuot == 1 = fx
          | otherwise = shaveFactors rmQuot f
          where rmQuot = rmFactor x fx

