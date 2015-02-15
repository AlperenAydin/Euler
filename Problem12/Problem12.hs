import Data.List
import Data.Numbers.Primes

prmFct :: Integer -> [Integer]
prmFct x = factor x primes
    where
      factor _ [] = []
      factor 1 _  = []
      factor k (p:ps) = if k `mod` p == 0
                        then p: factor (k `quot` p) (p:ps)
                        else factor k ps

divisor :: Integer -> Int
divisor x = product $ map ((1+).length) $ group $ prmFct x


divisorLess :: Int -> Integer -> Bool
divisorLess n x = (divisor x) < n


tri = 1 : next 2 tri
      where
        next n (x:xs) = (n+x):next (n+1) xs
      
