
import Control.Parallel.Strategies
import Data.List
import Data.List.Split
import Data.Numbers.Primes

fact n = map head $ group $ primeFactors n


totient n =  floor $ foldl (\ pr p -> pr*(1-1/p)) n' ps  
    where
      ps = (map fromIntegral $ fact n) :: [Double]
      n' = fromIntegral n


answer = sum lk
    where
      ls = chunksOf 100 [2..10^6]
      s l= foldl (\s d -> s+totient d) 0 l 
      lk = (map s ls) `using` parList rseq

main = do
  print answer

mod' x y = x' `mod` y'
    where
      x' = floor x
      y' = floor y

multDiv :: Double -> [Double] -> [Double]
multDiv p [] = []
multDiv p (x:xs) = (if x `mod'` p ==0 then x*m else x):multDiv p xs
    where
      m = 1 -1/p

totient' :: [Double]
totient' = tot [2.0 ..]
          where
            tot :: [Double] -> [Double]
            tot (p:ps) 
                | isPrime (floor p) = (p-1):tot (multDiv p ps)
                | otherwise = p : tot ps
