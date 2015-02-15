import Control.Exception
import Control.Parallel.Strategies
import Data.List

isPalindrome :: Integral a => a -> Bool
isPalindrome x = (x == symmetric) 
    where
      listDigits k = if k == 0 then [] else (k `mod` 10):listDigits (k `quot` 10)
      decimal [] = 0
      decimal (k:ks) = k + 10*decimal ks
          
      symmetric = decimal $ reverse $ listDigits x


answer n = take 10 products
    where
      lowerLimit = 10^(n-1) 
      upperLimit = (10^n) -1
      nDigit = [upperLimit, upperLimit-1 .. lowerLimit]
      palindromeProduct x = filter isPalindrome $ map (x*) nDigit
      products = foldl union [] $ map palindromeProduct nDigit
