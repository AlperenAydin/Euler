import Data.Numbers.Primes


sumOfprmBelow n = sum $ takeWhile (<n) primes

main :: IO ()
main = do 
  n <- getLine
  print $ sumOfprmBelow (read n :: Int)
