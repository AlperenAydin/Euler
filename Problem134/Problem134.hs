import qualified Control.DeepSeq as D
import Control.Exception
import Control.Parallel.Strategies
import Data.Numbers.Primes as P


deep :: D.NFData a => a -> a
deep a = D.deepseq a a

pwrOf10 = 10 : next pwrOf10
    where
      next (x:xs) = 10*x : next xs

findConnection :: Integer -> Integer -> Integer 
findConnection p1 p2 = connect p2
    where
      (x:_) = dropWhile (<p1) pwrOf10
      connect s = if s `mod` x == p1 then s else connect (s+p2)

prm1 :: [Integer]
prm1 = dropWhile (<5) p
    where p = takeWhile (<10^6) primes


prm2 = take l p 
    where
      l = length prm1
      p = dropWhile (<6) primes
         
connections = l `using` parList rseq
    where
      l = zipWith findConnection prm1 prm2


main :: IO()
main = 
    do 
      l <- evaluate $ deep $ connections
      print (sum l)
