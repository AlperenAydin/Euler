import Control.Parallel.Strategies
import Data.List
import Data.List.Split (chunksOf)

collatz :: Integer -> [Integer]
collatz n = n:next n 
    where
      next 1 = []
      next x = ans : next ans
          where
            ans = if even x 
                  then x `quot`2
                  else 3*x+1


collatzLen :: Integer -> Integer
collatzLen 1 = 1
collatzLen x = 1 + collatzLen next
    where
      next =if even x then x `div` 2 else 3*x+1
 
collatzTup :: Integer -> (Integer,Integer)
collatzTup x = (x,collatzLen x)      

biggerList (x,y) (a,b) = if y<b then (a,b) else (x,y)

answer :: Int -> (Integer,Integer)
answer n = foldr biggerList (1,1) g
    where 
      l = chunksOf 50 [1..10^n]
      f = foldr (biggerList.collatzTup) (1,1) 
      g = (map f l) `using` parList rseq

main :: IO()
main = do
  c <- getLine
  print $ answer $ (read c :: Int)
