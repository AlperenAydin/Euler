import Control.Exception
import Control.Parallel.Strategies
import Data.Numbers.Primes
-- Prm = Prime
-- Fc = Factor

smallestPrmFc :: Integer -> Integer
smallestPrmFc 1 = 0
smallestPrmFc 0 = 0
smallestPrmFc k = smallestFcIn primes 
    where
      smallestFcIn [] = k
      smallestFcIn (x:xs) = if k `mod` x == 0 then x else smallestFcIn xs


arithDerivative :: Integer -> Integer
arithDerivative p 
    | isPrime p = 1
    | otherwise = a*b' + b -- Actually ab'+a'b, but here a' =1
    where 
      a  = smallestPrmFc p
      b  = p `quot` a
      b' = arithDerivative b


termOfSum :: Integer -> Integer
termOfSum k = gcd k k'
    where
      k' = arithDerivative k


applyIntervalSum ::(Integer -> Integer) ->
                   Integer ->
                   Integer ->
                   Eval Integer
applyIntervalSum (f) v_min v_max 
    | v_min > v_max    = applyIntervalSum (f) v_max v_min
    | (v_min == v_max) = return (f v_max)
    | otherwise        = do 
  x <- rpar (f v_min)
  y <- applyIntervalSum (f) (v_min+1) v_max
  return (x+y)
 
           
applyIntervalSumStep :: (Integer -> Integer) ->
                        Integer ->
                        Integer ->
                        Integer ->
                        Eval Integer 
applyIntervalSumStep f v_min v_max step 
    | v_min == v_max          = return 0
    | (v_min + step) < v_max  = a1 
    | otherwise               = a2
    where a1 = do
            x <- applyIntervalSum f v_min (v_min+step-1)
            y <- applyIntervalSumStep f (v_min+step) v_max step 
            return (x+y)
          a2 = do
            x <- applyIntervalSum f v_min v_max
            return x
            
  
main :: IO ()
main = do
  print $ runEval $ applyIntervalSumStep id 0 (5*10^15) 100
