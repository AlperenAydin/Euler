import Data.List (delete)
import Data.Maybe (fromJust)

takeIf :: (a-> Bool) -> [a] -> [a]
takeIf f [] = []
takeIf f (x:xs) = (if f x then x:takeIf f xs else takeIf f xs)

properDivisors :: Integer -> [Integer]
properDivisors a = takeIf divisor nmLess
    where
      nmLess = [1.. (a`div`2)]
      divisor n = a `mod` n ==0

d :: Integer -> Integer
d = sum.properDivisors

isAmicable :: Integer -> Maybe Integer
isAmicable x 
    | (x == x') && (y /= x) = Just y
    | otherwise             = Nothing
      where
        y = d x
        x' = d y

amicable :: [Integer]
amicable = listAmicable [1..10^4]
    where 
      listAmicable [] = []
      listAmicable (x:xs)
          | ym == Nothing = listAmicable xs
          | otherwise     = x:y:listAmicable (delete y xs) 
          where
            ym= isAmicable x
            y = fromJust ym

