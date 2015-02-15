fib :: [Int]
fib = 1:2: next fib
    where 
      next (a: t@(b:_)) = (a+b): next t

-- Gives all the fibonacci numbers under limit
fibUnder :: Int -> [Int]
fibUnder limit= takeWhile (<limit) fib

-- Returns all the terms of list which verity 'condition'
takeIf :: (a->Bool) -> [a] -> [a]
takeIf condition list = foldl func [] list
    where
      func l x = l ++ (if condition x then [x] else [])


answer = sum $ takeIf even $ fibUnder (4*10^6)
      
