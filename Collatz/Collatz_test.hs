collatz :: Int -> [Int] 
collatz a0 = a0: (next a0)
    where 
      next a = ai : next ai
               where ai = if even a then quot a 2 else 3*a+1



