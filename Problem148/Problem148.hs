pascal :: Integer -> [[Integer]]
pascal n = [1] :rest[1]
    where
      nextRow ls = 1:next ls
          where
            next (x:[]) = [1]
            next (a:xs@(b:_)) = ((a+b) `mod` n):next xs

      rest ls = (r) : rest r
                where
                  r = nextRow ls


showTableIf :: (a-> Bool) -> [[a]] -> String
showTableIf _ [] = []
showTableIf f (l:ls) = showList l ++ "\n" ++ showTableIf f ls
    where
      showDiv k = if f k then "0" else "1" 
      showList [] = []
      showList (x:xs) = showDiv x ++" "++ showList xs


sierpinski n = putStr $ showTableIf (==0) $ take n $ pascal 7

-- The following is based on the previous things
-- We see that divisiblity by 7 of the pascal triangle 
-- makes a sierpinski triangle

-- The smallest solid triangle has an area of 28 
-- Meaning that there are 28 numbers not divisible by 7
-- After this, this motif repeated 28 times 
-- The new motif is then repeated 28 times again.
-- And so on , and do forth. 
-- This means that  7^k, we have 28^k non-divisble numbers

-- If we get a number not a power of 7:
-- We would only have to find the closest (inferieur) power of 7 
-- then the rest.

tri 0 = 0
tri n 
    |n <7 =n+tri (n-1)  
    |n==p7 =28^p -- Proper power of 7
    |otherwise=(tri i) + j*(tri (n-i))
    where
      i=p7*((n-1)`div`p7)
      j= -(n`div`(-p7))
      p7=7^p                                 -- Closest power of 7
      p=floor . logBase 7 . fromIntegral $ n -- Indice of closest power of 7

