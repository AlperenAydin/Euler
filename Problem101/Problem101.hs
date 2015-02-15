fI :: Integer -> Double
fI = fromIntegral


-- Lagrange intrepolation
p :: [(Integer,Integer)] -> Integer -> Integer
p l x = sum $ map (pj x) l
    where
      pj x (xj,yj) = round $ (fI yj) * foldl f 1 l
          where
            f p (xk,_) = if xj/= xk 
                         then p * (fI (x-xk) /  fI (xj-xk))
                         else p



bOP ::(Integer -> Integer) -> Integer -> Integer-> Integer
bOP u k = p l
    where
      l= map (\ n -> (n,u n)) [1..k]

fIT :: (Integer -> Integer) -> Integer -> [Integer] 
fIT u k = map (\ n -> bOP u n (n+1)) [1..k]
 

u :: Integer -> Integer
u n = 1-(n)+(n^2)-(n^3)+(n^4)-(n^5)+(n^6)-(n^7)+(n^8)-(n^9)+(n^10)

