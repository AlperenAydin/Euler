import Data.List

limit :: Double
limit = 1000

isInt x = (x == fromInteger (round x) && x>0)

isIntPos (x,y,z) = (isInt x && isInt y && isInt z) && (x> 0 && y>0 && z>0)

find_ab :: Double -> Double -> [(Double,Double,Double)]
find_ab d c 
    | delta < 0  =  []
    | otherwise  =  filter isIntPos [(a1,b1,c),(a2,b2,c)]
    where
      s = d-c
      p = (d^2)/2 -d*c
      delta = s^2 -4*p
      a1 = (s + sqrt(delta))/2
      b1 = s-a1
      a2 = (s + sqrt(delta))/2
      b2 = s-a2

verify = filter total list
    where
      sumTuple (a,b,c) = (a+b+c==1000)
      isPythagorian (a,b,c) = (a^2 +b^2 == c^2)
                              
      total p = sumTuple p && isPythagorian p

      list =  foldl union [] $ map (find_ab limit) [1..1000]

answer = map (\ (a,b,c) -> ((a,b,c),a*b*c)) l
    where
      l = foldl union [] $ map (find_ab limit) [1..limit]


 
