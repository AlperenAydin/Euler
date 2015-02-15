import Data.List

limit = 28123
factors n = [x| x <- [1..n-1], n `mod` x == 0]

isAbundant n = s > n
    where
      s= sum $ factors n

abundants = filter isAbundant [1.. limit]

mapSum' :: Int -> [Int] -> [Int]
mapSum' _ [] = []
mapSum' s (x:xs) 
    | x +s > limit  = []
    | otherwise     = (s+x):mapSum' s xs

allSums :: [Int] -> [Int]
allSums [] = []
allSums (x:xs) = (mapSum' x xs) ++ allSums xs 

allAbundantSums = allSums abundants

ans = [1..limit] \\  allAbundantSums
