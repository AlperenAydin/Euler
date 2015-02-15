import Control.Parallel.Strategies
import Data.List
import Data.List.Split

commas = splitOn ","

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

disjoint _ [] = True
disjoint l (x:xs) = if x `elem` l then False else disjoint l xs


cond1 [] = True
cond1 (l:ls) = if h then cond1 ls else False 
    where
      h = (all (/= sum l) $ (map sum) $ filter (disjoint l) ls) 

cond2 s = all f [1.. l `div` 2]
    where
      o = sort s
      l = length o 
      f x = sum h > sum t 
          where
            h = take (x+1) o
            t = drop (l-x) o

specialSumSet l = c1 && c2
    where
      c1 = cond1 $ subsets l
      c2 = cond2 l

prob105 :: [[Int]] -> Int
prob105 [] = 0
prob105 (x:xs) = s + prob105 xs
    where
      s = (if specialSumSet x then sum x else 0) `using` rpar 

readLine :: String -> [Int]
readLine l = read $ "[" ++ l ++ "]"

problem105 :: IO()
problem105 = do
  contents <- readFile "sets.txt"
  let s = map readLine $ lines contents
      s'= chunksOf 10 s
      ans = (map prob105 s') `using` parList rdeepseq
  print $ sum ans


main :: IO()
main = do
  problem105 
