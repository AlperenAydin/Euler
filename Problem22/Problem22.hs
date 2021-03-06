import Data.Char (ord)
import Data.List (sort,elemIndex,transpose)
import Data.List.Split (splitOn)

alphaVal :: String -> Int
alphaVal [] = 0
alphaVal (x:xs) = (ord x -64)+alphaVal xs

rm :: Eq a => a -> [a] -> [a]
rm _ [] = []
rm l (x:xs) = if x == l then rm l xs else x:rm l xs 


answer _ [] = 0
answer n (str:lstr) = (n*alphaVal str)+answer (n+1) lstr

commas = splitOn ","


main :: IO()
main = 
    do 
      contents <- readFile "names.txt"
      let l = sort.commas $ rm '"' contents
      print $ answer 1 l
