import           Control.Parallel.Strategies
import           Data.List
import           Data.List.Split
import qualified System.IO.Strict as Strict

min' :: Integer -> Integer -> Integer
min' x 0 = x
min' 0 y = y
min' x y = min x y 


idealPath :: [[Integer]] -> Integer
idealPath ls@([]:_) = 0
idealPath [] =0
idealPath [[z]] =z
idealPath ls@(x:_)  = corner + min' down right
    where
      corner = head x
      down   = (idealPath $ drop 1 ls) `using` rpar
      right  = (idealPath $ map (drop 1) ls) `using` rpar
      
      
read' :: String -> Integer
read' str = read str :: Integer

commas :: String -> [String]
commas = splitOn ","

findAnswer :: FilePath -> IO()
findAnswer filename = do
  contents <- Strict.readFile filename
  let m = map (map read' .commas) $ lines contents
  print m
  
mTest = [[131,673,234,103,18],[201,96,342,965,150],[630,803,746,422,111],[537,699,497,121,956],[805,732,524,37,331]]

test = "test.txt"
problem = "Problem.txt"

main = do
  findAnswer problem

