import Data.List 
import Data.List.Split (splitOn)
import Data.Ord (comparing)

type Edge = (Int,Int,Int)
 
type Network = [Edge]

shortestEdge :: Network -> Edge
shortestEdge net = minimumBy cond net 
    where
      cond (_,_,d1) (_,_,d2) = compare d1 d2 
      

minimalPath :: [Int] -> Network -> Int 
minimalPath _ [] = 0
minimalPath visited net 
    | length edges == 0    = 0
    | otherwise            = d + minimalPath newVisited net
    where
      -- First we find a list of edges leading to ynvisited vertices
      toUnvisited (x,y,_) = (x `elem` visited) && (y `notElem` visited)
      edges = filter toUnvisited net

      (a,b,d) = shortestEdge edges 
      newVisited = union visited [a,b]

readNet :: Int -> [[String]] -> Network
readNet _ [] = []
readNet x (t:ts) = (readLine 1 t) ++ readNet (x+1) ts
    where
      readLine _ [] = []
      readLine y ("-":ds) = readLine (y+1) ds
      readLine y (d:ds)  = (x,y, read d) : readLine (y+1) ds

commas = splitOn ","

originalSum :: [[String]]-> Int
originalSum [] = 0
originalSum (t:ts) = lineSum t + originalSum n
    where
      lineSum [] = 0
      lineSum ("-":xs) = lineSum xs
      lineSum (x:xs) = (read x) + lineSum xs

      n = map (drop 1) ts
                       
answer :: FilePath -> IO()
answer filename = do
  contents <- readFile filename
  let t = map commas $ lines contents
      net = readNet 1 t

  let minim = minimalPath [1] net
      ogSum = originalSum t
  print $ minim
  print $ ogSum
  print $ ogSum -minim
