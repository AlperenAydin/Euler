import qualified System.IO.Strict as Strict

bottomUp :: (Num a, Ord a) => [a] -> [a] -> [a]
bottomUp [] [z] = []
bottomUp (x:xs) (y:z:zs) = (x + max y z) : bottomUp xs (z:zs)

getTree :: String -> [[Integer]]
getTree str= map (map read .words) $ lines str


solveFile :: FilePath -> IO()
solveFile file = do
  contents <- Strict.readFile file
  let tree = getTree contents
      ans  = head $ foldr1 bottomUp tree
  print ans
