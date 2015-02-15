import Data.List

target :: Int
target = 2 * 10^6

nSquares :: (Int,Int) -> Int
nSquares (x,y) = (x^2 +x)*(y^2 +y) `div` 4

erreur :: (Int,Int) -> Int
erreur (x,y) = abs $ nSquares (x,y) -target

coord l= foldl f [] l
    where
      f s x = s ++ map (\ y -> (x,y)) l


ans = minimumBy cond coords
    where
      coords = coord [1..100]
      cond x1 x2  = compare (erreur x1) (erreur x2)  
