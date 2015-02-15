import GHC.IO.Handle
import GHC.IO.Handle.FD

getDigits :: String -> [Integer]
getDigits [] = []
getDigits (x:xs) = (read [x] :: Integer) :(getDigits xs)


prdctOfConsecutiveDigits :: Int -> [Integer] -> [Integer]
prdctOfConsecutiveDigits n list 
    | l < n  = []
    | otherwise = prdct : (prdctOfConsecutiveDigits n xs)
    where
      l       = length list
      (x:xs)  = list
      digits  = take n list
      prdct   = product digits


arrangeContents :: String -> [Integer]
arrangeContents str = foldl (++) [] ls
    where
      ls = map getDigits $ lines str


answer n str = maximum $ prdctOfConsecutiveDigits n l
    where 
      l = arrangeContents str



main :: IO ()
main = do
  str <- readFile "number.txt"
  print "No of Digits"
  n <- getLine
  print $ answer (read n :: Int) str

