import           Control.Parallel.Strategies
import qualified System.IO.Strict as Strict


main :: IO ()
main = do
  contents <- Strict.readFile "numbers.txt"
  let list :: [Integer]
      list = map read $ lines contents
      answer = take 10 $ show $ sum list
  putStrLn answer
