import Data.List
import Data.Numbers.Primes


ans = maximum $ takeWhile (<10^6) $ products $ lastVals $ take 15 primes
      where
        products = map product
        lastVals = tail.inits
