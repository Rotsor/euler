import Data.Char
import Primes hiding (main)
import Search

preciseSqrt n = esearch check 0 where
  check d = d * d > n * 10^198

main = print $ sum $ map (sum . map digitToInt . show . preciseSqrt) $ minus [1..100] [x^2|x<-[1..10]]