import Totient
import Factorisation
import Data.List
import Data.Ord
import Primes

main = print $ last $ takeWhile (<= 10^6) $ scanl (*) 1 primes
