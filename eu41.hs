import Data.Numbers.Primes
import Data.List

isPandi n = sort (show n) == take (length (show n)) ['1'..]

answer = filter isPandi (takeWhile (<10^9) primes)