import Data.Ratio
import Data.List
import Data.Numbers.Primes
r d = genericLength[0|x<-[1..d-1],gcd x d==1]%(d-1)
solution = [d|d<-[2..],r d<15499%94744]
rr :: Integer -> Integer -> Rational
rr i j = foldl (\a n -> a * ((n-1)%fromIntegral n)) 1 pl * (product pl * j % (product pl * j - 1)) where
  pl = genericTake i primes
main = print $ product (take 9 primes) * 4
-- manually bsearched arguments to find (9,4) the best pair with rr 9 4 < 15499%94744