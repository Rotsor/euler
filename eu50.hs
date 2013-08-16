import Primes hiding (minus)
import Data.List hiding (intersect)
import Data.Ord

primeSums = ifoldInf lMerge (map (tail . scanl (\(a,c) q->(q+a,c+1)) (0,0)) (tails primes))

intersect css@((c, r) : cs) pss@(p : ps) = case compare c p of
  LT -> intersect cs pss
  EQ -> (c, r) : intersect cs pss
  GT -> intersect css ps
intersect [] _ = []
intersect _ [] = []

main = print $ maximumBy (comparing snd) . (`intersect` primes) . takeWhile ((<1000000) . fst) $ primeSums
