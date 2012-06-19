import qualified Data.Set as Set
primes = 2 : filter (\cand -> all (\p -> cand `mod` p /= 0) $ takeWhile(\p -> p * p <= cand) primes) [3..]
fixx f x | y == x = y
  | otherwise = fixx f y where y = f x
circ (h:t) = t ++ [h]
answer = fixx (\s -> Set.intersection s (Set.map circ s)) (Set.fromList $ map show $ takeWhile (<1000000) primes)
