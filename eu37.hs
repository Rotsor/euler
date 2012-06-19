import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Arrow
primes :: [Integer]
primes = 2 : filter (\cand -> all (\p -> cand `mod` p /= 0) $ takeWhile(\p -> p * p <= cand) primes) [3..]
fixx f x | y == x = y
  | otherwise = fixx f y where y = f x
primeMap = Map.fromList $ map ((\p -> (p, Set.singleton p)) ) $ ("":) $ map show $ takeWhile (<1000000) primes
fff d = head $ (map snd)$ Map.toList $ fixx (\s -> Map.intersection (Map.fromListWith (Set.union) . map (first d) . Map.toList $ s) primeMap) primeMap
lefts = fff (drop 1)
rights = fff (reverse . drop 1 . reverse)
answer = sum $ map read $ filter ((>1).length) $ Set.toList $ Set.intersection lefts rights
