import Data.Ratio
s bound denom = case properFraction (bound * fromIntegral denom) of
  (x, 0) -> (x-1) % denom
  (x, _) -> x % denom
solve bound maxDenom = numerator $ maximum $ map (s bound) [1..maxDenom]
main = print $ solve (3 % 7)1000000 
