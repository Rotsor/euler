import Data.Ratio
import Data.Function

approxs (c : cs) = c : map ((c+) . recip) (approxs cs)

main = print $ length . filter (\x -> ((>) `on` (length . show)) (numerator x) (denominator x)) $ take 1000 (approxs (1 : repeat 2))