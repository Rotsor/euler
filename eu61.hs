import Search
import Control.Monad
import Data.List hiding (find)

polygonal p n = n * ((p - 2) * n - (p - 4)) `div` 2 
-- poly p n = n * (p * n - p + 2) / 2
-- poly p n = p * n * (n - 1 + 2/p) / 2
-- poly p n = p * (n + q - q) * (n + q + q) / 2 where q = 2/p-1
-- = p * ((n+q)^2 - q^2) / 2
-- 

-- | `unPoly p x` is maximum n such that `polygonal p n <= x`
unPoly :: Integer -> Integer -> Integer
unPoly p x = esearch ((>x) . polygonal p) 0

polyInRange p min max = [polygonal p n|n<-[start..end]] where
  minp = unPoly p min
  end = unPoly p max
  start | polygonal p minp == min = minp
        | otherwise = minp + 1

split = (`divMod` 100)
merge a b = a * 100 + b

find :: [Integer] -> [[Integer]]
find (p : ps) = polyInRange p 1000 9999 >>= \a -> map (a:) $ case split a of
  (first, second) -> guard (second > 9) >> go second ps where
    go :: Integer -> [Integer] -> [[Integer]]
    go pref [p] = let r = merge pref first in polyInRange p r r >> [[r]]
    go pref (p : ps) = polyInRange p (merge pref 10) (merge pref 99) >>= \x -> map (x :) (go (snd $ split x) ps)

main = print $ sum $ head $ map (8:) (permutations [3..7]) >>= find