import Data.Tuple
import Data.List
import Control.Monad
import Data.Array

decimal = unfoldr (\x -> guard (x > 0) >> Just (swap $ x `divMod` 10))
solve n = length $ filter (==89) $ elems arr where
  arr = array (1, n) $ [(1, 1), (89, 89)] ++ [(x, arr ! sum (map (^2) (decimal x)))|x<-[2..n],x/=89]
main = print $ solve (10^7-1)