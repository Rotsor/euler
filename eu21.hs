import Data.Tuple
import Control.Arrow
import qualified Data.Map as Map
import Data.Map ((!))

d x = sum $ filter (\y -> x `mod` y == 0) [1..x-1]

-- divisors = Map.fromListWith (++) $ map (second return) $ map swap $ concat $ map (\i -> map ((,) i) $ takeWhile (<10000) (map (*i) [2..])) [1..10000]

isAmicable x = let y = d x in y /= x && d y == x

result = filter isAmicable [1..10000]