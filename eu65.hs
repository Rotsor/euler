import Data.Ratio
import Data.List
import Control.Monad
eexpansion = 2 : (>>= \x -> [1,x,1]) [2,4..]
convergence (h : t) = fromIntegral h : map ((fromIntegral h+) . recip) (convergence t)
digits = unfoldr (\x -> guard (x > 0) >> Just (x `mod` 10, x `div` 10))
sdnc n = sum . digits . numerator $ convergence eexpansion !! n
main = print $ sdnc 99
