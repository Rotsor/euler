import Control.Monad.State
import Control.Monad.RWS
import Control.Arrow
import Data.Traversable
import Data.Numbers.Primes
import Data.Ratio
import Data.List

diffs = [2,4..] >>= replicate 4
diffs' = map (replicate 4) [2,4..]

nums = scanl (+) 1 diffs

nums' = [1] : fst (runState (traverse (traverse (\x -> modify (+x) >> get)) diffs') 1)

go = (\(_, _, r) -> r) $ runRWS (mapM_ (\l -> modify ((+genericLength (filter isPrime l)) *** (+genericLength l)) >> get >>= tell . (:[])) nums') () (0,0)

main = print $ (+3) . (*2) . length $ takeWhile (>0.10) (tail $ map (uncurry (%)) go)