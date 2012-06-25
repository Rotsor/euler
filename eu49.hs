import Prelude hiding (head, tail)
import Data.Numbers.Primes
import Data.List  hiding (head, tail)
import Data.Function
import Control.Monad

prims = takeWhile (<10000) . dropWhile (<1000) $ primes

tail (_ : t) = return t
tail _ = mzero

head (h : _) = return h
head _ = mzero

zz = do
  (p : ps) <- tails prims
  (p2 : ps') <- tails ps
  p3 <- head $ dropWhile (/=p2+p2-p) ps'
  guard $ all (((==) `on` (sort . show)) p) [p2, p3]
  return (p, p2, p3)
  
main = print zz