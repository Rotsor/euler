import Data.Ratio
import Data.List
import Primes

totient factors | denominator res == 1 = numerator res where
  res = fromIntegral (product' factors) * product [(p-1)%p|p<-map fst factors]

newtype Ignore a = Ignore { unIgnore :: a }
instance Eq (Ignore a) where
  x == y = True
instance Ord (Ignore a) where
  compare a b = EQ

mergeBy f as bs = unmf $ merge (mf as) (mf bs) where
  mf = map (\x -> (f x, Ignore x))
  unmf = map (unIgnore . snd)
mrgL f (a : as) bs = a : mergeBy f as bs

-- f must be monotonic: 
-- 1. f t <= f (h : t)
-- 2. x <= y -> f (x : t) <= f (y : t)
-- 3. t <= s -> f (x : t) <= f (x : s)
-- List must be non-decreasing. It may be infinite though.
neSubsetsWith f (h : t) = mrgL f (map (h:) (subsetsWith f t)) (neSubsetsWith f t)
subsetsWith f l = [] : neSubsetsWith f l

factorisationsTo mx primes = [] : concat (neFactorisationsTo mx [] primes)

neFactorisationsTo mx prev [] = []
neFactorisationsTo mx prev (p : ps) | p > mx = []
 | otherwise = [(p, np) : r|(np,pp)<-zip [1..] (takeWhile (<=mx) (iterate (*p) p)),r<-factorisationsTo(mx `div` pp) (reverse prev)] : neFactorisationsTo mx (p : prev) ps

preservation = product . map (\p -> (p - 1) % p)

primeSets = subsetsWith (recip . preservation) (reverse $ takeWhile (<10^7) primes)

product' = product . map (uncurry (^))

main = print $ snd $ minimum $ [(n % totient nf, n)|nf<-factorisationsTo (10^7-1) primes, let n = product' nf, (sort $ show $ totient nf) == (sort $ show n)]
