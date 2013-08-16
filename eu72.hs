import Data.Numbers.Primes
import Control.Monad
import Primes hiding (primes)
import Data.Ratio
import Data.List

factorisationsTo mx pss@(p : ps) | p > mx = [[]]
 | otherwise = map (p:) (factorisationsTo (mx `div` p) pss) ++ factorisationsTo mx ps

factorisationsTo' mx (p : ps) | p > mx = [[]]
 | otherwise = [(p, np) : r|(np,pp)<-zip [1..] (takeWhile (<=mx) (iterate (*p) p)),r<-factorisationsTo'(mx `div` pp) ps] ++ factorisationsTo' mx ps

--product' :: (Enum a, Num a) => [(a, Int)] -> [[a]]
product' = product . map (uncurry (^)) -- = mapM (\x -> [0..x-1]) . concatMap (\(x, c) -> replicate c x)

subs :: [(a, Integer)] -> [[a]]
subs = map concat . sequence . map (\(x, c) -> [genericReplicate i x|i<-[0..c]])


fracs dFact | denominator res == 1 = numerator res where
  res = fromIntegral (product' dFact) * product [(p-1)%p|p<-map fst dFact]

tst dFact = genericLength (filter ((==d) . denominator) [i % d|i<-[0..d-1]]) == fracs dFact where
  d = product' dFact

main = print $ subtract 1 $ sum $ map fracs $ factorisationsTo' (1000000 :: Integer) primes 