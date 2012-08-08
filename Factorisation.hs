module Factorisation where

import Control.Arrow
import Primes
import Data.List

factor _ 1 = []
factor (p : ps) x 
       | p * p > x = [x]
       | x `mod` p == 0 = p : factor (p : ps) (x `div` p)
       | otherwise = factor ps x

factor' :: (Integral a, Integral b) => a -> [(a, b)]
factor' = map (head &&& genericLength) . group . factor primes


factorisationsTo mx primes = [] : concat (neFactorisationsTo mx [] primes)

neFactorisationsTo mx prev [] = []
neFactorisationsTo mx prev (p : ps) | p > mx = []
 | otherwise = [(p, np) : r|(np,pp)<-zip [1..] (takeWhile (<=mx) (iterate (*p) p)),r<-factorisationsTo(mx `div` pp) (reverse prev)] : neFactorisationsTo mx (p : prev) ps

