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


{- factorisationsTo :: Integral p => p -> [p] -> [[(p,Integer)]]
factorisationsTo mx primes = [] : neFactorisationsTo mx [] primes


neFactorisationsTo :: Integral p => p -> [p] -> [p] -> [[(p, Integer)]]
neFactorisationsTo mx prev [] = []
neFactorisationsTo mx prev (p : ps) | p > mx = []
 | otherwise =
  [(p, np) : r|(np,pp)<-zip [1..] (takeWhile (<=mx) (iterate (*p) p)),r<-factorisationsTo(mx `div` pp) (reverse prev)] ++ neFactorisationsTo mx (p : prev) ps -}


-- factorisationsTo :: Integral p => p -> [p] -> [[(p,Integer)]]
factorisationsTo mx primes = [] : neFactorisationsTo mx primes 0 primes

factorisationsTo' mx primes p_c = [] : neFactorisationsTo mx primes 0 (take p_c primes)

-- neFactorisationsTo :: Integral p => p -> [p] -> [p] -> [[(p, Integer)]]
neFactorisationsTo mx all_p all_p_c [] = []
neFactorisationsTo mx all_p all_p_c (p : ps) | p > mx = []
 | otherwise =
  [(p, np) : r|(np,pp)<-zip [1..] (takeWhile (<=mx) (iterate (*p) p)),r<-factorisationsTo'(mx `div` pp) all_p all_p_c] ++ neFactorisationsTo mx all_p (all_p_c + 1) ps
