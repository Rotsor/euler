module Primes (pairwise, ifold, ifoldInf, minus, merge, lMerge, primes) where

import Data.List

minus (a : as) (b : bs) = case compare a b of
  LT -> a : minus as (b : bs)
  EQ -> minus as bs
  GT -> minus (a : as) bs

merge (a : as) (b : bs) = case compare a b of
  LT -> a : merge as (b : bs)
  EQ -> a : merge as bs
  GT -> b : merge (a : as) bs
merge [] bs = bs
merge as [] = as

lMerge (a : as) bs = a : merge as bs

pairwise f (a : bt) = rh : rt where
  ~(rh, rt) = case bt of
    [] -> (a, [])
    (b : t) -> (f a b, pairwise f t)
pairwise f [] = []

ifold f def [] = def
ifold f def (h : t) = f h . ifold f def . pairwise f $ t

ifoldInf f ~(h : t) = f h . ifoldInf f . pairwise f $ t

mkP p = 2 : 3 : minus [5,7..] (ifoldInf lMerge (map (\p -> [p*p, p*p+2*p..]) (tail p)))

primes1 = mkP primes1
primes = mkP primes1

main = print $ foldl' (+) 0 $ takeWhile (<= 20000000) (primes)
