module Primes (pairwise, ifold, ifoldInf, minus, merge, lMerge, primes) where

import Data.List
import Control.Monad
import Control.Arrow

minus (a : as) (b : bs) = case compare a b of
  LT -> a : minus as (b : bs)
  EQ -> minus as bs
  GT -> minus (a : as) bs
minus [] _ = []
minus as [] = as

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

data Bound a = MinBound | ExactBound a | MaxBound deriving (Ord, Eq)
data OrderedStream a = OrderedStream { osTakeWhileLeq :: Bound a -> ([a], (a, OrderedStream a)) }

below :: Ord a => Bound a -> a -> Bool
below bound x = bound >= ExactBound x

listToOS ~l@(h:t) = OrderedStream (\bound -> case span (below bound) l of ~(leq,~(gth:gtt)) -> (leq, (gth, listToOS gtt)))

consOS :: Ord a => a -> OrderedStream a -> OrderedStream a
consOS x s = OrderedStream (\y -> if below y x then first (x:) $ osTakeWhileLeq s y else ([], (x, s)))

{- mergeOSHeadsKnown :: Ord a => a -> OrderedStream a -> a -> OrderedStream a -> OrderedStream a
mergeOSHeadsKnown x xs y ys = OrderedStream takeWhile where

  takeWhile z = 
   let
    (xx, xxs) = case compare x y of
      LT -> (x, mergeOSHeadKnown y ys xs)
      EQ -> (x, mergeOS xs ys)
      GT -> (y, mergeOSHeadKnown x xs ys)
   in if xx <= z then first (xx:) (osTakeWhileLeq xxs z) else ([], (xx, xxs)) -}

osUncons os = snd $ osTakeWhileLeq os MinBound

prependOS lst str = OrderedStream takeWhile where
  takeWhile bound = case span (below bound) lst of 
    ~(leq,gt) -> first (leq++) $ case gt of
      gth : gtt -> ([], (gth, prependOS gtt str))
      [] -> osTakeWhileLeq str bound

mergeOSHeadKnown' :: Ord a => a -> OrderedStream a -> OrderedStream a -> OrderedStream a
mergeOSHeadKnown' x xs yys = case osTakeWhileLeq yys (ExactBound x) of
    ~(yi, ~(yth, ytt)) -> prependOS yi $ consOS x $ (mergeOSHeadKnown' yth ytt xs)

mergeOSHeadKnown :: Ord a => a -> OrderedStream a -> OrderedStream a -> OrderedStream a
mergeOSHeadKnown x xs yys = OrderedStream takeWhile where
 takeWhile z = case compare z (ExactBound x) of
     LT -> case osTakeWhileLeq yys z of 
        ~(yi, ~(yth, ytt)) | yth <= x -> (yi, (yth, mergeOSHeadKnown x xs ytt))
          | otherwise -> (yi, (x, mergeOSHeadKnown yth ytt xs))
     _ -> case osTakeWhileLeq yys (ExactBound x) of
        ~(yi, ~(yth, ytt)) -> first ((yi ++) . (x:)) $ osTakeWhileLeq (mergeOSHeadKnown yth ytt xs) z

mergeOS :: Ord a => OrderedStream a -> OrderedStream a -> OrderedStream a
mergeOS xxs yys = case osUncons xxs of
    ~(x, xs) -> mergeOSHeadKnown x xs yys

mergeOSL xxs yys = case osUncons xxs of
  ~(x, xs) -> consOS x (mergeOS xs yys)

toList :: OrderedStream a -> [a]
toList os = fst $ osTakeWhileLeq os MaxBound
tst1 = (take 999 $ toList (listToOS [1..])) == [1..999]
tst2 = (take 100 . toList) (mergeOS (listToOS [1,3..]) (listToOS [2,4..])) == [1..100]

primesNew = 2 : 3 : minus [5,7..] (lMerge (foldr lMerge [] [[p*p,p*p+p+p..]|p<-[3,5,7,11,13]]) $ toList (foldr mergeOSL undefined [listToOS [p*p,p*p+p+p..]|p<-dropWhile (<=13) primes]))
primesNew' = 2 : 3 : minus [5,7..] (toList (foldr mergeOSL undefined [listToOS [p*p,p*p+p+p..]|p<-tail primes]))
primesNewOld = 2 : 3 : minus [5,7..] (foldr lMerge undefined [[p*p,p*p+p+p..]|p<-tail primes])

-- main = print $ foldl' (+) 0 $ takeWhile (<= 20000000) (primes)
main = print $ sum $ take 100000 primesNew'
