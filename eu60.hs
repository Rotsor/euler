{-# LANGUAGE DeriveFunctor #-}
import Data.Numbers.Primes
import System.IO
import Control.Arrow
import qualified Data.Map as Map
import Data.Map(Map)
import Data.PQueue.Min (minView, empty, insert)
import Data.Array(listArray, (!))
import Data.Tuple
import Data.Monoid
import Data.List (foldl', unfoldr)

import qualified Primes as P

newtype Ignore a = Ignore { unIgnore :: a } deriving Functor
instance Eq (Ignore a) where
 _ == _ = True
instance Ord (Ignore a) where
 compare _ _ = EQ

data Stream o e = Stream !o !(Maybe e) [Stream o e]

may2Int Nothing = 0
may2Int _ = 1

instance Eq o => Eq (Stream o e) where
  Stream a1 b1 _ == Stream a2 b2 _ = a1 == a2 && may2Int b1 == may2Int b2
instance Ord o => Ord (Stream o e) where
  compare (Stream a1 b1 _) (Stream a2 b2 _) = compare a1 a2 `mappend` compare (may2Int b1) (may2Int b2)

runStream :: Ord o => Stream o e -> [e]
runStream s = gos empty s where
  gos m (Stream o Nothing tls) = go $ foldr insert m tls
  gos m (Stream o (Just e) tls) = e : go (foldr insert m tls)
  go m = case minView m of
    Nothing -> []
    Just (s, mm) -> gos mm s

type BndStream o e = (o, Stream o e)
type PES o e = Maybe (Stream o e)

listToStream :: [a] -> PES a a
listToStream [] = Nothing
listToStream (h : t) = Just $ go h t where
  go h l = Stream h (Just h) $ case l of
    [] -> []
    (g : t) -> [go g t]

mergeL :: Stream o e -> PES o e -> Stream o e
mergeL s Nothing = s
mergeL (Stream k1 v1 t1) (Just s2) = Stream k1 v1 (s2 : t1)

merge :: Ord o => Stream o e -> Stream o e -> Stream o e
merge s1@(Stream k1 _ _) s2@(Stream k2 v2 t2)
  | k2 < k1 = Stream k2 v2 (s1 : t2)
  | otherwise = Stream k2 v2 (s1 : t2)

singleton :: a -> b -> Stream a b
singleton a b = Stream a (Just b) []

estimate :: a -> Stream a b -> Stream a b
estimate a s = Stream a Nothing [s]

myConcat a b | b >= 100 = go 1000
  | otherwise = go 10 where
  go n | b >= n = go (n*10)
       | otherwise = a * n + b

augment :: Int -> [Int] -> Int -> Maybe (Int, [Int])
augment s l p
  | all isPrime [myConcat a b|(a,b)<-[(x,p)|x<-l]++[(p,x)|x<-l]] = Just (s+p, p : l)
  | otherwise = Nothing

primeSets :: [Int] -> Int -> Stream Int [Int]
primeSets primes n = go primes n [] 0 where
  go _ 0 l s = singleton s l
  go (p : ps) n l s = (Stream $! (s+p+sum(take(n-1)ps))) Nothing $ (case augment s l p of
     Just (s', l') -> [go ps (n-1) l' s']
     Nothing -> []) ++ [go ps n l s]
main = do
  hSetBuffering stdout NoBuffering
  print $ sum $ map sum $ take 20 $ runStream $ primeSets primes 4 -- subsetsBy sum 4 primes

{-

naive:

mergeBy f as bs = map snd (P.merge (pr as) (pr bs)) where pr = map (\x -> (f x, x))
lMergeBy f (a : as) bs = a : mergeBy f as bs

subsetsBy f 0 l = [[]]
subsetsBy f n [] = []
subsetsBy f n (h : t) = lMergeBy f (map (h:) (subsetsBy f (n-1) t)) (subsetsBy f n t)

isGood l = and [isPrime $ read $ show a++show b|a<-l,b<-l, a/=b]

-}