{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
import Prelude
import Factorisation
import Primes
import Data.List
import Data.Ord
import Control.Arrow
import Data.Int
import Data.Monoid
import Fold
import qualified Data.Map as Map
import qualified Data.FingerTree as FT
import Data.FingerTree((><),ViewL(..),(<|))

newtype Mod = Mod Int64 deriving (Eq, Show)

mm :: Integral x => x -> x
mm x = x `mod` 1000000993
instance Num Mod where
  Mod a + Mod b = Mod $ mm (a + b)
  Mod a * Mod b = Mod $ mm (a * b)
  fromInteger x = Mod (fromInteger $ mm x)
instance Ord Mod where
instance Enum Mod where
instance Real Mod where
instance Integral Mod where
  toInteger (Mod x) = toInteger x

facts sz = factorisationsTo sz primes

binomRatios sz = let f = map snd $ sort $ map (\l -> (product . map (uncurry (^))$ l, l)) $ facts sz in zip (reverse f) f

data Measure = Measure
               !Int {- prime -}
               !Mod {- accumulated sum up to this prime (not inclusive) -}
               !Mod {- accumulated product up to this prime (inclusive) -}
               deriving Show
instance Monoid Measure where
  Measure c1 s1 p1 `mappend` Measure c2 s2 p2 = Measure (c1 + c2) (s1 + p1 * s2) (p1 * p2)
  mempty = Measure 0 0 1

data Node = Node !Int {- prime -} !Int {- previous prime or 0 -} !Int {- prime power -} deriving Show

instance FT.Measured Measure Node where
  measure (Node p p0 pow) = Measure (p - p0) (fromIntegral $ p - (if p0 == 0 then 1 else p0)) (fromIntegral p ^ pow)

manyPrimes cut =
  FT.fromList ((\ps ->
                 Node 2 0 0 :
                 zipWith (\p0 p -> Node p p0 0) ps (tail ps))
               ((++[fromIntegral cut])$ map fromIntegral $ takeWhile (<cut) primes))

addPrimes = go 0 where
  go pinc [] ft = ft
  go pinc ((!p,!pp):ps) !ft = case FT.split (\(Measure c _ _) -> c + pinc >= p) ft of
    (l, r) -> l ><
              (case FT.viewl r of
                  Node pr p0r prp :< r | pr == p ->
                    Node p p0r (prp + pp) <| go p ps r)

gogo sz = snd $ go (manyPrimes (sz + 1)) (binomRatios sz) where
  go ft = foldl' step (ft,sFT ft)
  step (!ft,!acc) (a,b) =
    ((addPrimes (map (second negate) $ reverse b) . addPrimes (reverse a)) ft
     , acc + sFT ft)

sFT ft = (\(Measure _ s _) -> s) (FT.measure ft)

main = print $ gogo 11111111
