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

newtype Inte = Inte { unInte :: Map.Map Int Int } deriving Eq

instance Num Inte where
  Inte x * Inte y = Inte $ Map.unionWith (+) x y
  fromInteger x = Inte $ Map.fromList (factor' $ fromIntegral x)

instance Fractional Inte where
  x / y = x * recip y
  recip (Inte m) = Inte $ fmap negate m

instance Show Inte where
  show = show . toRational

instance Enum Inte where
instance Real Inte where
  toRational (Inte x) = product . map (\(p,pc) -> fromIntegral p ^^ pc) . Map.toList $ x


instance Ord Inte where
  compare = comparing toRational

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

binomRatios sz = let f = sort $ map (Inte . Map.fromList) $ facts sz in zipWith (/) (reverse f) f

binoms sz = scanl (*) 1 (binomRatios sz)

product' = treeFold (*)

sFromTo :: Int -> Int -> [(Int, Int)] -> Mod
{- a inclusive, b not inclusive -}
sFromTo a b ((p, c) : x)
  | b > p = fromIntegral (p - a) + (fromIntegral p ^ c) * sFromTo p b x
sFromTo a b _ = fromIntegral $ b - a

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
                 zipWith (\p0 p -> Node p p0 0) ps (tail ps)) ((++[fromIntegral cut])$ map fromIntegral $ takeWhile (<cut) primes))

addPrimes = go 0 where
  go pinc [] ft = ft
  go pinc ((!p,!pp):ps) !ft = case FT.split (\(Measure c _ _) -> c + pinc >= p) ft of
    (l, r) -> l ><
              (case FT.viewl r of
                  Node pr p0r prp :< r | pr == p ->
                    Node p p0r (prp + pp) <| go p ps r)

gogo sz = snd $ go (manyPrimes (sz + 1)) (binomRatios sz) where
  go ft = foldl' step (ft,sFT ft)
  step (!ft,!acc) ratio =
    (addPrimes (map (fromIntegral *** fromIntegral) . Map.toList . unInte $ ratio) ft
     , acc + sFT ft)

sFT ft = (\(Measure _ s _) -> s) (FT.measure ft)

sTo :: Int -> [(Int, Int)] -> Mod
sTo n l = (\(Measure _ s _) -> s) (FT.measure ft)
   where
    ft = FT.fromList (build 0 (map (fromIntegral *** fromIntegral) $ filter ((<n) . fst) l))
    
    build p0 ((p,pp):ps) = Node p p0 pp : build p ps
    build p0 [] = [Node (fromIntegral n) p0 0]

main = print $ gogo 1111111 {-
  let sz = 44444 in
  print $ fromIntegral $ (sum $ map (length . filter ((/=0) . snd) . Map.toList . unInte) (binoms sz))
-}
