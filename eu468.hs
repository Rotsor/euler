import Factorisation
import Primes
import Data.List
import Data.Ord
import Data.Int
import Fold
import qualified Data.Map as Map

newtype Inte = Inte { unInte :: Map.Map Integer Integer } deriving Eq

instance Num Inte where
  Inte x * Inte y = Inte $ Map.filter (/=0) $ Map.unionWith (+) x y
  fromInteger x = Inte $ Map.fromList (factor' x)

instance Integral Inte where
  toInteger (Inte x) = product . map (uncurry (^)) . Map.toList $ x

instance Fractional Inte where
  x / y = x * recip y
  recip (Inte m) = Inte $ fmap negate m

instance Show Inte where
  show = show . toInteger

instance Enum Inte where
instance Real Inte where
instance Ord Inte where
  compare = comparing toInteger

newtype Mod = Mod Int64 deriving Eq

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

facts sz = sort $ map (Inte . Map.fromList) (factorisationsTo sz primes)

binomRatios sz = let f = facts sz in zipWith (/) (reverse f) f

binoms sz = scanl (*) 1 (binomRatios sz)

product' = treeFold (*)

sFromTo :: Integer -> Integer -> Mod -> [(Integer, Integer)] -> Mod
{- a inclusive, b not inclusive -}
sFromTo a b m ((p, c) : x)
  | b > p = m * (fromIntegral $ p - a) + sFromTo p b (m * fromIntegral p ^ c) x
sFromTo a b m _ = m * (fromIntegral $ b - a)

main =
  let sz = 44444 in
  print $ fromIntegral $ (sum $ map (length . filter ((/=0) . snd) . Map.toList . unInte) (binoms sz))
