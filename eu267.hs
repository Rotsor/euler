import Data.Ratio
import Search
import Data.List
import Data.Char
import Primes
import Control.Arrow

tersearch :: (Ord a, Fractional a, Num a, Num b, Ord b) => (a -> b) -> a -> a -> a -> a -> a
tersearch f lim a b c = result where
  (p,q) | b - a > c - b = ((b + a) / 2, b)
        | otherwise = (b, (b + c) / 2)
  result
      | a + lim >= c = b
      | f q > f p = tersearch f lim p q c
      | otherwise = tersearch f lim a p q

cs = iterate (scanl (+) 0) (repeat 1)
cst = iterate (\l -> zipWith (+) l (0 : l)) (1 : repeat 0)

cccc = go (repeat 1) where
  go l = l : go (scanl (+) 0 l) 

ccc n x = cccc !! fromIntegral x !! fromIntegral n
fact n = product [1..n]

probCoins :: Integer -> Integer -> Integer
probCoins n x = ccc n x

powers x = res where
  res = x : (res >>= (\y -> [y, y * x]) . (^2))

-- tosses

chanceForBillionaireness :: Rational -> Integer
chanceForBillionaireness f = sum [probCoins 1000 a|a<-[mina..1000]] where
  good a = (1 + 2*f) ^ a * (1 - f) ^ b
           >= 10^9
      where
   b = 1000 - a
  mina = bsearch good 0 1001 + 1


{-
(1+2f)^a * (1-f)^b >= 10^9
d((1+2f)^a * (1-f)^b) / df = 
  = 2a * (1+2f)^(a-1) * (1-f)^b - b*(1-f)^(b-1) * (1+2f)^a
  = (1+2f)^(a-1)*(1-f)^(a-1)* (2a * (1-f) - b (1+2f))
  = ...                    * (2a - 2fa - b - 2fb)
  =  . . .                 * ((2a - b) - 2f(a + b))
  ...

  f = (2a - b) / (2 * n)
-}

findMillys n a = (best, (1+2*best)^a * (1-best)^b) where
  b = n - a
  best = ((2*a - b) % (2 * n))


main = do
  let (n, (f, g)) = head $ dropWhile ((<10^9) . snd . snd) $ map (id &&& findMillys 1000) [320..]
{-  print n
  print f
  print $ fromRational g -}
  putStrLn $ ("0." ++) $ map intToDigit $ take 12 $ unfoldr (\x -> Just (properFraction (x*10))) $ (+0.5e-12) $ (chanceForBillionaireness f % (2 ^ 1000 :: Integer))

--  mapM_ (\i -> print $ ((,) i) $ fromRational $ chanceForBillionaireness i % (2 ^ 1000 :: Integer)) [0,0.05..1]
--  let res = tersearch chanceForBillionaireness 1e-20 0 0.5 1
{-  print $ floor $ (*10^12) $ fromRational $ res
  let l = map (\x -> chanceForBillionaireness x % (2 ^ 1000 :: Integer)) [0.204, 0.205, 0.206, 0.207]
  print $ length $ nub l
   -}

{-  print $ 
  print $ fromRational $ chanceForBillionaireness 0.207 % (2 ^ 1000 :: Integer) -}
