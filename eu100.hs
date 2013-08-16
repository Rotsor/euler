import System.IO
import Data.Ratio
{-
a/n*(a-1)/(n-1)=1/2
a*(a-1)*2 = n*(n-1)
a*(a-1)*4*2 = n*(n-1)*4
n>10^12
a*a*2 - a*2 - n*(n-1) = 0

a*a - a - n*(n-1)/2 = 0
(a + c)^2 + q = 0
a*a + 2*a*c + c^2 + q = 0
c = -1/2
q = -n*(n-1)/2 - 1/4

2*a - 1 = sqrt (n*(n-1)*2 - 1)

4*n*(n-1) = (2n - 1)^2 - 1
(2n - 1)^2 = 4n^2 - 4n + 1

2(2a - 1)^2 = (2n-1)^2 + 1
-2x^2+y^2=-1

x = 2n-1
y = 2a-1

2x^2 - y^2 = 1
(2 * x1 ^ 2 - y1 ^ 2) (2 * x2 ^ 2 - y2 ^ 2) = 
= (2 * x1 * x2 + y1 * y2) ^ 2 - 2 * (x1 * y2 + x2 * y1) ^ 2
= 




-}

isqrt x = filter (\c -> c*c == x) [s, s+1] where
  s = floor $ sqrt $ fromIntegral x

isqrt' x | r * r == x = Right r
  | otherwise = Left $ (r, x - r*r) where
  r = floor $ sqrt $ fromIntegral x

idiv a b = case divMod a b of
  (r, 0) -> [r]
  _ -> []

bruteForce = [r|n<-[1+1..],let tam1 = isqrt' $ div((2*n-1)^2+1)2, Right r<-[tam1]]

cf x = case properFraction x of (nx, fx) -> nx : cf (recip fx)
approxs (x:xs) = fromIntegral x : map ((fromIntegral x+) . recip) (approxs xs)
odds (h : _ : t) = h : odds t
zzz = [ (blue, total) |x <- odds $ approxs $ (1 : repeat 2) :: [Rational]
        , let un2 x = (x + 1) `div` 2
        , let total = un2 (numerator x)
        , let blue = un2 (denominator x)
        ]
main = print $ fst $ head $ dropWhile ((<10^12).snd) zzz
