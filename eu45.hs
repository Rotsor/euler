
{-
penta n = n * (3*n - 1) / 2
 = 3* (n - 1/6 + 1/6) (n - 1/6 - 1/6) / 2
 = 3 * ((n - 1/6)^2 - 1/36) / 2
 = 3* ((6*n - 1)^2 - 1) / 36 / 2
 = ((6*n - 1)^2 - 1) / 24

tri n = n*(n+1)/2
 = (n-0.5+0.5)(n+0.5+0.5)/2
 = ((n+0.5)^2 - 0.5^2) / 2
 = ((2n + 1)^2 - 1) / 8
-}
isqrt x = filter (\c -> c*c == x) [s, s+1] where
  s = floor $ sqrt $ fromIntegral x

idiv a b = case divMod a b of
  (r, 0) -> [r]
  _ -> []

unTri t = isqrt (t * 8 + 1) >>= \s -> idiv (s - 1) 2
unPenta p = isqrt (p * 24 + 1) >>= \s -> idiv (s + 1) 6
hexa n = n * (2 * n - 1)
main = print $ [(ti,pi,hi)|hi<-[1..],let h = hexa hi,pi <- unPenta h,ti <- unTri h]