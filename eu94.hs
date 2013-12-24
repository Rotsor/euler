
isqrt n | r * r == n = [r]
        | otherwise = [] where
        r = floor (sqrt $ fromIntegral n)

candidatesP :: [Integer]
candidatesP = takeWhile (<= 10^9 `div` 3) $
  [2*i*i|i<-[1..]]

candidatesM :: [Integer]
candidatesM = takeWhile (<= 10^9 `div` 3) $
  [i*i|i<-[2..]]

tt = takeWhile ((<= 10^9) . snd)

qq = print $ sum $ map snd $
  tt [ (k, 3*(k*2+1)+1)
     | k <- candidatesP
     , rt <- isqrt(k*(3*k+2))
     ] ++
  tt [ (k, 3*(k*2+1)-1)
     | q <- candidatesM
     , let k = q - 1
     , rt <- isqrt(q*(3*q-2))
     ]

main = qq
-- areaP i =
--  sqrt (i ^ 2 - ((i + 1) / 2) ^ 2) * ((i + 1) / 2)
--  sqrt (3*i^2 - 2*i - 1) * ((i + 1) / 2) / 2
--  sqrt (3*(2*k+1)^2 - 2*(2*k+1) - 1) * (k + 1) / 2
--  sqrt (3*(4*k^2 + 4*k +1) - 4*k - 3) * (k + 1) / 2
--  sqrt (3*k^2 + 3*k - k) * (k + 1) / 2
--  sqrt (3*k^2 + 2*k) * (k + 1) / 2


--
-- Area of i x i x (i-1) triangle
-- areaM i =
--  sqrt (i ^ 2 - ((i - 1) / 2) ^ 2) * ((i - 1) / 2)
-- =
--   [i = 2*k+1]
-- sqrt ( (3 * (2*k+1)^2 + 2*(2*k+1) - 1) / 4 ) * (((2*k+1) - 1) / 2)
-- =
--  sqrt (3*k^2+3*k+ 1 + k) * k
-- =
--  sqrt (3*k^2 + 4*k + 1) * k
--  sqrt (k*(3*k + 4) + 1) * k
--   [k = q-1]
--  sqrt ((q-1)*(3*(q-1) + 4) + 1) * (q-1)
--  sqrt ((q-1)*(3*q + 1) + 1) * (q-1)
--  sqrt (3*q^2 -2*q - 1 + 1) * (q-1)
--  sqrt (3*q^2 -2*q) * (q-1)
--  sqrt (q * (3*q-2)) * (q-1)


-- sqrt ( (3 * (2*k+1)^2 + 2*(2*k+1) - 1) / 4 ) * (((2*k+1) - 1) / 2)
-- =

-- 3*k^2 + 4*k + 1
-- 

