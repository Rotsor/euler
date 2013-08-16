{-
as = 1 + b ^ 2
bs = 1 + c ^ 2
cs = b ^ 2 + c ^ 2
area = 1/4 * sqrt ((as + bs + cs)^2 - 2 * (as^2 + bs^2 + cs^2))


area = 1/4 * sqrt ((1 + b^2 + 1 + c^2 + b^2 + c^2)^2 - 2 * ((1 + b^2)^2 + (1 + c^2)^2 + (b^2+c^2)^2))
area = 1/4 * sqrt (4 * (1 + b^2 + c^2)^2 - 2 * (1 + 2*b^2 + b^4 + 1 + 2*c^2 + c^4 + b^4 + 2*b^2*c^2 + c^4))
area = 1/4 * sqrt (4 * (1 + b^2 + c^2 + b^2 + b^4 + b^2 * c^2 + c^2 + b^2*c^2 + c^4) 
                  - 2 * (2 + 2*b^2 + 2 * b^4 + 2*c^2 + 2*b^2*c^2 + 2 * c^4))
area = 1/4 * sqrt (4 * (1 + 2*b^2 + 2*c^2 + b^4 + 2*b^2*c^2 + c^4) 
                  - 4 * (1 + b^2 + b^4 + c^2 + b^2*c^2 + c^4))
area = 1/4 * sqrt (4 * (b^2 + c^2 + b^2*c^2))
area = 1/2 * sqrt (b^2 + c^2 + b^2*c^2)
area = 1/2 * sqrt ((b + c) ^ 2 - 2*b*c + (b*c)^2)
area = 1/2 * sqrt ((b + c) ^ 2 + (b*c - 1)^2 - 1)
area = 1/2 * sqrt ((1 + b^2) (1 + c^2) - 1)
-}

isqrt n | r * r == n = [r]
        | otherwise = [] where
        r = floor (sqrt $ fromIntegral n)

solution lim = [s `div` 2| b<-takeWhile(<lim)[1..]
                , let ss c = (1 + b*b) * (1 + c*c) - 1
                , c <-takeWhile((<=(lim*2)^2).ss)[b..]
                , s <- isqrt (ss c)
                , s `mod` 2 == 0 ]

main = print $ sum $ solution (10^6)

-- sum of integral area <= 10^10