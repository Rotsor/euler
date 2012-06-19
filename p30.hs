import Data.List
digits = unfoldr (\x -> if x == 0 then Nothing else Just (x`mod`10, x`div`10))
sd5 = sum . map (^5) . digits
sd5s = filter (\x -> sd5 x == x) [2..360000]
answer = sum sd5s
