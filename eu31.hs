ways 0 _ = 1
ways _ [] = 0
ways n css@(c : cs) | n < c = 0
 | otherwise = ways (n - c) css + ways n cs
               
answer = ways 200 [1,2,5,10,20,50,100,200]