import Data.List

collect n ds = go (head ds) n ds where
  go lim 0 [] = [[]]
  go lim n [] = []
  go lim n (d : ds) = do
     i <- [1..min n . min d $ lim]
     map (i:) $ go i (n-i) ds

fact n = product [1..n]  

choose n k = fact n `div` fact k `div` fact (n - k)

shuffles l = fact (length l) `div` product (map (fact . length) $ group l)

go sides dice cnt smm = sum [(c-1)^ncc*choose dice (cc+cnt) * shuffles (chosen ++ replicate cc c)|chosen <- collect smm (replicate cnt sides),let c=last chosen,cc<-[0..dice-cnt],let ncc=dice-cnt-cc]

ex = go 6 5 3 15
result = go 12 20 10 70
main = print result
-- go

 --main = 