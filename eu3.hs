factor x = go x 2 where
  go 1 _ = []
  go n p | p * p > n = [n]
     | n `mod` p == 0 = p : go (n `div` p) p
     | otherwise = go n (p + 1)

main = print $ last $ factor 600851475143

