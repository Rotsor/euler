fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

result = length $ takeWhile ((<1000).length.show) fibs