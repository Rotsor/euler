import Network.HTTP

solve = go [] where
  go pre [] = maximum pre
  go pre (l : ls) = go (zipWith max (zipWith (+) (pre ++ [0]) l) (zipWith (+) (0 : pre) l)) ls

main = simpleHTTP (getRequest "http://projecteuler.net/project/triangle.txt") >>= getResponseBody >>=
         print . solve . map (map read . words) . lines
