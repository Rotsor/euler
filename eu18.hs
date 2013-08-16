solve = go [] where
  go pre [] = maximum pre
  go pre (l : ls) = go (zipWith max (zipWith (+) (pre ++ [0]) l) (zipWith (+) (0 : pre) l)) ls

main = readFile "eu18.in" >>= print . solve . map (map read . words) . lines