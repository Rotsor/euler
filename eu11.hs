import Data.List

diagonals ((x : xs) : xss) = [x] : takeWhile (not . null) (zipWith (++) (map return xs ++ repeat []) (diagonals xss ++ repeat []))
diagonals [] = []

prods = map product . filter ((==4) . length) . map (take 4) . tails

main = do
  txt <- readFile "eu11.in" 
  let d = map (map read . words) . lines $ txt
  print $ maximum $ d ++ transpose d ++ diagonals d ++ diagonals (transpose d) >>= prods
