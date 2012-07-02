module Search where

bsearch f a b | a + 1 == b = a
  | f c = bsearch f a c
  | otherwise = bsearch f c b
 where c = (a + b) `div` 2

esearch f start = go 1 where
  go step | f (start + step) = bsearch f start (start+step)
    | otherwise = go (step*2)
