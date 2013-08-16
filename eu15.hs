biroutes = go (repeat 1) where
  go l = l : go (gogo (tail l) 1) where
   gogo (h : t) n = n : gogo t (h + n)
    
main = print $ biroutes !! 20 !! 20