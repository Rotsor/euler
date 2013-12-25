import Data.List

f n = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10

integrate x l = scanl (+) x l
differentiate l = (head l, zipWith (-) (tail l) l)

extend [x] = [x,x]
extend l = case differentiate l of
  (h, t) -> integrate h (extend t)

sequ = [f n | n <- [1..11]]

bopfit l
  | e == l = 0
  | otherwise = last e
  where
    e = extend (init l :: [Integer])
  

result = sum $ map bopfit (drop 2 $ inits sequ)
main = print result
