import Data.List
isPandi s = sort s == ['1'..'9']
answer = maximum $ do
  n <- [2..9]
  let catprod x = concat $ map show $ zipWith (*) [1..n] (repeat x)
  filter isPandi $ takeWhile((<10).length)(map catprod [1..])
  
