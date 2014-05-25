import Data.List
import Data.Tuple
import Test.QuickCheck

w i = go 1 i where
  go x i
    | i >= x * 2 = x + go (2*x) (i - x * 2)
    | otherwise = max 0 (i - x)

w' 0 = 0
w' 1 = 0
w' n = 1 + w (n - 2)

g n = [w' (i - 1) + w' (n - i)|i<-[1..n]]

f n = genericLength $ filter (== maximum (g n)) (g n)

s n = sum (map f [1..n])



tobin = reverse . unfoldr (\n -> if n == 0 then Nothing else Just (swap $ n `divMod` 2))
fromBin = foldr (\d -> (+d) . (*2)) 0 . reverse

f'' [] = 1
f'' [1] = 2
f'' [1,0] = 2
f'' [1,1] = 4
f'' [1,0,1] = 6
f'' (1 : 1 : 1 : r) = 1 + (2 ^ length r - fromBin r)
                      + 2 * (if (all (==0) r) then (2 ^ length r) else 0) + 2
f'' (1 : l) = case dropWhile (==0) l of
  (1 : 1 : r)
    ->
    2 * (1 + 2 ^ length r - fromBin r
         + if (all (==0) r)
           then
             2 ^ length r
           else
             0)
  (1 : 0 : r) -> 2 * (1 + fromBin r)
  [1] -> 8
  [] -> 3

f' :: Integer -> Integer
f' = f'' . tobin

f'Good n = n < 0 || f (n + 1) == f' n
s'Good n = n < 0 || s n == s' n

s' n = sum (map f' [0..n-1])

a // b | a `mod` b == 0 = a `div` b

sumLR f a b = ((f a + f b) * (b - a + 1)) // 2

s'' [] = 1
s'' [1] = 2 + s'' []
s'' [1,0] = 2 + s'' [1]
s'' [1,1] = 4 + s'' [1,0]
s'' [1,0,1] = 6 + s'' [1,0,0]
s'' (1 : 1 : 1 : r) =
  sumLR (\x -> 1 + (2 ^ length r - x) + 2) 0 (fromBin r)
  + 2 * (2 ^ length r)
  + s'' (1 : 1 : 0 : replicate (length r) 1)
s'' (1 : l) = case span (==0) l of
  (zs, (1 : 1 : r))
    ->
    2 * 2 ^ length r + 
    sumLR (\x -> 2 * (1 + 2 ^ length r - x)) 0 (fromBin r)
    + s'' (1 : zs ++ (1 : 0 : replicate (length r) 1))
  (zs, (1 : 0 : r)) ->
    sumLR (\x -> 2 * (1 + x)) 0 (fromBin r) + s'' (1 : zs ++ (0 : 1 : replicate (length r) 1))
  (zs, [1]) -> 8 + s'' (1 : zs ++ [0])
  (zs, []) -> 3 + s'' (replicate (length zs) 1)

s''' 0 = 0
s''' n = s'' . tobin $ n - 1

s'''Good n = n < 0 || s' n == s''' n

checkAll = do
  quickCheck f'Good
  quickCheck s'Good
  quickCheck s'''Good

main = putStrLn $ reverse $ take 8 $ reverse $ show $ s''' (10 ^ 12)
