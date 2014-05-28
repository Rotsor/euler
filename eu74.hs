import Data.List
import Data.Tuple

digits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (x `divMod` 10)))

chain x = x : chain (sum (map (\i->product [1..i]) (digits x)))

goodChain = good . reverse . take 61 . chain where
  good (last : others) = any (== last) others && (length . group . sort $ others) == length others

main = print $ length $ filter goodChain [1..10^6]
