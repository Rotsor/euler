
import Primes

multsOf [] = [1]
multsOf (p : ps) = foldr lMerge undefined [map (p^pow * ) (multsOf ps)|pow<-[1..]]

mergeBy f as bs = map snd (merge (pr as) (pr bs)) where pr = map (\x -> (f x, x))
lMergeBy f (a : as) bs = a : mergeBy f as bs

subsetsBy f 0 l = [[]]
subsetsBy f n [] = []
subsetsBy f n (h : t) = lMergeBy f (map (h:) (subsetsBy f (n-1) t)) (subsetsBy f n t)

primeQuads = subsetsBy product 4 primes
zz = ifold lMerge (map multsOf primeQuads)
qq = scanl (\(c, prev) next -> (if next == prev + 1 then c+1 else 0, next)) (0, 0) zz

main = print $ subtract 3 $ snd $ head $ dropWhile ((<3) . fst) qq