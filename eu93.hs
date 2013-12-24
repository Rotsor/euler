{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
import Data.Ord
import Control.Arrow
import Control.Monad
import qualified Data.Set as Set

ops =
  map liftM2 [(+),(*),(-)] ++
  [\x y->do
      x <- x
      y <- y
      guard (y /= 0)
      return (x / y)
  ]

splits [] = [([],[])]
splits (h : t) = do
  (l, r) <- splits t
  [(h : l, r), (l, h : r)]

subset 0 l = [[]]
subset k [] = []
subset k (h : t) =
  map (h:) (subset (k-1) t) ++ subset k t

go l = (\(l,r) -> guard (not $ null l) >> guard (not $ null r) >> map (\x -> (x,r)) (goA l)) =<< splits l

goA [x] = [Just x]
goA l = do
  (a,l) <- go l
  b <- goA l
  op <- ops
  return $ a `op` b

longestSeq s =
  let set = Set.fromList s in
  head $ dropWhile (\x -> Set.member (Just $ fromInteger x) set) [1..]

main = putStrLn $ concat $ map show $
       ((fst $ maximumBy (comparing snd) $
         map (id &&& (longestSeq . goA . map fromInteger ))
         $ subset 4 [1,2,3,4,5,6,7,8,9]))
