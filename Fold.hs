module Fold where

pairwise f (a : b : t) = f a b : pairwise f t
pairwise f [x] = [x]
pairwise f [] = []

treeFold f [x] = x
treeFold f l = treeFold f (pairwise f l)
