module Totient where
import Data.Ratio

product' = product . map (uncurry (^))

totient factors = product (map (\(p, c) -> p ^ (c-1)) factors) * product [(p-1)|p<-map fst factors]
totientCoeff factors = product [(p-1) % p|p<-map fst factors]
