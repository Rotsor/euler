{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Data.MemoTrie
import Data.List

import Modular

fact n = fromInteger $ product [2..n]

choices c n = fromInteger $ product (genericTake c [n,n-1..]) `div` fact c

fastVariants = memo3 variants

variants 0 0 0 = 1
variants n1 n2 n3
  | n1 >= 0 && n2 >= 0 && n3 >= 0 =
    fact 4 *
    sum
    [ choices c1 n1 * choices c2 n2 * choices c3 n3
      * fastVariants (n1 - c1 + c2) (n2 - c2 + c3) (n3 - c3)
    | c1 <- [0..min n1 4]
    , c2 <- [0..min n2 (4-c1)]
    , let c3 = 4 - c1 - c2, c3 <= n3]

solve n = variants 0 0 (n `div` 3) / fact (n `div` 3) :: Mod 1000000007

main = print $ solve 600
