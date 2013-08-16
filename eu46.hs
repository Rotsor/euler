module Main where

import Primes hiding (primes)
import Data.Numbers.Primes

goods = foldr lMerge undefined [[2 * n ^ 2 + p|p <- tail primes]|n <- [0..]]
bads = [3,5..] `minus` goods

main = print (head bads)
