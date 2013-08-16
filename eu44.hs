{-# LANGUAGE BangPatterns #-}

import Control.Arrow
import Data.List
import Test.QuickCheck
import Control.Monad
import System.IO

p n = n * (3 * n - 1)
ps = map p [1..]

initPara = Parabola 2 8

divu a b = (a + b - 1) `div` b

data Parabola = Parabola { paraPos :: !Integer, paraVelo :: !Integer }  deriving (Eq, Show)

stepProp n = n >= 0 ==> iterate step initPara !! n == stepBy (fromIntegral n) initPara

step (Parabola x v) = Parabola (x + v) (v + 6)
stepBy n (Parabola x v) = Parabola (x + v * n + 6 * n * (n - 1) `div` 2) (v + 6 * n)

findDiff :: Integer -> Parabola -> Parabola -> Int -> (Int, [(Integer, Integer)])
findDiff !diff !aPara !bPara !steps
  | need == 0 = second ((a, b) :) $ findDiff diff (step aPara) (step bPara) steps
  | b == a && b2 > a + diff = (steps, [])
  | b < a = (steps, [])
  | need > 0 && b2 > a + diff = 
     let
       stepValue = (b2 - b) - (a2 - a)
       stepss = (a + diff - b) `divu` stepValue
     in 
       findDiff diff (stepBy stepss aPara) (stepBy stepss bPara) (steps + 1)
  | need > 0 = findDiff diff aPara (step bPara) (steps + 1)
  | need < 0 = findDiff diff (step aPara) bPara (steps + 1)
    where
     a = paraPos aPara
     b = paraPos bPara
     b2 = paraPos (step bPara)
     a2 = paraPos (step aPara)
     need = a + diff - b

idiv a b = do { guard $ a `mod` b == 0; return $ a `div` b }

isqrt n = filter (\c -> c * c == n) [s, s+1] where
  s = floor $ sqrt $ fromIntegral n

unP p = isqrt (p * 12 + 1) >>= \s -> idiv (s+1) 6

go d = [(pa, pb, s) | (pa, pb) <- snd $ findDiff (p d) initPara initPara 0, let ps = pa + pb, s <- unP ps]

perebor = [(d,go d)|d<-[1..]]
solution = map ((`div` 2) . p . fst) $ filter (not . null . snd) perebor
main = hSetBuffering stdout NoBuffering >> print solution
