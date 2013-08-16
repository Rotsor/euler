{-# LANGUAGE RankNTypes #-}

import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Control.Applicative

lazyST :: (ST s (init s), init s -> ST s a) -> ST s [a]
lazyST (init, step) = do
  state <- init
  let go = do
       x <- step state
       (x:) <$> go
  go

runLazyST :: (forall s.(ST s (init s), init s -> ST s a)) -> [a]
runLazyST ops = runST $ lazyST ops

newtype Qwe s = Qwe { unQwe :: STRef s Int }

numbers :: [Int]
numbers = runLazyST zz where
  zz :: (ST s (Qwe s), Qwe s -> ST s Int)
  zz = (Qwe <$> newSTRef 0, \(Qwe r) -> modifySTRef r (+1) >> readSTRef r)

-- primes :: [Int]

p l = if null l then [[]] else foldr (++) [] [ map (x :) $ p (filter (/=x) l) | x <- l ]