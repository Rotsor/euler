{-# LANGUAGE FlexibleInstances #-}
import Control.Monad
import Control.Arrow

import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Internal

instance MonadPlus m => MonadPlus (Pipe i o m) where
  mzero = PipeM mzero (FinalizeM mzero)
  mplus a b = PipeM (mplus (return a) (return b)) (return $ error "wtf to do here")

choose [] = []
choose (a : as) = (a, as) : map (second (a:)) (choose as)

pick :: MonadPlus m => Sink a m a
pick = require await

primes = [1,2,3,5,7,11,13,17]

pandiSource :: [a] -> Source [] a
pandiSource [] = Done Nothing ()
pandiSource l = do
  (x, rest) <- lift $ choose l
  HaveOutput (return ()) (return ()) x
  pandiSource rest

require :: MonadPlus m => m (Maybe a) -> m a
require a = a >>= maybe mzero return

runPandi :: Sink a [] b -> [a] -> [b]
runPandi iter variants = pandiSource variants $$ iter

main = print $ sum $ map (read . reverse) $ (`runPandi` ['0'..'9']) $ do
  let 
      go [] l = return l 
      go (p : t) l = do
       next <- pick
       let l' = next : l
       guard (read (reverse $ take 3 l') `mod` p == 0)
       go t l'
  replicateM 2 pick >>= go primes
