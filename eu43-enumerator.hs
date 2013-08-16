{-# LANGUAGE FlexibleInstances #-}
import Control.Monad
import Control.Arrow

import qualified Data.Enumerator.List as EL
import Control.Monad.Trans
import Data.Enumerator (($$), run_, Iteratee, (>>==), returnI, Enumerator, Stream(..), Step(..))
import Data.Enumerator.Internal

instance MonadPlus m => MonadPlus (Iteratee a m) where
  mzero = Iteratee $ mzero
  mplus a b = Iteratee $ mplus (runIteratee a) (runIteratee b)

choose [] = []
choose (a : as) = (a, as) : map (second (a:)) (choose as)

pick :: Monad m => Iteratee a m a
pick = EL.head_

primes = [1,2,3,5,7,11,13,17]

pandiEnumerator :: [a] -> Enumerator a [] b
pandiEnumerator l (Continue k) = do
  (x, rest) <- lift $ choose l
  k (Chunks [x]) >>== pandiEnumerator rest
pandiEnumerator l step = returnI step

runPandi :: Iteratee a [] b -> [a] -> [b]
runPandi iter variants = run_ (pandiEnumerator variants $$ iter)

main = print $ sum $ map (read . reverse) $ (`runPandi` ['0'..'9']) $ do
  let 
      go [] l = return l 
      go (p : t) l = do
       next <- pick
       let l' = next : l
       guard (read (reverse $ take 3 l') `mod` p == 0)
       go t l'
  replicateM 2 pick >>= go primes

   
