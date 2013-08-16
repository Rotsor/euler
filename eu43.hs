import Control.Monad
import Control.Arrow

data Pandy p a = Pandy { runPandi :: [p] -> [(a, [p])] }
      
instance Monad (Pandy p) where
  return x = Pandy $ \l -> [(x,l)]
  Pandy pa >>= f = Pandy $ \l -> pa l >>= \(x, rest) -> runPandi (f x) rest
instance MonadPlus (Pandy p) where
  mzero = Pandy $ const []
  mplus a b = Pandy $ \l -> runPandi a l ++ runPandi b l

choose [] = []
choose (a : as) = (a, as) : map (second (a:)) (choose as)

pick :: Pandy p p
pick = Pandy choose

primes = [1,2,3,5,7,11,13,17]

main = print $ sum $ map (read . reverse . fst) $ (`runPandi` ['0'..'9']) $ do
  let 
      go [] l = return l 
      go (p : t) l = do
       next <- pick
       let l' = next : l
       guard (read (reverse $ take 3 l') `mod` p == 0)
       go t l'
  replicateM 2 pick >>= go primes

   
