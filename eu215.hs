{-# LANGUAGE TypeFamilies #-}
import Control.Monad

replace 0 x (h : t) = x : t
replace n x (h : t) = h : replace (n - 1) x t

{-
HasTrie stuff is there because I didn't have MemoTrie library
available and decided to reproduce it.
-}

class HasTrie a where
  type Trie a :: * -> *
  build :: (a -> b) -> Trie a b
  find :: Trie a b -> (a -> b)

data IntTrie b = IntTrie b (IntTrie b) (IntTrie b) (IntTrie b) (IntTrie b) {- zero, odd, even -}

{- non-negative integers only! -}
instance HasTrie Integer where
  type Trie Integer = IntTrie
  build f = IntTrie (f 0)
    (build $ \x -> f $ 1 + x * 4)
    (build $ \x -> f $ 2 + x * 4)
    (build $ \x -> f $ 3 + x * 4)
    (build $ \x -> f $ 4 + x * 4)

  find (IntTrie x _ _ _ _) 0 = x  
  find (IntTrie _ t1 t2 t3 t4) i = case i `divMod` 4 of
    (i, 1) -> find t1 i
    (i, 2) -> find t2 i
    (i, 3) -> find t3 i
    (i, 0) -> find t4 (i - 1)

data ListTrie a b =
  ListTrie (b {- at empty -}) (Trie a (ListTrie a b {-at cons-}))

instance HasTrie a => HasTrie [a] where
  type Trie [a] = ListTrie a
  build f = ListTrie (f []) (build $ \item -> build (f . (item:)))
  find (ListTrie e _) [] = e
  find (ListTrie _ t) (x : xs) =
    find (find t x) xs

newtype TupTrie a b c = TupTrie (Trie a (Trie b c))
    
instance (HasTrie a, HasTrie b) => HasTrie (a, b) where
  type Trie (a, b) = TupTrie a b
  build f = TupTrie $ build $ \a -> build (\b -> f (a, b))
  find (TupTrie t) (a, b) = find (find t a) b

    
memo :: HasTrie a => (a -> b) -> (a -> b)
memo f = find (build f)
  
solve l h w =
 mgo w where
  mgo = memo go
  go w
   | all (==l) w = 1
   | otherwise
    = case minimum (zip w [0..]) of
    (il, i) ->
      r 2 + r 3 where
       r bl =
        let guard c x = if c then x else 0 in
        let nl = bl + il in
        guard (l >= nl) $
        let ok i = i < 0 || i >= h || l == nl || nl /= (w !! i) in
        (guard . ok $ i + 1) $
        (guard . ok $ i - 1) $
        mgo (replace i nl w) :: Integer

ggg :: Integer -> Int -> Integer
ggg l h = solve l h (replicate h 0)
      
main = print $ ggg 32 10
