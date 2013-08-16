module Stream where

import Data.PQueue.Min (minView, empty, insert, size)
import Data.Monoid

data Stream o e = Stream !o !(Maybe e) [Stream o e]

may2Int Nothing = 0
may2Int _ = 1

instance Eq o => Eq (Stream o e) where
  Stream a1 b1 _ == Stream a2 b2 _ = a1 == a2 && may2Int b1 == may2Int b2
instance Ord o => Ord (Stream o e) where
  compare (Stream a1 b1 _) (Stream a2 b2 _) = compare a1 a2 `mappend` compare (may2Int b1) (may2Int b2)

runStream :: Ord o => Stream o e -> [e]
runStream s = gos empty s where
  gos m (Stream o Nothing tls) = go $ foldr insert m tls
  gos m (Stream o (Just e) tls) = e : go (foldr insert m tls)
  go m = case minView m of
    Nothing -> []
    Just (s, mm) -> gos mm s

runStreamDbg :: Ord o => Stream o e -> [((Int, Int), e)]
runStreamDbg s = gos 0 empty s where
  gos steps m (Stream o Nothing tls) = go steps $ foldr insert m tls
  gos steps m (Stream o (Just e) tls) = ((size m, steps), e) : go steps (foldr insert m tls)
  go steps m = case minView m of
    Nothing -> []
    Just (s, mm) -> (gos $! (steps + 1)) mm s

type PES o e = Maybe (Stream o e)

listToStream :: [a] -> PES a a
listToStream [] = Nothing
listToStream (h : t) = Just $ go h t where
  go h l = Stream h (Just h) $ case l of
    [] -> []
    (g : t) -> [go g t]
