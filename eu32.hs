import Data.List
import qualified Data.Set as Set
import Control.Arrow

choices [] = []
choices (a : as) = (a, as) : map (second (a:)) (choices as)

mkPandis :: Int -> [a] -> [([a],[a])]
mkPandis 0 l = [([],l)]
mkPandis n l = choices l >>= \(x,l') -> map (first (x:)) $ mkPandis (n-1) l'

main = print $ sum . Set.toList . Set.fromList $
  [ x
    | ([a,b],rest)<-go [1,4] ++ go [2,3]
    , let x = read a * read b
    , sort(show$read a*read b)==rest
  ] where
 go l = gogo l ['1'..'9'] where
  gogo [] l = [([],l)]
  gogo (n : t) l = mkPandis n l >>= \(x,rest) -> map (first (x:)) (gogo t rest)
