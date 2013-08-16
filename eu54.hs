import Control.Monad.RWS

type Suit = Int
data Card = Card { cardSuit :: Suit, cardValue :: Value }
type Valuator = RWS [Card] [Maybe ([Card], [Card])] ()


v :: ([Card] -> Maybe (Int, Int)) -> Valuator ()
v f = do
  inp <- ask
  tell (f ask)

isFlush cards = length (nub (map cardSuit cards)) > 1

-- ok cards 

fourOK cards = okOf 4

strFlush cards
  | not (isFlush cards) = Nothing
  | let vals = sort (map cardValue cards) in all (==1) $ zipWith (-) (tail vals) vals = Just (cards, [])
  | otherwise = Nothing

rFlush cards 
  | not $ isFlush cards = Nothing
  | map cardValue cards == [10..14] = Just (cards, [])

-- eval = v rFlush >> sflush >> fourok >> fullHouse >> flush >> straight >> threeok >> twoPairs >> onePair >> highCard

