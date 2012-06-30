import Data.List

data Nat = Zero | Suc Nat
instance Eq Nat where
instance Ord Nat where
  Zero > _ = False
  Suc _ > Zero = True
  Suc a > Suc b = a > b

isPalindrome str = str == reverse str

palindromize n | isPalindrome (show r) = []
  | otherwise = r : palindromize r where
  r = n + read (reverse (show n))
instance Num Nat where
  Zero + x = x
  Suc a + b = Suc (a + b)
  fromInteger 0 = Zero
  fromInteger x = Suc (fromInteger (x - 1))

isLychrel n = genericLength (palindromize n) > (50 :: Nat)
main = print $ length $ filter isLychrel [1..10000]