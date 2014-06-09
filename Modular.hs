{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modular where
import GHC.TypeLits
import Data.Proxy

newtype Mod (n :: Nat) = Mod { unMod :: Int }

instance Show (Mod n) where
  show (Mod n) = show n

type Mod7 = Mod 7

instance KnownNat n => Num (Mod n) where
  Mod a + Mod b = Mod $ (a + b) `mod` (fromIntegral $ natVal (Proxy :: Proxy n))
  Mod a * Mod b = Mod $ (a * b) `mod` (fromIntegral $ natVal (Proxy :: Proxy n))
  fromInteger i = Mod $ fromIntegral $ i `mod` (natVal (Proxy :: Proxy n))


instance KnownNat n => Fractional (Mod n) where
  recip = (^ (natVal (Proxy :: Proxy n) - 2))
  
