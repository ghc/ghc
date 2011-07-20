{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances #-}

-- Test Trac #2856

module T2856 where

import Data.Ratio

----------------------
class C a where
    data D a

instance C Bool where
    newtype D Bool = DInt Int deriving (Eq, Show, Num)

instance C a => C [a] where
    newtype D [a] = DList (Ratio a) deriving (Eq, Show, Num)

----------------------
data family W a
newtype instance W Bool = WInt Int deriving( Eq, Show )
newtype instance W [a] = WList (Ratio a) deriving( Eq, Show )

deriving instance Num (W Bool)
deriving instance (Integral a, Num a) => Num (W [a])
  -- Integral needed because superclass Eq needs it,
  -- because of the stupid context on Ratio

