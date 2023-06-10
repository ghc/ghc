module T17328a where

newtype N1 = MkN1 N2

newtype N2 = MkN2 N1

instance Eq N2 where
  (==) = const (const False)
