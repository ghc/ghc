{-# LANGUAGE AutoDeriveTypeable, PolyKinds, TypeFamilies, StandaloneDeriving #-}

module T9999 where

import Data.Typeable

data family F a

class C a where
  data F1 a
  type F2 a

main = typeRep (Proxy :: Proxy F) == typeRep (Proxy :: Proxy F1)
