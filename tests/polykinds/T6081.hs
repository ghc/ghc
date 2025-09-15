{-# LANGUAGE KindSignatures, PolyKinds, DataKinds, RankNTypes #-}

module T6081 where

import Data.Kind (Type)

data KProxy (a :: Type) = KP

class KindClass (kp :: KProxy k)
instance KindClass (KP :: KProxy [k])


