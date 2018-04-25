{-# LANGUAGE GADTs, TypeInType, ConstraintKinds #-}
module T11821a where
import Data.Proxy
type SameKind (a :: k1) (b :: k2) = ('Proxy :: Proxy k1) ~ ('Proxy :: Proxy k2)
