{-# LANGUAGE KindSignatures, PolyKinds, MultiParamTypeClasses, FlexibleInstances #-}
module T6021 where

class Panic a b
instance Panic (a :: b) b
