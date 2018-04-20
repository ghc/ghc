{-# LANGUAGE DataKinds, TypeFamilies #-}

newtype X = RollX (() -> X)

type family F t :: X where
    F t = RollX (t -> ())
