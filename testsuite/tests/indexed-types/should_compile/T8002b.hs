{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}
module T8002b where

type family Rep a
type instance Rep Int = Int
