{-# Language KindSignatures #-}
{-# Language PolyKinds      #-}

module T15881a where

import Data.Kind

data A n (a :: n) :: a -> Type
