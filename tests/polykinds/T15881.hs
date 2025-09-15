{-# Language KindSignatures #-}
{-# Language PolyKinds      #-}

module T15881 where

import Data.Kind

data A n (a :: n n) :: Type
