{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepPolyApp where

import Data.Kind
import GHC.Exts
import Prelude ( undefined )

foo :: forall rep (a :: TYPE rep). ( a -> a ) -> a
foo f = f ( undefined :: a )
