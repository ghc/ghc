{-# LANGUAGE UnliftedDatatypes, PolyKinds, DataKinds, TypeApplications #-}

module T18481a where

import Data.Kind
import GHC.Types( Levity(..), RuntimeRep(..), TYPE )

type T :: TYPE (BoxedRep r) -> TYPE (BoxedRep r)
data T a = MkT Int

f :: T Bool
f = MkT @Lifted @Bool 42
