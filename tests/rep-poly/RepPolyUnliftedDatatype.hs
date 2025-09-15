{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedDatatypes #-}

module RepPolyUnliftedDatatype where

import Data.Kind
import GHC.Exts

type D :: forall (l :: Levity) -> TYPE (BoxedRep l)
data D l = forall x. (Show x, Eq x, Ord x, Read x) => MkD x Int x
  -- the constraints are to ensure we are not including them
  -- in the calculation of the arity

foo :: forall (l :: Levity). () -> D l
foo _ = MkD False 12 True
