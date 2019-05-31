{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TopLevelKindSignatures, ExplicitForAll #-}
module T13780c where

import Data.Kind
import T13780b

type ElimBool ::
  forall (p :: Bool -> Type) ->
  forall (b :: Bool) ->
  Sing b ->
  p False ->
  p True ->
  p b
type family ElimBool p b s pFalse pTrue where
  ElimBool _ _ SFalse pFalse _ = pFalse
  ElimBool _ _ STrue  _ pTrue  = pTrue
