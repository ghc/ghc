{-# OPTIONS_GHC -fforce-recomp -Wincomplete-patterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Bug where

import Data.Kind
import Data.Void

type SFalse = SBool 'False
type STrue  = SBool 'True

data SBool :: Bool -> Type where
  SFalse :: SFalse
  STrue  :: STrue

type family F (b :: Bool) :: Type where
  F 'False = Void
  F 'True  = ()

data T (b :: Bool)
  = MkT1
  | MkT2 !(F b)

ex :: SBool b -> T b -> ()
ex sb t =
  case t of
    MkT1 -> ()
    MkT2 f ->
      case sb of
        STrue -> f

ex2 :: SBool b -> T b -> ()
ex2 sb t =
  case t of
    MkT2 f ->
      case sb of
        STrue -> f
    MkT1 -> ()
