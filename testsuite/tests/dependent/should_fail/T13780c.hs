{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T13780c where

import Data.Kind
import T13780b

type family ElimBool (p :: Bool -> Type) (b :: Bool) (s :: Sing b)
                     (pFalse :: p False) (pTrue :: p True) :: p b where
  ElimBool _ _ SFalse pFalse _ = pFalse
  ElimBool _ _ STrue  _ pTrue  = pTrue
