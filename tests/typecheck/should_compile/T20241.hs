{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

import Data.Kind
  ( Type, Constraint )
import Data.Proxy
  ( Proxy(..) )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )

type Assert :: Bool -> Constraint -> Constraint
type family Assert check errMsg where
  Assert 'True _      = ()
  Assert _     errMsg = errMsg

foo_sym :: Assert 'True (TypeError (Text "Boom")) => Proxy a -> ()
foo_sym Proxy = ()

type IsInt :: Type -> Bool
type family IsInt a where
  IsInt Int = True

bar_sym :: Assert (IsInt a) (TypeError (Text "Boom")) => Proxy a -> ()
bar_sym Proxy = ()
