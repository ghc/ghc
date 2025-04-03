{-# OPTIONS -Werror=implicit-rhs-quantification #-}
{-# LANGUAGE DataKinds #-}
module T23510a where

import Data.Proxy
import GHC.Types

type T1 = 'Nothing :: Maybe a

type T2 = 'Left :: a -> Either a b

type T3 = 'Proxy :: Proxy k

type Const (a :: Type) (b :: Type) = a
type TS = (Int :: Const Type a)

type Bad = (forall (v1 :: RuntimeRep) (a1 :: TYPE v). a1) :: TYPE v
