{- test GHCi support for unlifted types -}

{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -fobject-code #-}

module T19628a where

import GHC.Exts
import GHC.Arr

import Data.Kind
import Control.Exception

-- unlifted but boxed datatypes
type Strict :: Type -> TYPE ('BoxedRep 'Unlifted)
data Strict a = Force a

type Strict2 :: Type -> TYPE ('BoxedRep 'Unlifted)
data Strict2 a = Force2 a a

{-# NOINLINE addStrict #-}
addStrict :: Strict Int -> Strict Int -> Strict Int
addStrict (Force x) (Force y) = Force (x+y)

{-# NOINLINE unStrict #-}
unStrict :: Strict a -> a
unStrict (Force x) = x

{-# NOINLINE toStrict #-}
toStrict :: a -> Strict a
toStrict x = Force x
