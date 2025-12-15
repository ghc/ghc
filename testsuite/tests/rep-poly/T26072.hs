{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}

module T26072 where

import GHC.Exts
import Data.Proxy
import Data.Kind

type UnboxedRep :: TYPE r -> RuntimeRep
type family UnboxedRep b

type instance UnboxedRep Int = IntRep

type HasUnboxed :: forall {r}. TYPE r -> Constraint
class HasUnboxed b where
    type Unboxed b :: TYPE (UnboxedRep b)
    box :: Unboxed b -> b
    unbox :: b -> Unboxed b

instance HasUnboxed Int where
    type Unboxed Int = Int#
    box x = I# x
    unbox (I# x) = x

ok :: Int# -> Int
ok = box

bad :: Int# -> Int
bad x = box x
