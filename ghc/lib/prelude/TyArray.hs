module  PreludeArray (
	Assoc(..), Array(..), _ByteArray(..)
    ) where

import Cls
import Core
import TyComplex
import PS		( _PackedString, _unpackPS )


data  Assoc a b =  a := b  deriving ()

-- Report: we do not do this:
-- data  {-(Ix a)    =>-} Array a b = MkArray (a,a) (a -> b) deriving ()
-- context omitted to match builtin version

-- Our version of a Haskell array:
data Array ix elt = _Array (ix, ix) (Array# elt)

-- And the companion "byte array" type:
data _ByteArray ix = _ByteArray (ix,ix) ByteArray#

instance _CCallable (_ByteArray ix)

{- ToDo: Unboxed arrays:

{- SPECIALIZE data a{Int#} b :: Assoc a b #-}
{- SPECIALIZE data a{Int#} b :: Array a b #-}

-}
