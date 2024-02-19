{-# LANGUAGE MagicHash #-}

module Unsafe.Coerce
    (unsafeCoerce,
     unsafeCoerceUnlifted,
     unsafeCoerceAddr,
     unsafeEqualityProof,
     UnsafeEquality(..),
     unsafeCoerce#
     ) where

import GHC.Internal.Unsafe.Coerce
