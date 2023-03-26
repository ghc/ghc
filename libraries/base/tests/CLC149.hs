-- Test the COMPLETE pragmas for SChar, SNat, SSymbol, and TypeRep.
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module CLC149 where

import Data.Kind
import GHC.TypeLits
import Type.Reflection

type Dict :: Constraint -> Type
data Dict c where
  Dict :: c => Dict c

sc :: SChar c -> Dict (KnownChar c)
sc SChar = Dict

sn :: SNat n -> Dict (KnownNat n)
sn SNat = Dict

ss :: SSymbol s -> Dict (KnownSymbol s)
ss SSymbol = Dict

tr :: TypeRep a -> Dict (Typeable a)
tr TypeRep = Dict
