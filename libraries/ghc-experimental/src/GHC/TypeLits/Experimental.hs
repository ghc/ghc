{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
module GHC.TypeLits.Experimental (
    appendSSymbol,
    consSSymbol,
    sCharToSNat,
    sNatToSChar,
) where

import GHC.Internal.TypeLits
import Data.Char (ord, chr)

appendSSymbol :: SSymbol a -> SSymbol b -> SSymbol (AppendSymbol a b)
appendSSymbol (UnsafeSSymbol a) (UnsafeSSymbol b) = UnsafeSSymbol (a ++ b)

consSSymbol :: SChar a -> SSymbol b -> SSymbol (ConsSymbol a b)
consSSymbol (UnsafeSChar a) (UnsafeSSymbol b) = UnsafeSSymbol (a : b)

sCharToSNat :: SChar a -> SNat (CharToNat a)
sCharToSNat (UnsafeSChar a) = UnsafeSNat (fromIntegral (ord a))

sNatToSChar :: (n <= 1114111) => SNat n -> SChar (NatToChar n)
sNatToSChar (UnsafeSNat n) = UnsafeSChar (chr (fromIntegral n))
