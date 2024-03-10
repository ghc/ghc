{-# LANGUAGE NoImplicitPrelude #-}

{-
This SOURCE-imported hs-boot module cuts a big dependency loop:

module ‘GHC.Stable’
imports module ‘GHC.Ptr’
imports module ‘Numeric’
imports module ‘GHC.Read’
imports module ‘GHC.Unicode’
imports module ‘GHC.Unicode.Internal.Char.UnicodeData.GeneralCategory’
imports module ‘GHC.Unicode.Internal.Bits’
imports module ‘GHC.ByteOrder’
imports module ‘GHC.Generics’
imports module ‘Data.Ord’
imports module ‘Foreign.Storable’
imports module ‘GHC.Stable’
-}

module GHC.Internal.ByteOrder where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Types ()

data ByteOrder
    = BigEndian
    | LittleEndian

targetByteOrder :: ByteOrder
