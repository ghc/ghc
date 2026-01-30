{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ExtendedLiterals #-}

module T26862 where

type TCharPrim    = 'x'#
type TStringPrim  = "hello"#
type TIntPrim     = 1#
type TWordPrim    = 1##
type TInt8Prim    = 1#Int8
type TWord8Prim   = 1#Word8
type TInt16Prim   = 1#Int16
type TWord16Prim  = 1#Word16
type TInt32Prim   = 1#Int32
type TWord32Prim  = 1#Word32
type TInt64Prim   = 1#Int64
type TWord64Prim  = 1#Word64
type TFloatPrim   = 1.0#
type TDoublePrim  = 1.0##
type TDouble      = 1.0