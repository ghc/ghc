module NHC.SizedTypes
  -- This module just adds instances of Bits for Int/Word[8,16,32,64]
  ( Int8,  Int16,  Int32,  Int64
  , Word8, Word16, Word32, Word64
  ) where

{- Note explicit braces and semicolons here - layout is corrupted by cpp. -}

{
  import NHC.FFI	(Int8,Int16,Int32,Int64,Word8,Word16,Word32,Word64)
; import Data.Bits

#define SIZED_TYPE(T,BS,S)	\
; FOREIGNS(T)			\
; INSTANCE_BITS(T,BS,S)


#define FOREIGNS(T)	\
; foreign import ccall nhc_prim/**/T/**/And         :: T -> T   -> T	\
; foreign import ccall nhc_prim/**/T/**/Or          :: T -> T   -> T	\
; foreign import ccall nhc_prim/**/T/**/Xor         :: T -> T   -> T	\
; foreign import ccall nhc_prim/**/T/**/Lsh         :: T -> Int -> T	\
; foreign import ccall nhc_prim/**/T/**/Rsh         :: T -> Int -> T	\
; foreign import ccall nhc_prim/**/T/**/Compl       :: T        -> T


#define INSTANCE_BITS(T,BS,S)		\
; instance Bits T where			\
    { (.&.)      = nhc_prim/**/T/**/And	\
    ; (.|.)      = nhc_prim/**/T/**/Or	\
    ; xor        = nhc_prim/**/T/**/Xor	\
    ; complement = nhc_prim/**/T/**/Compl	\
    ; shiftL     = nhc_prim/**/T/**/Lsh	\
    ; shiftR     = nhc_prim/**/T/**/Rsh	\
    ; bitSize  _ = BS			\
    ; isSigned _ = S			\
    }

SIZED_TYPE(Int8,8,True)
SIZED_TYPE(Int16,16,True)
SIZED_TYPE(Int32,32,True)
SIZED_TYPE(Int64,64,True)

SIZED_TYPE(Word8,8,False)
SIZED_TYPE(Word16,16,False)
SIZED_TYPE(Word32,32,False)
SIZED_TYPE(Word64,64,False)

}
