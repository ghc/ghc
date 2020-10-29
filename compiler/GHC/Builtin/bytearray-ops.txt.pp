primop IndexByteArrayOp_Char "indexCharArray#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 8-bit character; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_WideChar "indexWideCharArray#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 31-bit character; offset in 4-byte words.}
   with can_fail = True

primop IndexByteArrayOp_Int "indexIntArray#" GenPrimOp
   ByteArray# -> Int# -> Int#
   with can_fail = True

primop IndexByteArrayOp_Word "indexWordArray#" GenPrimOp
   ByteArray# -> Int# -> Word#
   with can_fail = True

primop IndexByteArrayOp_Addr "indexAddrArray#" GenPrimOp
   ByteArray# -> Int# -> Addr#
   with can_fail = True

primop IndexByteArrayOp_Float "indexFloatArray#" GenPrimOp
   ByteArray# -> Int# -> Float#
   with can_fail = True

primop IndexByteArrayOp_Double "indexDoubleArray#" GenPrimOp
   ByteArray# -> Int# -> Double#
   with can_fail = True

primop IndexByteArrayOp_StablePtr "indexStablePtrArray#" GenPrimOp
   ByteArray# -> Int# -> StablePtr# a
   with can_fail = True

primop IndexByteArrayOp_Int8 "indexInt8Array#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read 8-bit integer; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Int16 "indexInt16Array#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read 16-bit integer; offset in 16-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Int32 "indexInt32Array#" GenPrimOp
   ByteArray# -> Int# -> INT32
   {Read 32-bit integer; offset in 32-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Int64 "indexInt64Array#" GenPrimOp
   ByteArray# -> Int# -> INT64
   {Read 64-bit integer; offset in 64-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word8 "indexWord8Array#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read 8-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word16 "indexWord16Array#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read 16-bit word; offset in 16-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word32 "indexWord32Array#" GenPrimOp
   ByteArray# -> Int# -> WORD32
   {Read 32-bit word; offset in 32-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word64 "indexWord64Array#" GenPrimOp
   ByteArray# -> Int# -> WORD64
   {Read 64-bit word; offset in 64-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsChar "indexWord8ArrayAsChar#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 8-bit character; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWideChar "indexWord8ArrayAsWideChar#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 31-bit character; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsAddr "indexWord8ArrayAsAddr#" GenPrimOp
   ByteArray# -> Int# -> Addr#
   {Read address; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsFloat "indexWord8ArrayAsFloat#" GenPrimOp
   ByteArray# -> Int# -> Float#
   {Read float; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsDouble "indexWord8ArrayAsDouble#" GenPrimOp
   ByteArray# -> Int# -> Double#
   {Read double; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsStablePtr "indexWord8ArrayAsStablePtr#" GenPrimOp
   ByteArray# -> Int# -> StablePtr# a
   {Read stable pointer; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt16 "indexWord8ArrayAsInt16#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read 16-bit int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt32 "indexWord8ArrayAsInt32#" GenPrimOp
   ByteArray# -> Int# -> INT32
   {Read 32-bit int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt64 "indexWord8ArrayAsInt64#" GenPrimOp
   ByteArray# -> Int# -> INT64
   {Read 64-bit int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt "indexWord8ArrayAsInt#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord16 "indexWord8ArrayAsWord16#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read 16-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord32 "indexWord8ArrayAsWord32#" GenPrimOp
   ByteArray# -> Int# -> WORD32
   {Read 32-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord64 "indexWord8ArrayAsWord64#" GenPrimOp
   ByteArray# -> Int# -> WORD64
   {Read 64-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord "indexWord8ArrayAsWord#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read word; offset in bytes.}
   with can_fail = True

primop  ReadByteArrayOp_Char "readCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   {Read 8-bit character; offset in bytes.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_WideChar "readWideCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   {Read 31-bit character; offset in 4-byte words.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int "readIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   {Read integer; offset in machine words.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word "readWordArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   {Read word; offset in machine words.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Addr "readAddrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Addr# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Float "readFloatArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Float# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Double "readDoubleArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Double# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_StablePtr "readStablePtrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, StablePtr# a #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int8 "readInt8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int16 "readInt16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int32 "readInt32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int64 "readInt64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8 "readWord8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word16 "readWord16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word32 "readWord32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word64 "readWord64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsChar "readWord8ArrayAsChar#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWideChar "readWord8ArrayAsWideChar#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsAddr "readWord8ArrayAsAddr#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Addr# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsFloat "readWord8ArrayAsFloat#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Float# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsDouble "readWord8ArrayAsDouble#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Double# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsStablePtr "readWord8ArrayAsStablePtr#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, StablePtr# a #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt16 "readWord8ArrayAsInt16#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt32 "readWord8ArrayAsInt32#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt64 "readWord8ArrayAsInt64#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt "readWord8ArrayAsInt#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord16 "readWord8ArrayAsWord16#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord32 "readWord8ArrayAsWord32#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord64 "readWord8ArrayAsWord64#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord "readWord8ArrayAsWord#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Char "writeCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   {Write 8-bit character; offset in bytes.}
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_WideChar "writeWideCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   {Write 31-bit character; offset in 4-byte words.}
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int "writeIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word "writeWordArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Addr "writeAddrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Float "writeFloatArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Double "writeDoubleArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_StablePtr "writeStablePtrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int8 "writeInt8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int16 "writeInt16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int32 "writeInt32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int64 "writeInt64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> INT64 -> State# s -> State# s
   with can_fail = True
        has_side_effects = True

primop  WriteByteArrayOp_Word8 "writeWord8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word16 "writeWord16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word32 "writeWord32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word64 "writeWord64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsChar "writeWord8ArrayAsChar#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWideChar "writeWord8ArrayAsWideChar#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsAddr "writeWord8ArrayAsAddr#" GenPrimOp
   MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsFloat "writeWord8ArrayAsFloat#" GenPrimOp
   MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsDouble "writeWord8ArrayAsDouble#" GenPrimOp
   MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsStablePtr "writeWord8ArrayAsStablePtr#" GenPrimOp
   MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt16 "writeWord8ArrayAsInt16#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt32 "writeWord8ArrayAsInt32#" GenPrimOp
   MutableByteArray# s -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt64 "writeWord8ArrayAsInt64#" GenPrimOp
   MutableByteArray# s -> Int# -> INT64 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt "writeWord8ArrayAsInt#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord16 "writeWord8ArrayAsWord16#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord32 "writeWord8ArrayAsWord32#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord64 "writeWord8ArrayAsWord64#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord "writeWord8ArrayAsWord#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True
