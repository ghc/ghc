module AccessOps (byteArrayAccessOps, addrAccessOps) where

import Syntax

data ElementType = MkElementType
  { elt_name :: String
  , elt_rep_ty :: Ty
  , elt_desc :: String
  , elt_width :: Maybe Int
  }

machWord :: Maybe Int
machWord = Nothing

strToTy :: String -> Ty
strToTy s = TyApp (TyCon s) []

elementTypes :: [ElementType]
elementTypes =
  [ MkElementType "Char"      (strToTy "Char#"  ) "an 8-bit character"             (Just 1)
  , MkElementType "WideChar"  (strToTy "Char#"  ) "a 32-bit character"             (Just 4)
  , MkElementType "Int"       (strToTy "Int#"   ) "a word-sized integer"           machWord
  , MkElementType "Word"      (strToTy "Word#"  ) "a word-sized unsigned integer"  machWord
  , MkElementType "Addr"      (strToTy "Addr#"  ) "a machine address"              machWord
  , MkElementType "Float"     (strToTy "Float#" ) "a single-precision floating-point value"  (Just 4)
  , MkElementType "Double"    (strToTy "Double#") "a double-precision floating-point value"  (Just 8)
  , MkElementType "StablePtr" (TyApp (TyCon "StablePtr#") [TyVar "a"])
                  "a 'StablePtr#' value"  machWord
  ] ++ do
  n <- [8, 16, 32, 64]
  let mkDesc suff = (if n == 8 then "an " else "a ") ++ shows n suff
  [ MkElementType
      { elt_name = "Int" ++ show n
      , elt_rep_ty = strToTy $ "Int" ++ shows n "#"
      , elt_desc = mkDesc "-bit signed integer"
      , elt_width = Just (n `quot` 8)
      },
    MkElementType
      { elt_name = "Word" ++ show n
      , elt_rep_ty = strToTy $ "Word" ++ shows n "#"
      , elt_desc = mkDesc "-bit unsigned integer"
      , elt_width = Just (n `quot` 8)
      }
    ]

unalignedElementTypes :: [ElementType]
unalignedElementTypes
  = filter (\e -> elt_name e `notElem` ["Int8", "Word8"]) elementTypes
--unalignedElementTypes = filter (\e -> elt_width e /= Just 1) elementTypes

prettyOffset :: ElementType -> String
prettyOffset e = case elt_width e of
  Nothing -> "machine words"
  Just 1  -> "bytes"
  Just n  -> shows n "-byte words"

getAlignWarn :: ElementType -> String
getAlignWarn e = case elt_width e of
  Just 1 -> ""
  _ -> "On some platforms, the access may fail\n"
       ++ "for an insufficiently aligned @Addr#@."

mutableByteArrayS :: Ty
mutableByteArrayS = TyApp (TyCon "MutableByteArray#") [TyVar "s"]

stateS :: Ty
stateS = TyApp (TyCon "State#") [TyVar "s"]

readResTy :: ElementType -> Ty
readResTy e = TyF stateS (TyUTup [stateS, elt_rep_ty e])

writeResTy :: ElementType -> Ty
writeResTy e = TyF (elt_rep_ty e) (TyF stateS stateS)



mkIndexByteArrayOp :: ElementType -> Entry
mkIndexByteArrayOp e = PrimOpSpec
  { cons = "IndexByteArrayOp_" ++ elt_name e
  , name = "index" ++ elt_name e ++ "Array#"
  , ty = TyF (strToTy "ByteArray#")
       $ TyF (strToTy "Int#")
             (elt_rep_ty e)
  , cat = GenPrimOp
  , desc = "Read " ++ elt_desc e ++ " from immutable array; offset in " ++ prettyOffset e ++ "."
  , opts = [OptionEffect CanFail]
  }

mkUnalignedIndexByteArrayOp :: ElementType -> Entry
mkUnalignedIndexByteArrayOp e = PrimOpSpec
  { cons = "IndexByteArrayOp_Word8As" ++ elt_name e
  , name = "indexWord8ArrayAs" ++ elt_name e ++ "#"
  , ty = TyF (strToTy "ByteArray#")
       $ TyF (strToTy "Int#")
             (elt_rep_ty e)
  , cat = GenPrimOp
  , desc = "Read " ++ elt_desc e ++ " from immutable array; offset in bytes."
  , opts = [OptionEffect CanFail]
  }

mkReadByteArrayOp :: ElementType -> Entry
mkReadByteArrayOp e = PrimOpSpec
  { cons = "ReadByteArrayOp_" ++ elt_name e
  , name = "read" ++ elt_name e ++ "Array#"
  , ty = TyF mutableByteArrayS
       $ TyF (strToTy "Int#")
       $ readResTy e
  , cat = GenPrimOp
  , desc = "Read " ++ elt_desc e ++ " from mutable array; offset in " ++ prettyOffset e ++ "."
  , opts = [ OptionEffect ReadWriteEffect
           , OptionCanFailWarnFlag YesWarnCanFail
           , OptionTrue "is_discardable_mutable_read"
           ]
  }

mkUnalignedReadByteArrayOp :: ElementType -> Entry
mkUnalignedReadByteArrayOp e = PrimOpSpec
  { cons = "ReadByteArrayOp_Word8As" ++ elt_name e
  , name = "readWord8ArrayAs" ++ elt_name e ++ "#"
  , ty = TyF mutableByteArrayS
       $ TyF (strToTy "Int#")
       $ readResTy e
  , cat = GenPrimOp
  , desc = "Read " ++ elt_desc e ++ " from mutable array; offset in bytes."
  , opts = [ OptionEffect ReadWriteEffect
           , OptionCanFailWarnFlag YesWarnCanFail
           , OptionTrue "is_discardable_mutable_read"
           ]
  }

mkWriteByteArrayOp :: ElementType -> Entry
mkWriteByteArrayOp e = PrimOpSpec
  { cons = "WriteByteArrayOp_" ++ elt_name e
  , name = "write" ++ elt_name e ++ "Array#"
  , ty = TyF mutableByteArrayS
       $ TyF (strToTy "Int#")
       $ writeResTy e
  , cat = GenPrimOp
  , desc = "Write " ++ elt_desc e ++ " to mutable array; offset in " ++ prettyOffset e ++ "."
  , opts = [OptionEffect ReadWriteEffect, OptionCanFailWarnFlag YesWarnCanFail]
  }

mkUnalignedWriteByteArrayOp :: ElementType -> Entry
mkUnalignedWriteByteArrayOp e = PrimOpSpec
  { cons = "WriteByteArrayOp_Word8As" ++ elt_name e
  , name = "writeWord8ArrayAs" ++ elt_name e ++ "#"
  , ty = TyF mutableByteArrayS
       $ TyF (strToTy "Int#")
       $ writeResTy e
  , cat = GenPrimOp
  , desc = "Write " ++ elt_desc e ++ " to mutable array; offset in bytes."
  , opts = [OptionEffect ReadWriteEffect, OptionCanFailWarnFlag YesWarnCanFail]
  }


byteArrayAccessOps :: [Entry]
byteArrayAccessOps
  =  map mkIndexByteArrayOp elementTypes
  ++ map mkUnalignedIndexByteArrayOp unalignedElementTypes
  ++ map mkReadByteArrayOp elementTypes
  ++ map mkUnalignedReadByteArrayOp unalignedElementTypes
  ++ map mkWriteByteArrayOp elementTypes
  ++ map mkUnalignedWriteByteArrayOp unalignedElementTypes



mkIndexOffAddrOp :: ElementType -> Entry
mkIndexOffAddrOp e = PrimOpSpec
  { cons = "IndexOffAddrOp_" ++ elt_name e
  , name = "index" ++ elt_name e ++ "OffAddr#"
  , ty = TyF (strToTy "Addr#")
       $ TyF (strToTy "Int#")
             (elt_rep_ty e)
  , cat = GenPrimOp
  , desc = "Read " ++ elt_desc e ++ " from immutable address; offset in " ++ prettyOffset e ++ ".\n\n"
           ++ getAlignWarn e
  , opts = [OptionEffect CanFail]
  }

mkUnalignedIndexOffAddrOp :: ElementType -> Entry
mkUnalignedIndexOffAddrOp e = PrimOpSpec
  { cons = "IndexOffAddrOp_Word8As" ++ elt_name e
  , name = "indexWord8OffAddrAs" ++ elt_name e ++ "#"
  , ty = TyF (strToTy "Addr#")
       $ TyF (strToTy "Int#")
             (elt_rep_ty e)
  , cat = GenPrimOp
  , desc = "Read " ++ elt_desc e ++ " from immutable address; offset in bytes."
  , opts = [OptionEffect CanFail]
  }

mkReadOffAddrOp :: ElementType -> Entry
mkReadOffAddrOp e = PrimOpSpec
  { cons = "ReadOffAddrOp_" ++ elt_name e
  , name = "read" ++ elt_name e ++ "OffAddr#"
  , ty = TyF (strToTy "Addr#")
       $ TyF (strToTy "Int#")
       $ readResTy e
  , cat = GenPrimOp
  , desc = "Read " ++ elt_desc e ++ " from mutable address; offset in " ++ prettyOffset e ++ ".\n\n"
           ++ getAlignWarn e
  , opts = [ OptionEffect ReadWriteEffect
           , OptionCanFailWarnFlag YesWarnCanFail
           , OptionTrue "is_discardable_mutable_read"
           ]
  }

mkUnalignedReadOffAddrOp :: ElementType -> Entry
mkUnalignedReadOffAddrOp e = PrimOpSpec
  { cons = "ReadOffAddrOp_Word8As" ++ elt_name e
  , name = "readWord8OffAddrAs" ++ elt_name e ++ "#"
  , ty = TyF (strToTy "Addr#")
       $ TyF (strToTy "Int#")
       $ readResTy e
  , cat = GenPrimOp
  , desc = "Read " ++ elt_desc e ++ " from mutable address; offset in bytes."
  , opts = [ OptionEffect ReadWriteEffect
           , OptionCanFailWarnFlag YesWarnCanFail
           , OptionTrue "is_discardable_mutable_read"
           ]
  }

mkWriteOffAddrOp :: ElementType -> Entry
mkWriteOffAddrOp e = PrimOpSpec
  { cons = "WriteOffAddrOp_" ++ elt_name e
  , name = "write" ++ elt_name e ++ "OffAddr#"
  , ty = TyF (strToTy "Addr#")
       $ TyF (strToTy "Int#")
       $ writeResTy e
  , cat = GenPrimOp
  , desc = "Write " ++ elt_desc e ++ " to mutable address; offset in " ++ prettyOffset e ++ ".\n\n"
           ++ getAlignWarn e
  , opts = [OptionEffect ReadWriteEffect, OptionCanFailWarnFlag YesWarnCanFail]
  }

mkUnalignedWriteOffAddrOp :: ElementType -> Entry
mkUnalignedWriteOffAddrOp e = PrimOpSpec
  { cons = "WriteOffAddrOp_Word8As" ++ elt_name e
  , name = "writeWord8OffAddrAs" ++ elt_name e ++ "#"
  , ty = TyF (strToTy "Addr#")
       $ TyF (strToTy "Int#")
       $ writeResTy e
  , cat = GenPrimOp
  , desc = "Write " ++ elt_desc e ++ " to mutable address; offset in bytes."
  , opts = [OptionEffect ReadWriteEffect, OptionCanFailWarnFlag YesWarnCanFail]
  }


addrAccessOps :: [Entry]
addrAccessOps
  =  map mkIndexOffAddrOp elementTypes
  ++ map mkUnalignedIndexOffAddrOp unalignedElementTypes
  ++ map mkReadOffAddrOp elementTypes
  ++ map mkUnalignedReadOffAddrOp unalignedElementTypes
  ++ map mkWriteOffAddrOp elementTypes
  ++ map mkUnalignedWriteOffAddrOp unalignedElementTypes
