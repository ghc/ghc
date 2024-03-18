{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-orphans -Wincomplete-patterns #-}

-- | Orphan Binary instances for Data.Typeable stuff
module GHC.Utils.Binary.Typeable
   ( getSomeTypeRep
   )
where

import GHC.Prelude

import GHC.Utils.Binary

import GHC.Exts (RuntimeRep(..), VecCount(..), VecElem(..))
import GHC.Exts (Levity(Lifted, Unlifted))
import GHC.Serialized

import Foreign
import Type.Reflection
import Type.Reflection.Unsafe
import Data.Kind (Type)


instance Binary TyCon where
    putNoStack_ bh tc = do
        put_ bh (tyConPackage tc)
        put_ bh (tyConModule tc)
        put_ bh (tyConName tc)
        put_ bh (tyConKindArgs tc)
        put_ bh (tyConKindRep tc)
    get bh =
        mkTyCon <$> get bh <*> get bh <*> get bh <*> get bh <*> get bh

getSomeTypeRep :: BinHandle -> IO SomeTypeRep
getSomeTypeRep bh = do
    tag <- get bh :: IO Word8
    case tag of
        0 -> return $ SomeTypeRep (typeRep :: TypeRep Type)
        1 -> do con <- get bh :: IO TyCon
                ks <- get bh :: IO [SomeTypeRep]
                return $ SomeTypeRep $ mkTrCon con ks
        2 -> do SomeTypeRep f <- getSomeTypeRep bh
                SomeTypeRep x <- getSomeTypeRep bh
                case typeRepKind f of
                  Fun arg res ->
                      case arg `eqTypeRep` typeRepKind x of
                        Just HRefl ->
                            case typeRepKind res `eqTypeRep` (typeRep :: TypeRep Type) of
                              Just HRefl -> return $ SomeTypeRep $ mkTrApp f x
                              _ -> failure "Kind mismatch in type application" []
                        _ -> failure "Kind mismatch in type application"
                             [ "    Found argument of kind: " ++ show (typeRepKind x)
                             , "    Where the constructor:  " ++ show f
                             , "    Expects kind:           " ++ show arg
                             ]
                  _ -> failure "Applied non-arrow"
                       [ "    Applied type: " ++ show f
                       , "    To argument:  " ++ show x
                       ]
        _ -> failure "Invalid SomeTypeRep" []
  where
    failure description info =
        fail $ unlines $ [ "Binary.getSomeTypeRep: "++description ]
                      ++ map ("    "++) info

instance Binary SomeTypeRep where
    putNoStack_ bh (SomeTypeRep rep) = putTypeRep bh rep
    get = getSomeTypeRep

instance (Typeable k, Typeable a) => Binary (TypeRep (a :: k)) where
    putNoStack_ = putTypeRep
    get bh = do
        SomeTypeRep rep <- getSomeTypeRep bh
        case rep `eqTypeRep` expected of
            Just HRefl -> pure rep
            Nothing    -> fail $ unlines
                               [ "Binary: Type mismatch"
                               , "    Deserialized type: " ++ show rep
                               , "    Expected type:     " ++ show expected
                               ]
     where expected = typeRep :: TypeRep a


instance Binary VecCount where
    putNoStack_ bh = putByte bh . fromIntegral . fromEnum
    get bh = toEnum . fromIntegral <$> getByte bh

instance Binary VecElem where
    putNoStack_ bh = putByte bh . fromIntegral . fromEnum
    get bh = toEnum . fromIntegral <$> getByte bh

instance Binary RuntimeRep where
    putNoStack_ bh (VecRep a b)    = putByte bh 0 >> put_ bh a >> put_ bh b
    putNoStack_ bh (TupleRep reps) = putByte bh 1 >> put_ bh reps
    putNoStack_ bh (SumRep reps)   = putByte bh 2 >> put_ bh reps
    putNoStack_ bh (BoxedRep Lifted)   = putByte bh 3
    putNoStack_ bh (BoxedRep Unlifted) = putByte bh 4
    putNoStack_ bh IntRep          = putByte bh 5
    putNoStack_ bh WordRep         = putByte bh 6
    putNoStack_ bh Int64Rep        = putByte bh 7
    putNoStack_ bh Word64Rep       = putByte bh 8
    putNoStack_ bh AddrRep         = putByte bh 9
    putNoStack_ bh FloatRep        = putByte bh 10
    putNoStack_ bh DoubleRep       = putByte bh 11
    putNoStack_ bh Int8Rep         = putByte bh 12
    putNoStack_ bh Word8Rep        = putByte bh 13
    putNoStack_ bh Int16Rep        = putByte bh 14
    putNoStack_ bh Word16Rep       = putByte bh 15
    putNoStack_ bh Int32Rep        = putByte bh 16
    putNoStack_ bh Word32Rep       = putByte bh 17

    get bh = do
        tag <- getByte bh
        case tag of
          0  -> VecRep <$> get bh <*> get bh
          1  -> TupleRep <$> get bh
          2  -> SumRep <$> get bh
          3  -> pure (BoxedRep Lifted)
          4  -> pure (BoxedRep Unlifted)
          5  -> pure IntRep
          6  -> pure WordRep
          7  -> pure Int64Rep
          8  -> pure Word64Rep
          9  -> pure AddrRep
          10 -> pure FloatRep
          11 -> pure DoubleRep
          12 -> pure Int8Rep
          13 -> pure Word8Rep
          14 -> pure Int16Rep
          15 -> pure Word16Rep
          16 -> pure Int32Rep
          17 -> pure Word32Rep
          _  -> fail "Binary.putRuntimeRep: invalid tag"

instance Binary KindRep where
    putNoStack_ bh (KindRepTyConApp tc k) = putByte bh 0 >> put_ bh tc >> put_ bh k
    putNoStack_ bh (KindRepVar bndr) = putByte bh 1 >> put_ bh bndr
    putNoStack_ bh (KindRepApp a b) = putByte bh 2 >> put_ bh a >> put_ bh b
    putNoStack_ bh (KindRepFun a b) = putByte bh 3 >> put_ bh a >> put_ bh b
    putNoStack_ bh (KindRepTYPE r) = putByte bh 4 >> put_ bh r
    putNoStack_ bh (KindRepTypeLit sort r) = putByte bh 5 >> put_ bh sort >> put_ bh r

    get bh = do
        tag <- getByte bh
        case tag of
          0 -> KindRepTyConApp <$> get bh <*> get bh
          1 -> KindRepVar <$> get bh
          2 -> KindRepApp <$> get bh <*> get bh
          3 -> KindRepFun <$> get bh <*> get bh
          4 -> KindRepTYPE <$> get bh
          5 -> KindRepTypeLit <$> get bh <*> get bh
          _ -> fail "Binary.putKindRep: invalid tag"

instance Binary TypeLitSort where
    putNoStack_ bh TypeLitSymbol = putByte bh 0
    putNoStack_ bh TypeLitNat = putByte bh 1
    putNoStack_ bh TypeLitChar = putByte bh 2
    get bh = do
        tag <- getByte bh
        case tag of
          0 -> pure TypeLitSymbol
          1 -> pure TypeLitNat
          2 -> pure TypeLitChar
          _ -> fail "Binary.putTypeLitSort: invalid tag"

putTypeRep :: BinHandle -> TypeRep a -> IO ()
putTypeRep bh rep -- Handle Type specially since it's so common
  | Just HRefl <- rep `eqTypeRep` (typeRep :: TypeRep Type)
  = put_ bh (0 :: Word8)
putTypeRep bh (Con' con ks) = do
    putNoStack_ bh (1 :: Word8)
    putNoStack_ bh con
    putNoStack_ bh ks
putTypeRep bh (App f x) = do
    putNoStack_ bh (2 :: Word8)
    putTypeRep bh f
    putTypeRep bh x

instance Binary Serialized where
    putNoStack_ bh (Serialized the_type bytes) = do
        put_ bh the_type
        put_ bh bytes
    get bh = do
        the_type <- get bh
        bytes <- get bh
        return (Serialized the_type bytes)
