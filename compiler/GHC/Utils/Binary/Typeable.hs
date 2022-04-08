{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-orphans -Wincomplete-patterns #-}
#if MIN_VERSION_base(4,16,0)
#define HAS_TYPELITCHAR
#endif

-- | Orphan Binary instances for Data.Typeable stuff
module GHC.Utils.Binary.Typeable
   ( getSomeTypeRep
   )
where

import GHC.Prelude

import GHC.Utils.Binary

import GHC.Exts (RuntimeRep(..), VecCount(..), VecElem(..))
#if __GLASGOW_HASKELL__ >= 901
import GHC.Exts (Levity(Lifted, Unlifted))
#endif
import GHC.Serialized

import Foreign
import Type.Reflection
import Type.Reflection.Unsafe
import Data.Kind (Type)


instance Binary TyCon where
    put_ bh tc = do
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
    put_ bh (SomeTypeRep rep) = putTypeRep bh rep
    get = getSomeTypeRep

instance Typeable a => Binary (TypeRep (a :: k)) where
    put_ = putTypeRep
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
    put_ bh = putByte bh . fromIntegral . fromEnum
    get bh = toEnum . fromIntegral <$> getByte bh

instance Binary VecElem where
    put_ bh = putByte bh . fromIntegral . fromEnum
    get bh = toEnum . fromIntegral <$> getByte bh

instance Binary RuntimeRep where
    put_ bh (VecRep a b)    = putByte bh 0 >> put_ bh a >> put_ bh b
    put_ bh (TupleRep reps) = putByte bh 1 >> put_ bh reps
    put_ bh (SumRep reps)   = putByte bh 2 >> put_ bh reps
#if __GLASGOW_HASKELL__ >= 901
    put_ bh (BoxedRep Lifted)   = putByte bh 3
    put_ bh (BoxedRep Unlifted) = putByte bh 4
#else
    put_ bh LiftedRep       = putByte bh 3
    put_ bh UnliftedRep     = putByte bh 4
#endif
    put_ bh IntRep          = putByte bh 5
    put_ bh WordRep         = putByte bh 6
    put_ bh Int64Rep        = putByte bh 7
    put_ bh Word64Rep       = putByte bh 8
    put_ bh AddrRep         = putByte bh 9
    put_ bh FloatRep        = putByte bh 10
    put_ bh DoubleRep       = putByte bh 11
    put_ bh Int8Rep         = putByte bh 12
    put_ bh Word8Rep        = putByte bh 13
    put_ bh Int16Rep        = putByte bh 14
    put_ bh Word16Rep       = putByte bh 15
    put_ bh Int32Rep        = putByte bh 16
    put_ bh Word32Rep       = putByte bh 17

    get bh = do
        tag <- getByte bh
        case tag of
          0  -> VecRep <$> get bh <*> get bh
          1  -> TupleRep <$> get bh
          2  -> SumRep <$> get bh
#if __GLASGOW_HASKELL__ >= 901
          3  -> pure (BoxedRep Lifted)
          4  -> pure (BoxedRep Unlifted)
#else
          3  -> pure LiftedRep
          4  -> pure UnliftedRep
#endif
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
    put_ bh (KindRepTyConApp tc k) = putByte bh 0 >> put_ bh tc >> put_ bh k
    put_ bh (KindRepVar bndr) = putByte bh 1 >> put_ bh bndr
    put_ bh (KindRepApp a b) = putByte bh 2 >> put_ bh a >> put_ bh b
    put_ bh (KindRepFun a b) = putByte bh 3 >> put_ bh a >> put_ bh b
    put_ bh (KindRepTYPE r) = putByte bh 4 >> put_ bh r
    put_ bh (KindRepTypeLit sort r) = putByte bh 5 >> put_ bh sort >> put_ bh r

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
    put_ bh TypeLitSymbol = putByte bh 0
    put_ bh TypeLitNat = putByte bh 1
#if defined(HAS_TYPELITCHAR)
    put_ bh TypeLitChar = putByte bh 2
#endif
    get bh = do
        tag <- getByte bh
        case tag of
          0 -> pure TypeLitSymbol
          1 -> pure TypeLitNat
#if defined(HAS_TYPELITCHAR)
          2 -> pure TypeLitChar
#endif
          _ -> fail "Binary.putTypeLitSort: invalid tag"

putTypeRep :: BinHandle -> TypeRep a -> IO ()
putTypeRep bh rep -- Handle Type specially since it's so common
  | Just HRefl <- rep `eqTypeRep` (typeRep :: TypeRep Type)
  = put_ bh (0 :: Word8)
putTypeRep bh (Con' con ks) = do
    put_ bh (1 :: Word8)
    put_ bh con
    put_ bh ks
putTypeRep bh (App f x) = do
    put_ bh (2 :: Word8)
    putTypeRep bh f
    putTypeRep bh x
#if __GLASGOW_HASKELL__ < 903
putTypeRep bh (Fun arg res) = do
    put_ bh (3 :: Word8)
    putTypeRep bh arg
    putTypeRep bh res
#endif

instance Binary Serialized where
    put_ bh (Serialized the_type bytes) = do
        put_ bh the_type
        put_ bh bytes
    get bh = do
        the_type <- get bh
        bytes <- get bh
        return (Serialized the_type bytes)
