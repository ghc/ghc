{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- This module is full of orphans, unfortunately
module GHCi.TH.Binary () where

import Data.Binary
import qualified Data.ByteString as B
#if MIN_VERSION_base(4,10,0)
import Type.Reflection
import Type.Reflection.Unsafe
import Data.Kind (Type)
import GHC.Exts (RuntimeRep(..), VecCount, VecElem)
#else
import Data.Typeable
#endif
import GHC.Serialized
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

-- Put these in a separate module because they take ages to compile

instance Binary TH.Loc
instance Binary TH.Name
instance Binary TH.ModName
instance Binary TH.NameFlavour
instance Binary TH.PkgName
instance Binary TH.NameSpace
instance Binary TH.Module
instance Binary TH.Info
instance Binary TH.Type
instance Binary TH.TyLit
instance Binary TH.TyVarBndr
instance Binary TH.Role
instance Binary TH.Lit
instance Binary TH.Range
instance Binary TH.Stmt
instance Binary TH.Pat
instance Binary TH.Exp
instance Binary TH.Dec
instance Binary TH.Overlap
instance Binary TH.DerivClause
instance Binary TH.DerivStrategy
instance Binary TH.Guard
instance Binary TH.Body
instance Binary TH.Match
instance Binary TH.Fixity
instance Binary TH.TySynEqn
instance Binary TH.FamFlavour
instance Binary TH.FunDep
instance Binary TH.AnnTarget
instance Binary TH.RuleBndr
instance Binary TH.Phases
instance Binary TH.RuleMatch
instance Binary TH.Inline
instance Binary TH.Pragma
instance Binary TH.Safety
instance Binary TH.Callconv
instance Binary TH.Foreign
instance Binary TH.Bang
instance Binary TH.SourceUnpackedness
instance Binary TH.SourceStrictness
instance Binary TH.DecidedStrictness
instance Binary TH.FixityDirection
instance Binary TH.OccName
instance Binary TH.Con
instance Binary TH.AnnLookup
instance Binary TH.ModuleInfo
instance Binary TH.Clause
instance Binary TH.InjectivityAnn
instance Binary TH.FamilyResultSig
instance Binary TH.TypeFamilyHead
instance Binary TH.PatSynDir
instance Binary TH.PatSynArgs

-- We need Binary TypeRep for serializing annotations

#if MIN_VERSION_base(4,10,0)
instance Binary VecCount where
    put = putWord8 . fromIntegral . fromEnum
    get = toEnum . fromIntegral <$> getWord8

instance Binary VecElem where
    put = putWord8 . fromIntegral . fromEnum
    get = toEnum . fromIntegral <$> getWord8

instance Binary RuntimeRep where
    put (VecRep a b)    = putWord8 0 >> put a >> put b
    put (TupleRep reps) = putWord8 1 >> put reps
    put (SumRep reps)   = putWord8 2 >> put reps
    put LiftedRep       = putWord8 3
    put UnliftedRep     = putWord8 4
    put IntRep          = putWord8 5
    put WordRep         = putWord8 6
    put Int64Rep        = putWord8 7
    put Word64Rep       = putWord8 8
    put AddrRep         = putWord8 9
    put FloatRep        = putWord8 10
    put DoubleRep       = putWord8 11

    get = do
        tag <- getWord8
        case tag of
          0  -> VecRep <$> get <*> get
          1  -> TupleRep <$> get
          2  -> SumRep <$> get
          3  -> pure LiftedRep
          4  -> pure UnliftedRep
          5  -> pure IntRep
          6  -> pure WordRep
          7  -> pure Int64Rep
          8  -> pure Word64Rep
          9  -> pure AddrRep
          10 -> pure FloatRep
          11 -> pure DoubleRep
          _  -> fail "GHCi.TH.Binary.putRuntimeRep: invalid tag"

instance Binary TyCon where
    put tc = do
        put (tyConPackage tc)
        put (tyConModule tc)
        put (tyConName tc)
        put (tyConKindArgs tc)
        put (tyConKindRep tc)
    get = mkTyCon <$> get <*> get <*> get <*> get <*> get

instance Binary KindRep where
    put (KindRepTyConApp tc k) = putWord8 0 >> put tc >> put k
    put (KindRepVar bndr) = putWord8 1 >> put bndr
    put (KindRepApp a b) = putWord8 2 >> put a >> put b
    put (KindRepFun a b) = putWord8 3 >> put a >> put b
    put (KindRepTYPE r) = putWord8 4 >> put r
    put (KindRepTypeLit sort r) = putWord8 5 >> put sort >> put r
    put _ = fail "GHCi.TH.Binary.putKindRep: Impossible"

    get = do
        tag <- getWord8
        case tag of
          0 -> KindRepTyConApp <$> get <*> get
          1 -> KindRepVar <$> get
          2 -> KindRepApp <$> get <*> get
          3 -> KindRepFun <$> get <*> get
          4 -> KindRepTYPE <$> get
          5 -> KindRepTypeLit <$> get <*> get
          _ -> fail "GHCi.TH.Binary.putKindRep: invalid tag"

instance Binary TypeLitSort where
    put TypeLitSymbol = putWord8 0
    put TypeLitNat = putWord8 1
    get = do
        tag <- getWord8
        case tag of
          0 -> pure TypeLitSymbol
          1 -> pure TypeLitNat
          _ -> fail "GHCi.TH.Binary.putTypeLitSort: invalid tag"

putTypeRep :: TypeRep a -> Put
-- Special handling for TYPE, (->), and RuntimeRep due to recursive kind
-- relations.
-- See Note [Mutually recursive representations of primitive types]
putTypeRep rep  -- Handle Type specially since it's so common
  | Just HRefl <- rep `eqTypeRep` (typeRep :: TypeRep Type)
  = put (0 :: Word8)
putTypeRep (Con' con ks) = do
    put (1 :: Word8)
    put con
    put ks
putTypeRep (App f x) = do
    put (2 :: Word8)
    putTypeRep f
    putTypeRep x
putTypeRep (Fun arg res) = do
    put (3 :: Word8)
    putTypeRep arg
    putTypeRep res
putTypeRep _ = fail "GHCi.TH.Binary.putTypeRep: Impossible"

getSomeTypeRep :: Get SomeTypeRep
getSomeTypeRep = do
    tag <- get :: Get Word8
    case tag of
        0 -> return $ SomeTypeRep (typeRep :: TypeRep Type)
        1 -> do con <- get :: Get TyCon
                ks <- get :: Get [SomeTypeRep]
                return $ SomeTypeRep $ mkTrCon con ks
        2 -> do SomeTypeRep f <- getSomeTypeRep
                SomeTypeRep x <- getSomeTypeRep
                case typeRepKind f of
                  Fun arg res ->
                      case arg `eqTypeRep` typeRepKind x of
                        Just HRefl -> do
                            case typeRepKind res `eqTypeRep` (typeRep :: TypeRep Type) of
                                Just HRefl -> return $ SomeTypeRep $ mkTrApp f x
                                _ -> failure "Kind mismatch" []
                        _ -> failure "Kind mismatch"
                             [ "Found argument of kind:      " ++ show (typeRepKind x)
                             , "Where the constructor:       " ++ show f
                             , "Expects an argument of kind: " ++ show arg
                             ]
                  _ -> failure "Applied non-arrow type"
                       [ "Applied type: " ++ show f
                       , "To argument:  " ++ show x
                       ]
        3 -> do SomeTypeRep arg <- getSomeTypeRep
                SomeTypeRep res <- getSomeTypeRep
                case typeRepKind arg `eqTypeRep` (typeRep :: TypeRep Type) of
                  Just HRefl ->
                      case typeRepKind res `eqTypeRep` (typeRep :: TypeRep Type) of
                        Just HRefl -> return $ SomeTypeRep $ Fun arg res
                        Nothing -> failure "Kind mismatch" []
                  Nothing -> failure "Kind mismatch" []
        _ -> failure "Invalid SomeTypeRep" []
  where
    failure description info =
        fail $ unlines $ [ "GHCi.TH.Binary.getSomeTypeRep: "++description ]
                      ++ map ("    "++) info

instance Typeable a => Binary (TypeRep (a :: k)) where
    put = putTypeRep
    get = do
        SomeTypeRep rep <- getSomeTypeRep
        case rep `eqTypeRep` expected of
          Just HRefl -> pure rep
          Nothing    -> fail $ unlines
                        [ "GHCi.TH.Binary: Type mismatch"
                        , "    Deserialized type: " ++ show rep
                        , "    Expected type:     " ++ show expected
                        ]
     where expected = typeRep :: TypeRep a

instance Binary SomeTypeRep where
    put (SomeTypeRep rep) = putTypeRep rep
    get = getSomeTypeRep
#else
instance Binary TyCon where
    put tc = put (tyConPackage tc) >> put (tyConModule tc) >> put (tyConName tc)
    get = mkTyCon3 <$> get <*> get <*> get

instance Binary TypeRep where
    put type_rep = put (splitTyConApp type_rep)
    get = do
        (ty_con, child_type_reps) <- get
        return (mkTyConApp ty_con child_type_reps)
#endif

instance Binary Serialized where
    put (Serialized tyrep wds) = put tyrep >> put (B.pack wds)
    get = Serialized <$> get <*> (B.unpack <$> get)
