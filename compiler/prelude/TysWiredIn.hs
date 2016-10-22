{-
(c) The GRASP Project, Glasgow University, 1994-1998

\section[TysWiredIn]{Wired-in knowledge about {\em non-primitive} types}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module is about types that can be defined in Haskell, but which
--   must be wired into the compiler nonetheless.  C.f module TysPrim
module TysWiredIn (
        -- * Helper functions defined here
        mkWiredInTyConName, -- This is used in TcTypeNats to define the
                            -- built-in functions for evaluation.

        mkWiredInIdName,    -- used in MkId

        mkFunKind, mkForAllKind,

        -- * All wired in things
        wiredInTyCons, isBuiltInOcc_maybe,

        -- * Bool
        boolTy, boolTyCon, boolTyCon_RDR, boolTyConName,
        trueDataCon,  trueDataConId,  true_RDR,
        falseDataCon, false_RDR,
        promotedFalseDataCon, promotedTrueDataCon,

        -- * Ordering
        orderingTyCon,
        ltDataCon, ltDataConId,
        eqDataCon, eqDataConId,
        gtDataCon, gtDataConId,
        promotedLTDataCon, promotedEQDataCon, promotedGTDataCon,

        -- * Char
        charTyCon, charDataCon, charTyCon_RDR,
        charTy, stringTy, charTyConName,

        -- * Double
        doubleTyCon, doubleDataCon, doubleTy, doubleTyConName,

        -- * Float
        floatTyCon, floatDataCon, floatTy, floatTyConName,

        -- * Int
        intTyCon, intDataCon, intTyCon_RDR, intDataCon_RDR, intTyConName,
        intTy,

        -- * Word
        wordTyCon, wordDataCon, wordTyConName, wordTy,

        -- * Word8
        word8TyCon, word8DataCon, word8TyConName, word8Ty,

        -- * List
        listTyCon, listTyCon_RDR, listTyConName, listTyConKey,
        nilDataCon, nilDataConName, nilDataConKey,
        consDataCon_RDR, consDataCon, consDataConName,
        promotedNilDataCon, promotedConsDataCon,
        mkListTy,

        -- * Maybe
        maybeTyCon, maybeTyConName,
        nothingDataCon, nothingDataConName, promotedNothingDataCon,
        justDataCon, justDataConName, promotedJustDataCon,

        -- * Tuples
        mkTupleTy, mkBoxedTupleTy,
        tupleTyCon, tupleDataCon, tupleTyConName,
        promotedTupleDataCon,
        unitTyCon, unitDataCon, unitTy, unitTyConKey,
        pairTyCon,
        unboxedUnitTyCon, unboxedUnitDataCon,
        cTupleTyConName, cTupleTyConNames, isCTupleTyConName,

        -- * Any
        anyTyCon, anyTy, anyTypeOfKind,

        -- * Sums
        mkSumTy, sumTyCon, sumDataCon,

        -- * Kinds
        typeNatKindCon, typeNatKind, typeSymbolKindCon, typeSymbolKind,
        isLiftedTypeKindTyConName, liftedTypeKind, constraintKind,
        starKindTyCon, starKindTyConName, unboxedTupleKind,
        unicodeStarKindTyCon, unicodeStarKindTyConName,
        liftedTypeKindTyCon, constraintKindTyCon,

        -- * Parallel arrays
        mkPArrTy,
        parrTyCon, parrFakeCon, isPArrTyCon, isPArrFakeCon,
        parrTyCon_RDR, parrTyConName,

        -- * Equality predicates
        heqTyCon, heqClass, heqDataCon,
        coercibleTyCon, coercibleDataCon, coercibleClass,

        -- * RuntimeRep and friends
        runtimeRepTyCon, vecCountTyCon, vecElemTyCon,

        runtimeRepTy, ptrRepLiftedTy,

        vecRepDataConTyCon, ptrRepUnliftedDataConTyCon,

        voidRepDataConTy, intRepDataConTy,
        wordRepDataConTy, int64RepDataConTy, word64RepDataConTy, addrRepDataConTy,
        floatRepDataConTy, doubleRepDataConTy, unboxedTupleRepDataConTy,
        unboxedSumRepDataConTy,

        vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy,
        vec64DataConTy,

        int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy,
        int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy,
        word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy,
        doubleElemRepDataConTy

    ) where

#include "HsVersions.h"
#include "MachDeps.h"

import {-# SOURCE #-} MkId( mkDataConWorkId, mkSimpleDataConRep, mkDictSelId )

-- friends:
import PrelNames
import TysPrim

-- others:
import CoAxiom
import Id
import Constants        ( mAX_TUPLE_SIZE, mAX_CTUPLE_SIZE, mAX_SUM_SIZE )
import Module           ( Module )
import Type
import DataCon
import {-# SOURCE #-} ConLike
import TyCon
import Class            ( Class, mkClass )
import RdrName
import Name
import NameSet          ( NameSet, mkNameSet, elemNameSet )
import BasicTypes       ( Arity, Boxity(..), TupleSort(..), ConTagZ )
import ForeignCall
import SrcLoc           ( noSrcSpan )
import Unique
import Data.Array
import FastString
import Outputable
import Util
import BooleanFormula   ( mkAnd )

import qualified Data.ByteString.Char8 as BS
#if !MIN_VERSION_bytestring(0,10,8)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
#endif

alpha_tyvar :: [TyVar]
alpha_tyvar = [alphaTyVar]

alpha_ty :: [Type]
alpha_ty = [alphaTy]

{-
Note [Wiring in RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The RuntimeRep type (and friends) in GHC.Types has a bunch of constructors,
making it a pain to wire in. To ease the pain somewhat, we use lists of
the different bits, like Uniques, Names, DataCons. These lists must be
kept in sync with each other. The rule is this: use the order as declared
in GHC.Types. All places where such lists exist should contain a reference
to this Note, so a search for this Note's name should find all the lists.

************************************************************************
*                                                                      *
\subsection{Wired in type constructors}
*                                                                      *
************************************************************************

If you change which things are wired in, make sure you change their
names in PrelNames, so they use wTcQual, wDataQual, etc
-}

-- This list is used only to define PrelInfo.wiredInThings. That in turn
-- is used to initialise the name environment carried around by the renamer.
-- This means that if we look up the name of a TyCon (or its implicit binders)
-- that occurs in this list that name will be assigned the wired-in key we
-- define here.
--
-- Because of their infinite nature, this list excludes tuples, Any and implicit
-- parameter TyCons (see Note [Built-in syntax and the OrigNameCache]).
--
-- See also Note [Known-key names]
wiredInTyCons :: [TyCon]

wiredInTyCons = [ unitTyCon     -- Not treated like other tuples, because
                                -- it's defined in GHC.Base, and there's only
                                -- one of it.  We put it in wiredInTyCons so
                                -- that it'll pre-populate the name cache, so
                                -- the special case in lookupOrigNameCache
                                -- doesn't need to look out for it
                , anyTyCon
                , boolTyCon
                , charTyCon
                , doubleTyCon
                , floatTyCon
                , intTyCon
                , wordTyCon
                , word8TyCon
                , listTyCon
                , maybeTyCon
                , parrTyCon
                , heqTyCon
                , coercibleTyCon
                , typeNatKindCon
                , typeSymbolKindCon
                , runtimeRepTyCon
                , vecCountTyCon
                , vecElemTyCon
                , constraintKindTyCon
                , liftedTypeKindTyCon
                , starKindTyCon
                , unicodeStarKindTyCon
                ]

mkWiredInTyConName :: BuiltInSyntax -> Module -> FastString -> Unique -> TyCon -> Name
mkWiredInTyConName built_in modu fs unique tycon
  = mkWiredInName modu (mkTcOccFS fs) unique
                  (ATyCon tycon)        -- Relevant TyCon
                  built_in

mkWiredInDataConName :: BuiltInSyntax -> Module -> FastString -> Unique -> DataCon -> Name
mkWiredInDataConName built_in modu fs unique datacon
  = mkWiredInName modu (mkDataOccFS fs) unique
                  (AConLike (RealDataCon datacon))    -- Relevant DataCon
                  built_in

mkWiredInIdName :: Module -> FastString -> Unique -> Id -> Name
mkWiredInIdName mod fs uniq id
 = mkWiredInName mod (mkOccNameFS Name.varName fs) uniq (AnId id) UserSyntax

-- See Note [Kind-changing of (~) and Coercible]
-- in libraries/ghc-prim/GHC/Types.hs
heqTyConName, heqDataConName, heqSCSelIdName :: Name
heqTyConName   = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "~~")   heqTyConKey      heqTyCon
heqDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "Eq#")  heqDataConKey heqDataCon
heqSCSelIdName = mkWiredInIdName gHC_TYPES (fsLit "HEq_sc") heqSCSelIdKey heqSCSelId

-- See Note [Kind-changing of (~) and Coercible] in libraries/ghc-prim/GHC/Types.hs
coercibleTyConName, coercibleDataConName, coercibleSCSelIdName :: Name
coercibleTyConName   = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Coercible")  coercibleTyConKey   coercibleTyCon
coercibleDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "MkCoercible") coercibleDataConKey coercibleDataCon
coercibleSCSelIdName = mkWiredInIdName gHC_TYPES (fsLit "Coercible_sc") coercibleSCSelIdKey coercibleSCSelId

charTyConName, charDataConName, intTyConName, intDataConName :: Name
charTyConName     = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Char") charTyConKey charTyCon
charDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "C#") charDataConKey charDataCon
intTyConName      = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Int") intTyConKey   intTyCon
intDataConName    = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "I#") intDataConKey  intDataCon

boolTyConName, falseDataConName, trueDataConName :: Name
boolTyConName     = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Bool") boolTyConKey boolTyCon
falseDataConName  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "False") falseDataConKey falseDataCon
trueDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "True")  trueDataConKey  trueDataCon

listTyConName, nilDataConName, consDataConName :: Name
listTyConName     = mkWiredInTyConName   BuiltInSyntax gHC_TYPES (fsLit "[]") listTyConKey listTyCon
nilDataConName    = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit "[]") nilDataConKey nilDataCon
consDataConName   = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit ":") consDataConKey consDataCon

maybeTyConName, nothingDataConName, justDataConName :: Name
maybeTyConName     = mkWiredInTyConName   UserSyntax gHC_BASE (fsLit "Maybe")
                                          maybeTyConKey maybeTyCon
nothingDataConName = mkWiredInDataConName UserSyntax gHC_BASE (fsLit "Nothing")
                                          nothingDataConKey nothingDataCon
justDataConName    = mkWiredInDataConName UserSyntax gHC_BASE (fsLit "Just")
                                          justDataConKey justDataCon

wordTyConName, wordDataConName, word8TyConName, word8DataConName :: Name
wordTyConName      = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Word")   wordTyConKey     wordTyCon
wordDataConName    = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "W#")     wordDataConKey   wordDataCon
word8TyConName     = mkWiredInTyConName   UserSyntax gHC_WORD  (fsLit "Word8")  word8TyConKey    word8TyCon
word8DataConName   = mkWiredInDataConName UserSyntax gHC_WORD  (fsLit "W8#")    word8DataConKey  word8DataCon

floatTyConName, floatDataConName, doubleTyConName, doubleDataConName :: Name
floatTyConName     = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Float")  floatTyConKey    floatTyCon
floatDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "F#")     floatDataConKey  floatDataCon
doubleTyConName    = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Double") doubleTyConKey   doubleTyCon
doubleDataConName  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "D#")     doubleDataConKey doubleDataCon

-- Any

{-
Note [Any types]
~~~~~~~~~~~~~~~~
The type constructor Any,

    type family Any :: k where { }

It has these properties:

  * Note that 'Any' is kind polymorphic since in some program we may
    need to use Any to fill in a type variable of some kind other than *
    (see #959 for examples).  Its kind is thus `forall k. k``.

  * It is defined in module GHC.Types, and exported so that it is
    available to users.  For this reason it's treated like any other
    wired-in type:
      - has a fixed unique, anyTyConKey,
      - lives in the global name cache

  * It is a *closed* type family, with no instances.  This means that
    if   ty :: '(k1, k2)  we add a given coercion
             g :: ty ~ (Fst ty, Snd ty)
    If Any was a *data* type, then we'd get inconsistency because 'ty'
    could be (Any '(k1,k2)) and then we'd have an equality with Any on
    one side and '(,) on the other. See also #9097 and #9636.

  * When instantiated at a lifted type it is inhabited by at least one value,
    namely bottom

  * You can safely coerce any /lifted/ type to Any, and back with unsafeCoerce.

  * It does not claim to be a *data* type, and that's important for
    the code generator, because the code gen may *enter* a data value
    but never enters a function value.

  * It is wired-in so we can easily refer to it where we don't have a name
    environment (e.g. see Rules.matchRule for one example)

  * If (Any k) is the type of a value, it must be a /lifted/ value. So
    if we have (Any @(TYPE rr)) then rr must be 'PtrRepLifted.  See
    Note [TYPE and RuntimeRep] in TysPrim.  This is a convenient
    invariant, and makes isUnliftedTyCon well-defined; otherwise what
    would (isUnliftedTyCon Any) be?

It's used to instantiate un-constrained type variables after type checking. For
example, 'length' has type

  length :: forall a. [a] -> Int

and the list datacon for the empty list has type

  [] :: forall a. [a]

In order to compose these two terms as @length []@ a type
application is required, but there is no constraint on the
choice.  In this situation GHC uses 'Any',

> length (Any *) ([] (Any *))

Above, we print kinds explicitly, as if with --fprint-explicit-kinds.

The Any tycon used to be quite magic, but we have since been able to
implement it merely with an empty kind polymorphic type family. See #10886 for a
bit of history.
-}


anyTyConName :: Name
anyTyConName =
    mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Any") anyTyConKey anyTyCon

anyTyCon :: TyCon
anyTyCon = mkFamilyTyCon anyTyConName binders res_kind Nothing
                         (ClosedSynFamilyTyCon Nothing)
                         Nothing
                         NotInjective
  where
    binders@[kv] = mkTemplateKindTyConBinders [liftedTypeKind]
    res_kind = mkTyVarTy (binderVar kv)

anyTy :: Type
anyTy = mkTyConTy anyTyCon

anyTypeOfKind :: Kind -> Type
anyTypeOfKind kind = mkTyConApp anyTyCon [kind]

-- Kinds
typeNatKindConName, typeSymbolKindConName :: Name
typeNatKindConName    = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Nat")    typeNatKindConNameKey    typeNatKindCon
typeSymbolKindConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Symbol") typeSymbolKindConNameKey typeSymbolKindCon

constraintKindTyConName :: Name
constraintKindTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Constraint") constraintKindTyConKey   constraintKindTyCon

liftedTypeKindTyConName, starKindTyConName, unicodeStarKindTyConName
  :: Name
liftedTypeKindTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Type") liftedTypeKindTyConKey liftedTypeKindTyCon
starKindTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "*") starKindTyConKey starKindTyCon
unicodeStarKindTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "★") unicodeStarKindTyConKey unicodeStarKindTyCon

runtimeRepTyConName, vecRepDataConName :: Name
runtimeRepTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "RuntimeRep") runtimeRepTyConKey runtimeRepTyCon
vecRepDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "VecRep") vecRepDataConKey vecRepDataCon

-- See Note [Wiring in RuntimeRep]
runtimeRepSimpleDataConNames :: [Name]
runtimeRepSimpleDataConNames
  = zipWith3Lazy mk_special_dc_name
      [ fsLit "PtrRepLifted", fsLit "PtrRepUnlifted"
      , fsLit "VoidRep", fsLit "IntRep"
      , fsLit "WordRep", fsLit "Int64Rep", fsLit "Word64Rep"
      , fsLit "AddrRep", fsLit "FloatRep", fsLit "DoubleRep"
      , fsLit "UnboxedTupleRep", fsLit "UnboxedSumRep" ]
      runtimeRepSimpleDataConKeys
      runtimeRepSimpleDataCons

vecCountTyConName :: Name
vecCountTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "VecCount") vecCountTyConKey vecCountTyCon

-- See Note [Wiring in RuntimeRep]
vecCountDataConNames :: [Name]
vecCountDataConNames = zipWith3Lazy mk_special_dc_name
                         [ fsLit "Vec2", fsLit "Vec4", fsLit "Vec8"
                         , fsLit "Vec16", fsLit "Vec32", fsLit "Vec64" ]
                         vecCountDataConKeys
                         vecCountDataCons

vecElemTyConName :: Name
vecElemTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "VecElem") vecElemTyConKey vecElemTyCon

-- See Note [Wiring in RuntimeRep]
vecElemDataConNames :: [Name]
vecElemDataConNames = zipWith3Lazy mk_special_dc_name
                        [ fsLit "Int8ElemRep", fsLit "Int16ElemRep", fsLit "Int32ElemRep"
                        , fsLit "Int64ElemRep", fsLit "Word8ElemRep", fsLit "Word16ElemRep"
                        , fsLit "Word32ElemRep", fsLit "Word64ElemRep"
                        , fsLit "FloatElemRep", fsLit "DoubleElemRep" ]
                        vecElemDataConKeys
                        vecElemDataCons

mk_special_dc_name :: FastString -> Unique -> DataCon -> Name
mk_special_dc_name fs u dc = mkWiredInDataConName UserSyntax gHC_TYPES fs u dc

parrTyConName, parrDataConName :: Name
parrTyConName   = mkWiredInTyConName   BuiltInSyntax
                    gHC_PARR' (fsLit "[::]") parrTyConKey parrTyCon
parrDataConName = mkWiredInDataConName UserSyntax
                    gHC_PARR' (fsLit "PArr") parrDataConKey parrDataCon

boolTyCon_RDR, false_RDR, true_RDR, intTyCon_RDR, charTyCon_RDR,
    intDataCon_RDR, listTyCon_RDR, consDataCon_RDR, parrTyCon_RDR :: RdrName
boolTyCon_RDR   = nameRdrName boolTyConName
false_RDR       = nameRdrName falseDataConName
true_RDR        = nameRdrName trueDataConName
intTyCon_RDR    = nameRdrName intTyConName
charTyCon_RDR   = nameRdrName charTyConName
intDataCon_RDR  = nameRdrName intDataConName
listTyCon_RDR   = nameRdrName listTyConName
consDataCon_RDR = nameRdrName consDataConName
parrTyCon_RDR   = nameRdrName parrTyConName

{-
************************************************************************
*                                                                      *
\subsection{mkWiredInTyCon}
*                                                                      *
************************************************************************
-}

pcNonEnumTyCon :: Name -> Maybe CType -> [TyVar] -> [DataCon] -> TyCon
-- Not an enumeration
pcNonEnumTyCon = pcTyCon False

-- This function assumes that the types it creates have all parameters at
-- Representational role, and that there is no kind polymorphism.
pcTyCon :: Bool -> Name -> Maybe CType -> [TyVar] -> [DataCon] -> TyCon
pcTyCon is_enum name cType tyvars cons
  = mkAlgTyCon name
                (mkAnonTyConBinders tyvars)
                liftedTypeKind
                (map (const Representational) tyvars)
                cType
                []              -- No stupid theta
                (DataTyCon cons is_enum)
                (VanillaAlgTyCon (mkPrelTyConRepName name))
                False           -- Not in GADT syntax

pcDataCon :: Name -> [TyVar] -> [Type] -> TyCon -> DataCon
pcDataCon n univs = pcDataConWithFixity False n univs []  -- no ex_tvs

pcDataConWithFixity :: Bool      -- ^ declared infix?
                    -> Name      -- ^ datacon name
                    -> [TyVar]   -- ^ univ tyvars
                    -> [TyVar]   -- ^ ex tyvars
                    -> [Type]    -- ^ args
                    -> TyCon
                    -> DataCon
pcDataConWithFixity infx n = pcDataConWithFixity' infx n (dataConWorkerUnique (nameUnique n)) (dataConWrapperUnique (nameUnique n))
                                                  NoRRI
-- The Name's unique is the first of four free uniques;
-- the first is used for the datacon itself,
-- the second is used for the "worker name"
-- the third is used for the "wrapper name"
--
-- To support this the mkPreludeDataConUnique function "allocates"
-- one DataCon unique per pair of Ints.

pcDataConWithFixity' :: Bool -> Name -> Unique -> Unique -> RuntimeRepInfo
                     -> [TyVar] -> [TyVar]
                     -> [Type] -> TyCon -> DataCon
-- The Name should be in the DataName name space; it's the name
-- of the DataCon itself.

pcDataConWithFixity' declared_infix dc_name wrk_key wrp_key rri tyvars ex_tyvars arg_tys tycon
  = data_con
  where
    data_con = mkDataCon dc_name declared_infix prom_info
                (map (const no_bang) arg_tys)
                []      -- No labelled fields
                (mkTyVarBinders Specified tyvars)
                (mkTyVarBinders Specified ex_tyvars)
                []      -- No equality spec
                []      -- No theta
                arg_tys (mkTyConApp tycon (mkTyVarTys tyvars))
                rri
                tycon
                []      -- No stupid theta
                (mkDataConWorkId wrk_name data_con)
                (mkSimpleDataConRep wrp_name data_con)

    no_bang = HsSrcBang Nothing NoSrcUnpack NoSrcStrict

    modu     = ASSERT( isExternalName dc_name )
               nameModule dc_name
    dc_occ   = nameOccName dc_name
    wrk_occ  = mkDataConWorkerOcc dc_occ
    wrk_name = mkWiredInName modu wrk_occ wrk_key
                             (AnId (dataConWorkId data_con)) UserSyntax

    wrp_occ  = mkDataConWrapperOcc dc_occ
    wrp_name = mkWiredInName modu wrp_occ wrp_key
                             (AnId (dataConWrapId data_con)) UserSyntax

    prom_info = mkPrelTyConRepName dc_name


-- used for RuntimeRep and friends
pcSpecialDataCon :: Name -> [Type] -> TyCon -> RuntimeRepInfo -> DataCon
pcSpecialDataCon dc_name arg_tys tycon rri
  = pcDataConWithFixity' False dc_name
                         (dataConWorkerUnique (nameUnique dc_name))
                         (dataConWrapperUnique (nameUnique dc_name))
                         rri
                         [] [] arg_tys tycon

{-
************************************************************************
*                                                                      *
      Kinds
*                                                                      *
************************************************************************
-}

typeNatKindCon, typeSymbolKindCon :: TyCon
-- data Nat
-- data Symbol
typeNatKindCon    = pcTyCon False typeNatKindConName    Nothing [] []
typeSymbolKindCon = pcTyCon False typeSymbolKindConName Nothing [] []

typeNatKind, typeSymbolKind :: Kind
typeNatKind    = mkTyConTy typeNatKindCon
typeSymbolKind = mkTyConTy typeSymbolKindCon

constraintKindTyCon :: TyCon
constraintKindTyCon = pcTyCon False constraintKindTyConName
                              Nothing [] []

liftedTypeKind, constraintKind, unboxedTupleKind :: Kind
liftedTypeKind   = tYPE ptrRepLiftedTy
constraintKind   = mkTyConApp constraintKindTyCon []
unboxedTupleKind = tYPE unboxedTupleRepDataConTy

-- mkFunKind and mkForAllKind are defined here
-- solely so that TyCon can use them via a SOURCE import
mkFunKind :: Kind -> Kind -> Kind
mkFunKind = mkFunTy

mkForAllKind :: TyVar -> ArgFlag -> Kind -> Kind
mkForAllKind = mkForAllTy

{-
************************************************************************
*                                                                      *
                Stuff for dealing with tuples
*                                                                      *
************************************************************************

Note [How tuples work]  See also Note [Known-key names] in PrelNames
~~~~~~~~~~~~~~~~~~~~~~
* There are three families of tuple TyCons and corresponding
  DataCons, expressed by the type BasicTypes.TupleSort:
    data TupleSort = BoxedTuple | UnboxedTuple | ConstraintTuple

* All three families are AlgTyCons, whose AlgTyConRhs is TupleTyCon

* BoxedTuples
    - A wired-in type
    - Data type declarations in GHC.Tuple
    - The data constructors really have an info table

* UnboxedTuples
    - A wired-in type
    - Have a pretend DataCon, defined in GHC.Prim,
      but no actual declaration and no info table

* ConstraintTuples
    - Are known-key rather than wired-in. Reason: it's awkward to
      have all the superclass selectors wired-in.
    - Declared as classes in GHC.Classes, e.g.
         class (c1,c2) => (c1,c2)
    - Given constraints: the superclasses automatically become available
    - Wanted constraints: there is a built-in instance
         instance (c1,c2) => (c1,c2)
    - Currently just go up to 16; beyond that
      you have to use manual nesting
    - Their OccNames look like (%,,,%), so they can easily be
      distinguished from term tuples.  But (following Haskell) we
      pretty-print saturated constraint tuples with round parens; see
      BasicTypes.tupleParens.

* In quite a lot of places things are restrcted just to
  BoxedTuple/UnboxedTuple, and then we used BasicTypes.Boxity to distinguish
  E.g. tupleTyCon has a Boxity argument

* When looking up an OccName in the original-name cache
  (IfaceEnv.lookupOrigNameCache), we spot the tuple OccName to make sure
  we get the right wired-in name.  This guy can't tell the difference
  between BoxedTuple and ConstraintTuple (same OccName!), so tuples
  are not serialised into interface files using OccNames at all.

Note [One-tuples]
~~~~~~~~~~~~~~~~~
GHC supports both boxed and unboxed one-tuples:
 - Unboxed one-tuples are sometimes useful when returning a
   single value after CPR analysis
 - A boxed one-tuple is used by DsUtils.mkSelectorBinds, when
   there is just one binder
Basically it keeps everythig uniform.

However the /naming/ of the type/data constructors for one-tuples is a
bit odd:
  3-tuples:  (,,)   (,,)#
  2-tuples:  (,)    (,)#
  1-tuples:  ??
  0-tuples:  ()     ()#

Zero-tuples have used up the logical name. So we use 'Unit' and 'Unit#'
for one-tuples.  So in ghc-prim:GHC.Tuple we see the declarations:
  data ()     = ()
  data Unit a = Unit a
  data (a,b)  = (a,b)

NB (Feb 16): for /constraint/ one-tuples I have 'Unit%' but no class
decl in GHC.Classes, so I think this part may not work properly. But
it's unused I think.
-}

-- | Built in syntax isn't "in scope" so these OccNames map to wired-in Names
-- with BuiltInSyntax. However, this should only be necessary while resolving
-- names produced by Template Haskell splices since we take care to encode
-- built-in syntax names specially in interface files. See
-- Note [Symbol table representation of names].
isBuiltInOcc_maybe :: OccName -> Maybe Name
isBuiltInOcc_maybe occ =
    case name of
      "[]" -> Just $ choose_ns listTyConName nilDataConName
      ":"    -> Just consDataConName
      "[::]" -> Just parrTyConName
      "()"    -> Just $ tup_name Boxed 0
      "(##)"  -> Just $ tup_name Unboxed 0
      _ | Just rest <- "(" `stripPrefix` name
        , (commas, rest') <- BS.span (==',') rest
        , ")" <- rest'
             -> Just $ tup_name Boxed (1+BS.length commas)
      _ | Just rest <- "(#" `stripPrefix` name
        , (commas, rest') <- BS.span (==',') rest
        , "#)" <- rest'
             -> Just $ tup_name Unboxed (1+BS.length commas)
      _ -> Nothing
  where
    -- TODO: Drop when bytestring 0.10.8 can be assumed
#if MIN_VERSION_bytestring(0,10,8)
    stripPrefix = BS.stripPrefix
#else
    stripPrefix bs1@(BSI.PS _ _ l1) bs2
      | bs1 `BS.isPrefixOf` bs2 = Just (BSU.unsafeDrop l1 bs2)
      | otherwise = Nothing
#endif

    name = fastStringToByteString $ occNameFS occ

    choose_ns :: Name -> Name -> Name
    choose_ns tc dc
      | isTcClsNameSpace ns   = tc
      | isDataConNameSpace ns = dc
      | otherwise             = pprPanic "tup_name" (ppr occ)
      where ns = occNameSpace occ

    tup_name boxity arity
      = choose_ns (getName (tupleTyCon   boxity arity))
                  (getName (tupleDataCon boxity arity))

mkTupleOcc :: NameSpace -> Boxity -> Arity -> OccName
-- No need to cache these, the caching is done in mk_tuple
mkTupleOcc ns Boxed   ar = mkOccName ns (mkBoxedTupleStr   ar)
mkTupleOcc ns Unboxed ar = mkOccName ns (mkUnboxedTupleStr ar)

mkCTupleOcc :: NameSpace -> Arity -> OccName
mkCTupleOcc ns ar = mkOccName ns (mkConstraintTupleStr ar)

mkBoxedTupleStr :: Arity -> String
mkBoxedTupleStr 0  = "()"
mkBoxedTupleStr 1  = "Unit"   -- See Note [One-tuples]
mkBoxedTupleStr ar = '(' : commas ar ++ ")"

mkUnboxedTupleStr :: Arity -> String
mkUnboxedTupleStr 0  = "(##)"
mkUnboxedTupleStr 1  = "Unit#"  -- See Note [One-tuples]
mkUnboxedTupleStr ar = "(#" ++ commas ar ++ "#)"

mkConstraintTupleStr :: Arity -> String
mkConstraintTupleStr 0  = "(%%)"
mkConstraintTupleStr 1  = "Unit%"   -- See Note [One-tuples]
mkConstraintTupleStr ar = "(%" ++ commas ar ++ "%)"

commas :: Arity -> String
commas ar = take (ar-1) (repeat ',')

cTupleTyConName :: Arity -> Name
cTupleTyConName arity
  = mkExternalName (mkCTupleTyConUnique arity) gHC_CLASSES
                   (mkCTupleOcc tcName arity) noSrcSpan
  -- The corresponding DataCon does not have a known-key name

cTupleTyConNames :: [Name]
cTupleTyConNames = map cTupleTyConName (0 : [2..mAX_CTUPLE_SIZE])

cTupleTyConNameSet :: NameSet
cTupleTyConNameSet = mkNameSet cTupleTyConNames

isCTupleTyConName :: Name -> Bool
-- Use Type.isCTupleClass where possible
isCTupleTyConName n
 = ASSERT2( isExternalName n, ppr n )
   nameModule n == gHC_CLASSES
   && n `elemNameSet` cTupleTyConNameSet

tupleTyCon :: Boxity -> Arity -> TyCon
tupleTyCon sort i | i > mAX_TUPLE_SIZE = fst (mk_tuple sort i)  -- Build one specially
tupleTyCon Boxed   i = fst (boxedTupleArr   ! i)
tupleTyCon Unboxed i = fst (unboxedTupleArr ! i)

tupleTyConName :: TupleSort -> Arity -> Name
tupleTyConName ConstraintTuple a = cTupleTyConName a
tupleTyConName BoxedTuple      a = tyConName (tupleTyCon Boxed a)
tupleTyConName UnboxedTuple    a = tyConName (tupleTyCon Unboxed a)

promotedTupleDataCon :: Boxity -> Arity -> TyCon
promotedTupleDataCon boxity i = promoteDataCon (tupleDataCon boxity i)

tupleDataCon :: Boxity -> Arity -> DataCon
tupleDataCon sort i | i > mAX_TUPLE_SIZE = snd (mk_tuple sort i)    -- Build one specially
tupleDataCon Boxed   i = snd (boxedTupleArr   ! i)
tupleDataCon Unboxed i = snd (unboxedTupleArr ! i)

boxedTupleArr, unboxedTupleArr :: Array Int (TyCon,DataCon)
boxedTupleArr   = listArray (0,mAX_TUPLE_SIZE) [mk_tuple Boxed   i | i <- [0..mAX_TUPLE_SIZE]]
unboxedTupleArr = listArray (0,mAX_TUPLE_SIZE) [mk_tuple Unboxed i | i <- [0..mAX_TUPLE_SIZE]]

mk_tuple :: Boxity -> Int -> (TyCon,DataCon)
mk_tuple Boxed arity = (tycon, tuple_con)
  where
    tycon = mkTupleTyCon tc_name tc_binders tc_res_kind tc_arity tuple_con
                         BoxedTuple flavour

    tc_binders  = mkTemplateAnonTyConBinders (nOfThem arity liftedTypeKind)
    tc_res_kind = liftedTypeKind
    tc_arity    = arity
    flavour     = VanillaAlgTyCon (mkPrelTyConRepName tc_name)

    dc_tvs     = binderVars tc_binders
    dc_arg_tys = mkTyVarTys dc_tvs
    tuple_con  = pcDataCon dc_name dc_tvs dc_arg_tys tycon

    boxity  = Boxed
    modu    = gHC_TUPLE
    tc_name = mkWiredInName modu (mkTupleOcc tcName boxity arity) tc_uniq
                         (ATyCon tycon) BuiltInSyntax
    dc_name = mkWiredInName modu (mkTupleOcc dataName boxity arity) dc_uniq
                            (AConLike (RealDataCon tuple_con)) BuiltInSyntax
    tc_uniq = mkTupleTyConUnique   boxity arity
    dc_uniq = mkTupleDataConUnique boxity arity

mk_tuple Unboxed arity = (tycon, tuple_con)
  where
    tycon = mkTupleTyCon tc_name tc_binders tc_res_kind tc_arity tuple_con
                         UnboxedTuple flavour

    -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
    -- Kind:  forall (k1:RuntimeRep) (k2:RuntimeRep). TYPE k1 -> TYPE k2 -> #
    tc_binders = mkTemplateTyConBinders (nOfThem arity runtimeRepTy)
                                        (\ks -> map tYPE ks)

    tc_res_kind | arity == 0 = tYPE voidRepDataConTy  -- Nullary unboxed tuple
                | otherwise  = unboxedTupleKind

    tc_arity    = arity * 2
    flavour     = UnboxedAlgTyCon

    dc_tvs     = binderVars tc_binders
    dc_arg_tys = mkTyVarTys (drop arity dc_tvs)
    tuple_con  = pcDataCon dc_name dc_tvs dc_arg_tys tycon

    boxity  = Unboxed
    modu    = gHC_PRIM
    tc_name = mkWiredInName modu (mkTupleOcc tcName boxity arity) tc_uniq
                         (ATyCon tycon) BuiltInSyntax
    dc_name = mkWiredInName modu (mkTupleOcc dataName boxity arity) dc_uniq
                            (AConLike (RealDataCon tuple_con)) BuiltInSyntax
    tc_uniq = mkTupleTyConUnique   boxity arity
    dc_uniq = mkTupleDataConUnique boxity arity

unitTyCon :: TyCon
unitTyCon = tupleTyCon Boxed 0

unitTyConKey :: Unique
unitTyConKey = getUnique unitTyCon

unitDataCon :: DataCon
unitDataCon   = head (tyConDataCons unitTyCon)

pairTyCon :: TyCon
pairTyCon = tupleTyCon Boxed 2

unboxedUnitTyCon :: TyCon
unboxedUnitTyCon = tupleTyCon Unboxed 0

unboxedUnitDataCon :: DataCon
unboxedUnitDataCon = tupleDataCon   Unboxed 0


{- *********************************************************************
*                                                                      *
      Unboxed sums
*                                                                      *
********************************************************************* -}

-- | OccName for n-ary unboxed sum type constructor.
mkSumTyConOcc :: Arity -> OccName
mkSumTyConOcc n = mkOccName tcName str
  where
    -- No need to cache these, the caching is done in mk_sum
    str = '(' : '#' : bars ++ "#)"
    bars = replicate (n-1) '|'

-- | OccName for i-th alternative of n-ary unboxed sum data constructor.
mkSumDataConOcc :: ConTag -> Arity -> OccName
mkSumDataConOcc alt n = mkOccName dataName str
  where
    -- No need to cache these, the caching is done in mk_sum
    str = '(' : '#' : bars alt ++ '_' : bars (n - alt - 1) ++ "#)"
    bars i = replicate i '|'

-- | Type constructor for n-ary unboxed sum.
sumTyCon :: Arity -> TyCon
sumTyCon arity
  | arity > mAX_SUM_SIZE
  = fst (mk_sum arity)  -- Build one specially

  | arity < 2
  = panic ("sumTyCon: Arity starts from 2. (arity: " ++ show arity ++ ")")

  | otherwise
  = fst (unboxedSumArr ! arity)

-- | Data constructor for i-th alternative of a n-ary unboxed sum.
sumDataCon :: ConTag -- Alternative
           -> Arity  -- Arity
           -> DataCon
sumDataCon alt arity
  | alt > arity
  = panic ("sumDataCon: index out of bounds: alt: "
           ++ show alt ++ " > arity " ++ show arity)

  | alt <= 0
  = panic ("sumDataCon: Alts start from 1. (alt: " ++ show alt
           ++ ", arity: " ++ show arity ++ ")")

  | arity < 2
  = panic ("sumDataCon: Arity starts from 2. (alt: " ++ show alt
           ++ ", arity: " ++ show arity ++ ")")

  | arity > mAX_SUM_SIZE
  = snd (mk_sum arity) ! (alt - 1)  -- Build one specially

  | otherwise
  = snd (unboxedSumArr ! arity) ! (alt - 1)

-- | Cached type and data constructors for sums. The outer array is
-- indexed by the arity of the sum and the inner array is indexed by
-- the alternative.
unboxedSumArr :: Array Int (TyCon, Array Int DataCon)
unboxedSumArr = listArray (2,mAX_SUM_SIZE) [mk_sum i | i <- [2..mAX_SUM_SIZE]]

-- | Create type constructor and data constructors for n-ary unboxed sum.
mk_sum :: Arity -> (TyCon, Array ConTagZ DataCon)
mk_sum arity = (tycon, sum_cons)
  where
    tycon   = mkSumTyCon tc_name tc_binders tc_res_kind (arity * 2) tyvars (elems sum_cons)
                         UnboxedAlgTyCon

    tc_binders = mkTemplateTyConBinders (nOfThem arity runtimeRepTy)
                                        (\ks -> map tYPE ks)

    tyvars = mkTemplateTyVars (replicate arity runtimeRepTy ++
                               map (tYPE . mkTyVarTy) (take arity tyvars))

    tc_res_kind = tYPE unboxedSumRepDataConTy

    open_tvs = drop arity tyvars

    tc_name = mkWiredInName gHC_PRIM (mkSumTyConOcc arity) tc_uniq
                            (ATyCon tycon) BuiltInSyntax

    sum_cons = listArray (0,arity-1) [sum_con i | i <- [0..arity-1]]
    sum_con i = let dc = pcDataCon dc_name
                                   tyvars -- univ tyvars
                                   [tyvar_tys !! i] -- arg types
                                   tycon

                    dc_name = mkWiredInName gHC_PRIM
                                            (mkSumDataConOcc i arity)
                                            (dc_uniq i)
                                            (AConLike (RealDataCon dc))
                                            BuiltInSyntax
                in dc
    tyvar_tys = mkTyVarTys open_tvs
    tc_uniq   = mkSumTyConUnique   arity
    dc_uniq i = mkSumDataConUnique i arity

{-
************************************************************************
*                                                                      *
              Equality types and classes
*                                                                      *
********************************************************************* -}

-- See Note [The equality types story] in TysPrim
-- (:~~: :: forall k1 k2 (a :: k1) (b :: k2). a -> b -> Constraint)
heqTyCon, coercibleTyCon :: TyCon
heqClass, coercibleClass :: Class
heqDataCon, coercibleDataCon :: DataCon
heqSCSelId, coercibleSCSelId :: Id

(heqTyCon, heqClass, heqDataCon, heqSCSelId)
  = (tycon, klass, datacon, sc_sel_id)
  where
    tycon     = mkClassTyCon heqTyConName binders roles
                             rhs klass
                             (mkPrelTyConRepName heqTyConName)
    klass     = mk_class tycon sc_pred sc_sel_id
    datacon   = pcDataCon heqDataConName tvs [sc_pred] tycon

    -- Kind: forall k1 k2. k1 -> k2 -> Constraint
    binders   = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] (\ks -> ks)
    roles     = [Nominal, Nominal, Nominal, Nominal]
    rhs       = DataTyCon { data_cons = [datacon], is_enum = False }

    tvs       = binderVars binders
    sc_pred   = mkTyConApp eqPrimTyCon (mkTyVarTys tvs)
    sc_sel_id = mkDictSelId heqSCSelIdName klass

(coercibleTyCon, coercibleClass, coercibleDataCon, coercibleSCSelId)
  = (tycon, klass, datacon, sc_sel_id)
  where
    tycon     = mkClassTyCon coercibleTyConName binders roles
                             rhs klass
                             (mkPrelTyConRepName coercibleTyConName)
    klass     = mk_class tycon sc_pred sc_sel_id
    datacon   = pcDataCon coercibleDataConName tvs [sc_pred] tycon

    -- Kind: forall k. k -> k -> Constraint
    binders   = mkTemplateTyConBinders [liftedTypeKind] (\[k] -> [k,k])
    roles     = [Nominal, Representational, Representational]
    rhs       = DataTyCon { data_cons = [datacon], is_enum = False }

    tvs@[k,a,b] = binderVars binders
    sc_pred     = mkTyConApp eqReprPrimTyCon (mkTyVarTys [k, k, a, b])
    sc_sel_id   = mkDictSelId coercibleSCSelIdName klass

mk_class :: TyCon -> PredType -> Id -> Class
mk_class tycon sc_pred sc_sel_id
  = mkClass (tyConName tycon) (tyConTyVars tycon) [] [sc_pred] [sc_sel_id]
            [] [] (mkAnd []) tycon

{- *********************************************************************
*                                                                      *
                Kinds and RuntimeRep
*                                                                      *
********************************************************************* -}

-- For information about the usage of the following type,
-- see Note [TYPE and RuntimeRep] in module TysPrim
runtimeRepTy :: Type
runtimeRepTy = mkTyConTy runtimeRepTyCon

liftedTypeKindTyCon, starKindTyCon, unicodeStarKindTyCon :: TyCon

-- Type syononyms; see Note [TYPE and RuntimeRep] in TysPrim
-- type Type = tYPE 'PtrRepLifted
-- type *    = tYPE 'PtrRepLifted
-- type *    = tYPE 'PtrRepLifted  -- Unicode variant

liftedTypeKindTyCon   = mkSynonymTyCon liftedTypeKindTyConName
                                       [] liftedTypeKind []
                                       (tYPE ptrRepLiftedTy)

starKindTyCon         = mkSynonymTyCon starKindTyConName
                                       [] liftedTypeKind []
                                       (tYPE ptrRepLiftedTy)

unicodeStarKindTyCon  = mkSynonymTyCon unicodeStarKindTyConName
                                       [] liftedTypeKind []
                                       (tYPE ptrRepLiftedTy)

runtimeRepTyCon :: TyCon
runtimeRepTyCon = pcNonEnumTyCon runtimeRepTyConName Nothing []
                          (vecRepDataCon : runtimeRepSimpleDataCons)

vecRepDataCon :: DataCon
vecRepDataCon = pcSpecialDataCon vecRepDataConName [ mkTyConTy vecCountTyCon
                                                   , mkTyConTy vecElemTyCon ]
                                 runtimeRepTyCon
                                 (RuntimeRep prim_rep_fun)
  where
    prim_rep_fun [count, elem]
      | VecCount n <- tyConRuntimeRepInfo (tyConAppTyCon count)
      , VecElem  e <- tyConRuntimeRepInfo (tyConAppTyCon elem)
      = VecRep n e
    prim_rep_fun args
      = pprPanic "vecRepDataCon" (ppr args)

vecRepDataConTyCon :: TyCon
vecRepDataConTyCon = promoteDataCon vecRepDataCon

ptrRepUnliftedDataConTyCon :: TyCon
ptrRepUnliftedDataConTyCon = promoteDataCon ptrRepUnliftedDataCon

-- See Note [Wiring in RuntimeRep]
runtimeRepSimpleDataCons :: [DataCon]
ptrRepLiftedDataCon, ptrRepUnliftedDataCon :: DataCon
runtimeRepSimpleDataCons@(ptrRepLiftedDataCon : ptrRepUnliftedDataCon : _)
  = zipWithLazy mk_runtime_rep_dc
    [ PtrRep, PtrRep, VoidRep, IntRep, WordRep, Int64Rep
    , Word64Rep, AddrRep, FloatRep, DoubleRep
    , panic "unboxed tuple PrimRep", panic "unboxed sum PrimRep" ]
    runtimeRepSimpleDataConNames
  where
    mk_runtime_rep_dc primrep name
      = pcSpecialDataCon name [] runtimeRepTyCon (RuntimeRep (\_ -> primrep))

-- See Note [Wiring in RuntimeRep]
voidRepDataConTy, intRepDataConTy, wordRepDataConTy, int64RepDataConTy,
  word64RepDataConTy, addrRepDataConTy, floatRepDataConTy, doubleRepDataConTy,
  unboxedTupleRepDataConTy, unboxedSumRepDataConTy :: Type
[_, _, voidRepDataConTy, intRepDataConTy, wordRepDataConTy, int64RepDataConTy,
   word64RepDataConTy, addrRepDataConTy, floatRepDataConTy, doubleRepDataConTy,
   unboxedTupleRepDataConTy, unboxedSumRepDataConTy] = map (mkTyConTy . promoteDataCon)
                                   runtimeRepSimpleDataCons

vecCountTyCon :: TyCon
vecCountTyCon = pcNonEnumTyCon vecCountTyConName Nothing []
                        vecCountDataCons

-- See Note [Wiring in RuntimeRep]
vecCountDataCons :: [DataCon]
vecCountDataCons = zipWithLazy mk_vec_count_dc
                     [ 2, 4, 8, 16, 32, 64 ]
                     vecCountDataConNames
  where
    mk_vec_count_dc n name
      = pcSpecialDataCon name [] vecCountTyCon (VecCount n)

-- See Note [Wiring in RuntimeRep]
vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy,
  vec64DataConTy :: Type
[vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy,
  vec64DataConTy] = map (mkTyConTy . promoteDataCon) vecCountDataCons

vecElemTyCon :: TyCon
vecElemTyCon = pcNonEnumTyCon vecElemTyConName Nothing [] vecElemDataCons

-- See Note [Wiring in RuntimeRep]
vecElemDataCons :: [DataCon]
vecElemDataCons = zipWithLazy mk_vec_elem_dc
                    [ Int8ElemRep, Int16ElemRep, Int32ElemRep, Int64ElemRep
                    , Word8ElemRep, Word16ElemRep, Word32ElemRep, Word64ElemRep
                    , FloatElemRep, DoubleElemRep ]
                    vecElemDataConNames
  where
    mk_vec_elem_dc elem name
      = pcSpecialDataCon name [] vecElemTyCon (VecElem elem)

-- See Note [Wiring in RuntimeRep]
int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy,
  int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy,
  word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy,
  doubleElemRepDataConTy :: Type
[int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy,
  int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy,
  word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy,
  doubleElemRepDataConTy] = map (mkTyConTy . promoteDataCon)
                                vecElemDataCons

-- The type ('PtrRepLifted)
ptrRepLiftedTy :: Type
ptrRepLiftedTy = mkTyConTy $ promoteDataCon ptrRepLiftedDataCon

{- *********************************************************************
*                                                                      *
     The boxed primitive types: Char, Int, etc
*                                                                      *
********************************************************************* -}

charTy :: Type
charTy = mkTyConTy charTyCon

charTyCon :: TyCon
charTyCon   = pcNonEnumTyCon charTyConName
                       (Just (CType "" Nothing ("HsChar",fsLit "HsChar")))
                       [] [charDataCon]
charDataCon :: DataCon
charDataCon = pcDataCon charDataConName [] [charPrimTy] charTyCon

stringTy :: Type
stringTy = mkListTy charTy -- convenience only

intTy :: Type
intTy = mkTyConTy intTyCon

intTyCon :: TyCon
intTyCon = pcNonEnumTyCon intTyConName
                            (Just (CType "" Nothing ("HsInt",fsLit "HsInt"))) []
                            [intDataCon]
intDataCon :: DataCon
intDataCon = pcDataCon intDataConName [] [intPrimTy] intTyCon

wordTy :: Type
wordTy = mkTyConTy wordTyCon

wordTyCon :: TyCon
wordTyCon = pcNonEnumTyCon wordTyConName
                      (Just (CType "" Nothing ("HsWord", fsLit "HsWord"))) []
                      [wordDataCon]
wordDataCon :: DataCon
wordDataCon = pcDataCon wordDataConName [] [wordPrimTy] wordTyCon

word8Ty :: Type
word8Ty = mkTyConTy word8TyCon

word8TyCon :: TyCon
word8TyCon = pcNonEnumTyCon word8TyConName
                      (Just (CType "" Nothing ("HsWord8", fsLit "HsWord8"))) []
                      [word8DataCon]
word8DataCon :: DataCon
word8DataCon = pcDataCon word8DataConName [] [wordPrimTy] word8TyCon

floatTy :: Type
floatTy = mkTyConTy floatTyCon

floatTyCon :: TyCon
floatTyCon   = pcNonEnumTyCon floatTyConName
                      (Just (CType "" Nothing ("HsFloat", fsLit "HsFloat"))) []
                      [floatDataCon]
floatDataCon :: DataCon
floatDataCon = pcDataCon         floatDataConName [] [floatPrimTy] floatTyCon

doubleTy :: Type
doubleTy = mkTyConTy doubleTyCon

doubleTyCon :: TyCon
doubleTyCon = pcNonEnumTyCon doubleTyConName
                      (Just (CType "" Nothing ("HsDouble",fsLit "HsDouble"))) []
                      [doubleDataCon]

doubleDataCon :: DataCon
doubleDataCon = pcDataCon doubleDataConName [] [doublePrimTy] doubleTyCon

{-
************************************************************************
*                                                                      *
              The Bool type
*                                                                      *
************************************************************************

An ordinary enumeration type, but deeply wired in.  There are no
magical operations on @Bool@ (just the regular Prelude code).

{\em BEGIN IDLE SPECULATION BY SIMON}

This is not the only way to encode @Bool@.  A more obvious coding makes
@Bool@ just a boxed up version of @Bool#@, like this:
\begin{verbatim}
type Bool# = Int#
data Bool = MkBool Bool#
\end{verbatim}

Unfortunately, this doesn't correspond to what the Report says @Bool@
looks like!  Furthermore, we get slightly less efficient code (I
think) with this coding. @gtInt@ would look like this:

\begin{verbatim}
gtInt :: Int -> Int -> Bool
gtInt x y = case x of I# x# ->
            case y of I# y# ->
            case (gtIntPrim x# y#) of
                b# -> MkBool b#
\end{verbatim}

Notice that the result of the @gtIntPrim@ comparison has to be turned
into an integer (here called @b#@), and returned in a @MkBool@ box.

The @if@ expression would compile to this:
\begin{verbatim}
case (gtInt x y) of
  MkBool b# -> case b# of { 1# -> e1; 0# -> e2 }
\end{verbatim}

I think this code is a little less efficient than the previous code,
but I'm not certain.  At all events, corresponding with the Report is
important.  The interesting thing is that the language is expressive
enough to describe more than one alternative; and that a type doesn't
necessarily need to be a straightforwardly boxed version of its
primitive counterpart.

{\em END IDLE SPECULATION BY SIMON}
-}

boolTy :: Type
boolTy = mkTyConTy boolTyCon

boolTyCon :: TyCon
boolTyCon = pcTyCon True boolTyConName
                    (Just (CType "" Nothing ("HsBool", fsLit "HsBool")))
                    [] [falseDataCon, trueDataCon]

falseDataCon, trueDataCon :: DataCon
falseDataCon = pcDataCon falseDataConName [] [] boolTyCon
trueDataCon  = pcDataCon trueDataConName  [] [] boolTyCon

trueDataConId :: Id
trueDataConId  = dataConWrapId trueDataCon

orderingTyCon :: TyCon
orderingTyCon = pcTyCon True orderingTyConName Nothing
                        [] [ltDataCon, eqDataCon, gtDataCon]

ltDataCon, eqDataCon, gtDataCon :: DataCon
ltDataCon = pcDataCon ltDataConName  [] [] orderingTyCon
eqDataCon = pcDataCon eqDataConName  [] [] orderingTyCon
gtDataCon = pcDataCon gtDataConName  [] [] orderingTyCon

ltDataConId, eqDataConId, gtDataConId :: Id
ltDataConId = dataConWorkId ltDataCon
eqDataConId = dataConWorkId eqDataCon
gtDataConId = dataConWorkId gtDataCon

{-
************************************************************************
*                                                                      *
            The List type
   Special syntax, deeply wired in,
   but otherwise an ordinary algebraic data type
*                                                                      *
************************************************************************

       data [] a = [] | a : (List a)
-}

mkListTy :: Type -> Type
mkListTy ty = mkTyConApp listTyCon [ty]

listTyCon :: TyCon
listTyCon = buildAlgTyCon listTyConName alpha_tyvar [Representational]
                          Nothing []
                          (DataTyCon [nilDataCon, consDataCon] False )
                          False
                          (VanillaAlgTyCon $ mkPrelTyConRepName listTyConName)

nilDataCon :: DataCon
nilDataCon  = pcDataCon nilDataConName alpha_tyvar [] listTyCon

consDataCon :: DataCon
consDataCon = pcDataConWithFixity True {- Declared infix -}
               consDataConName
               alpha_tyvar [] [alphaTy, mkTyConApp listTyCon alpha_ty] listTyCon
-- Interesting: polymorphic recursion would help here.
-- We can't use (mkListTy alphaTy) in the defn of consDataCon, else mkListTy
-- gets the over-specific type (Type -> Type)

-- Wired-in type Maybe

maybeTyCon :: TyCon
maybeTyCon = pcTyCon False maybeTyConName Nothing alpha_tyvar
                     [nothingDataCon, justDataCon]

nothingDataCon :: DataCon
nothingDataCon = pcDataCon nothingDataConName alpha_tyvar [] maybeTyCon

justDataCon :: DataCon
justDataCon = pcDataCon justDataConName alpha_tyvar [alphaTy] maybeTyCon

{-
** *********************************************************************
*                                                                      *
            The tuple types
*                                                                      *
************************************************************************

The tuple types are definitely magic, because they form an infinite
family.

\begin{itemize}
\item
They have a special family of type constructors, of type @TyCon@
These contain the tycon arity, but don't require a Unique.

\item
They have a special family of constructors, of type
@Id@. Again these contain their arity but don't need a Unique.

\item
There should be a magic way of generating the info tables and
entry code for all tuples.

But at the moment we just compile a Haskell source
file\srcloc{lib/prelude/...} containing declarations like:
\begin{verbatim}
data Tuple0             = Tup0
data Tuple2  a b        = Tup2  a b
data Tuple3  a b c      = Tup3  a b c
data Tuple4  a b c d    = Tup4  a b c d
...
\end{verbatim}
The print-names associated with the magic @Id@s for tuple constructors
``just happen'' to be the same as those generated by these
declarations.

\item
The instance environment should have a magic way to know
that each tuple type is an instances of classes @Eq@, @Ix@, @Ord@ and
so on. \ToDo{Not implemented yet.}

\item
There should also be a way to generate the appropriate code for each
of these instances, but (like the info tables and entry code) it is
done by enumeration\srcloc{lib/prelude/InTup?.hs}.
\end{itemize}
-}

-- | Make a tuple type. The list of types should /not/ include any
-- RuntimeRep specifications.
mkTupleTy :: Boxity -> [Type] -> Type
-- Special case for *boxed* 1-tuples, which are represented by the type itself
mkTupleTy Boxed   [ty] = ty
mkTupleTy Boxed   tys  = mkTyConApp (tupleTyCon Boxed (length tys)) tys
mkTupleTy Unboxed tys  = mkTyConApp (tupleTyCon Unboxed (length tys))
                                        (map (getRuntimeRep "mkTupleTy") tys ++ tys)

-- | Build the type of a small tuple that holds the specified type of thing
mkBoxedTupleTy :: [Type] -> Type
mkBoxedTupleTy tys = mkTupleTy Boxed tys

unitTy :: Type
unitTy = mkTupleTy Boxed []

{- *********************************************************************
*                                                                      *
            The sum types
*                                                                      *
************************************************************************
-}

mkSumTy :: [Type] -> Type
mkSumTy tys = mkTyConApp (sumTyCon (length tys))
                         (map (getRuntimeRep "mkSumTy") tys ++ tys)

{- *********************************************************************
*                                                                      *
        The parallel-array type,  [::]
*                                                                      *
************************************************************************

Special syntax for parallel arrays needs some wired in definitions.
-}

-- | Construct a type representing the application of the parallel array constructor
mkPArrTy    :: Type -> Type
mkPArrTy ty  = mkTyConApp parrTyCon [ty]

-- | Represents the type constructor of parallel arrays
--
--  * This must match the definition in @PrelPArr@
--
-- NB: Although the constructor is given here, it will not be accessible in
--     user code as it is not in the environment of any compiled module except
--     @PrelPArr@.
--
parrTyCon :: TyCon
parrTyCon  = pcNonEnumTyCon parrTyConName Nothing alpha_tyvar [parrDataCon]

parrDataCon :: DataCon
parrDataCon  = pcDataCon
                 parrDataConName
                 alpha_tyvar            -- forall'ed type variables
                 [intTy,                -- 1st argument: Int
                  mkTyConApp            -- 2nd argument: Array# a
                    arrayPrimTyCon
                    alpha_ty]
                 parrTyCon

-- | Check whether a type constructor is the constructor for parallel arrays
isPArrTyCon    :: TyCon -> Bool
isPArrTyCon tc  = tyConName tc == parrTyConName

-- | Fake array constructors
--
-- * These constructors are never really used to represent array values;
--   however, they are very convenient during desugaring (and, in particular,
--   in the pattern matching compiler) to treat array pattern just like
--   yet another constructor pattern
--
parrFakeCon                        :: Arity -> DataCon
parrFakeCon i | i > mAX_TUPLE_SIZE  = mkPArrFakeCon  i  -- build one specially
parrFakeCon i                       = parrFakeConArr!i

-- pre-defined set of constructors
--
parrFakeConArr :: Array Int DataCon
parrFakeConArr  = array (0, mAX_TUPLE_SIZE) [(i, mkPArrFakeCon i)
                                            | i <- [0..mAX_TUPLE_SIZE]]

-- build a fake parallel array constructor for the given arity
--
mkPArrFakeCon       :: Int -> DataCon
mkPArrFakeCon arity  = data_con
  where
        data_con  = pcDataCon name [tyvar] tyvarTys parrTyCon
        tyvar     = head alphaTyVars
        tyvarTys  = replicate arity $ mkTyVarTy tyvar
        nameStr   = mkFastString ("MkPArr" ++ show arity)
        name      = mkWiredInName gHC_PARR' (mkDataOccFS nameStr) unique
                                  (AConLike (RealDataCon data_con)) UserSyntax
        unique      = mkPArrDataConUnique arity

-- | Checks whether a data constructor is a fake constructor for parallel arrays
isPArrFakeCon      :: DataCon -> Bool
isPArrFakeCon dcon  = dcon == parrFakeCon (dataConSourceArity dcon)

-- Promoted Booleans

promotedFalseDataCon, promotedTrueDataCon :: TyCon
promotedTrueDataCon   = promoteDataCon trueDataCon
promotedFalseDataCon  = promoteDataCon falseDataCon

-- Promoted Maybe
promotedNothingDataCon, promotedJustDataCon :: TyCon
promotedNothingDataCon = promoteDataCon nothingDataCon
promotedJustDataCon    = promoteDataCon justDataCon

-- Promoted Ordering

promotedLTDataCon
  , promotedEQDataCon
  , promotedGTDataCon
  :: TyCon
promotedLTDataCon     = promoteDataCon ltDataCon
promotedEQDataCon     = promoteDataCon eqDataCon
promotedGTDataCon     = promoteDataCon gtDataCon

-- Promoted List
promotedConsDataCon, promotedNilDataCon :: TyCon
promotedConsDataCon   = promoteDataCon consDataCon
promotedNilDataCon    = promoteDataCon nilDataCon
