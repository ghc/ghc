{-
(c) The GRASP Project, Glasgow University, 1994-1998

Wired-in knowledge about {\em non-primitive} types
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | This module is about types that can be defined in Haskell, but which
--   must be wired into the compiler nonetheless.  C.f module "GHC.Builtin.Types.Prim"
module GHC.Builtin.Types (
        -- * Helper functions defined here
        mkWiredInTyConName, -- This is used in GHC.Builtin.Types.Literals to define the
                            -- built-in functions for evaluation.

        mkWiredInIdName,    -- used in GHC.Types.Id.Make

        -- * All wired in things
        wiredInTyCons, isBuiltInOcc_maybe, isTupleTyOcc_maybe, isSumTyOcc_maybe,
        isPunOcc_maybe,

        -- * Bool
        boolTy, boolTyCon, boolTyCon_RDR, boolTyConName,
        trueDataCon,  trueDataConId,  true_RDR,
        falseDataCon, falseDataConId, false_RDR,
        promotedFalseDataCon, promotedTrueDataCon,

        -- * Ordering
        orderingTyCon,
        ordLTDataCon, ordLTDataConId,
        ordEQDataCon, ordEQDataConId,
        ordGTDataCon, ordGTDataConId,
        promotedLTDataCon, promotedEQDataCon, promotedGTDataCon,

        -- * Boxing primitive types
        boxingDataCon, BoxingInfo(..),

        -- * Char
        charTyCon, charDataCon, charTyCon_RDR,
        charTy, stringTy, charTyConName, stringTyCon_RDR,

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
        word8TyCon, word8DataCon, word8Ty,

        -- * List
        listTyCon, listTyCon_RDR, listTyConName, listTyConKey,
        nilDataCon, nilDataConName, nilDataConKey,
        consDataCon_RDR, consDataCon, consDataConName,
        promotedNilDataCon, promotedConsDataCon,
        mkListTy, mkPromotedListTy, extractPromotedList,

        -- * Maybe
        maybeTyCon, maybeTyConName,
        nothingDataCon, nothingDataConName, promotedNothingDataCon,
        justDataCon, justDataConName, promotedJustDataCon,
        mkPromotedMaybeTy, mkMaybeTy, isPromotedMaybeTy,

        -- * Tuples
        mkTupleTy, mkTupleTy1, mkBoxedTupleTy, mkTupleStr,
        tupleTyCon, tupleDataCon, tupleTyConName, tupleDataConName,
        promotedTupleDataCon,
        unitTyCon, unitDataCon, unitDataConId, unitTy, unitTyConKey,
        soloTyCon,
        pairTyCon, mkPromotedPairTy, isPromotedPairType,
        unboxedUnitTy,
        unboxedUnitTyCon, unboxedUnitDataCon,
        unboxedTupleKind, unboxedSumKind,
        filterCTuple, mkConstraintTupleTy,

        -- ** Constraint tuples
        cTupleTyCon, cTupleTyConName, cTupleTyConNames, isCTupleTyConName,
        cTupleTyConNameArity_maybe,
        cTupleDataCon, cTupleDataConName, cTupleDataConNames,
        cTupleSelId, cTupleSelIdName,

        -- * Any
        anyTyCon, anyTy, anyTypeOfKind,

        -- * Recovery TyCon
        makeRecoveryTyCon,

        -- * Sums
        mkSumTy, sumTyCon, sumDataCon,

        -- * Kinds
        typeSymbolKindCon, typeSymbolKind,
        isLiftedTypeKindTyConName,
        typeToTypeKind,
        liftedRepTyCon, unliftedRepTyCon,
        tYPETyCon, tYPETyConName, tYPEKind,
        cONSTRAINTTyCon, cONSTRAINTTyConName, cONSTRAINTKind,
        constraintKind, liftedTypeKind, unliftedTypeKind, zeroBitTypeKind,
        constraintKindTyCon, liftedTypeKindTyCon, unliftedTypeKindTyCon,
        constraintKindTyConName, liftedTypeKindTyConName, unliftedTypeKindTyConName,
        liftedRepTyConName, unliftedRepTyConName,

        -- * Equality predicates
        heqTyCon, heqTyConName, heqClass, heqDataCon,
        eqTyCon, eqTyConName, eqClass, eqDataCon, eqTyCon_RDR,
        coercibleTyCon, coercibleTyConName, coercibleDataCon, coercibleClass,

        -- * RuntimeRep and friends
        runtimeRepTyCon, vecCountTyCon, vecElemTyCon,

        boxedRepDataConTyCon,
        runtimeRepTy, liftedRepTy, unliftedRepTy, zeroBitRepTy,

        vecRepDataConTyCon, tupleRepDataConTyCon, sumRepDataConTyCon,

        -- * Levity
        levityTyCon, levityTy,
        liftedDataConTyCon, unliftedDataConTyCon,
        liftedDataConTy,    unliftedDataConTy,

        intRepDataConTy,
        int8RepDataConTy, int16RepDataConTy, int32RepDataConTy, int64RepDataConTy,
        wordRepDataConTy,
        word8RepDataConTy, word16RepDataConTy, word32RepDataConTy, word64RepDataConTy,
        addrRepDataConTy,
        floatRepDataConTy, doubleRepDataConTy,

        vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy,
        vec64DataConTy,

        int8ElemRepDataConTy, int16ElemRepDataConTy, int32ElemRepDataConTy,
        int64ElemRepDataConTy, word8ElemRepDataConTy, word16ElemRepDataConTy,
        word32ElemRepDataConTy, word64ElemRepDataConTy, floatElemRepDataConTy,

        doubleElemRepDataConTy,

        -- * Multiplicity and friends
        multiplicityTyConName, oneDataConName, manyDataConName, multiplicityTy,
        multiplicityTyCon, oneDataCon, manyDataCon, oneDataConTy, manyDataConTy,
        oneDataConTyCon, manyDataConTyCon,
        multMulTyCon,

        unrestrictedFunTyCon, unrestrictedFunTyConName,

        -- * Bignum
        integerTy, integerTyCon, integerTyConName,
        integerISDataCon, integerISDataConName,
        integerIPDataCon, integerIPDataConName,
        integerINDataCon, integerINDataConName,
        naturalTy, naturalTyCon, naturalTyConName,
        naturalNSDataCon, naturalNSDataConName,
        naturalNBDataCon, naturalNBDataConName,

         pretendNameIsInScope,
    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Types.Id.Make ( mkDataConWorkId, mkDictSelId )

-- friends:
import GHC.Builtin.Names
import GHC.Builtin.Types.Prim
import GHC.Builtin.Uniques

-- others:
import GHC.Core( Expr(Type), mkConApp )
import GHC.Core.Coercion.Axiom
import GHC.Core.Type
import GHC.Types.Id
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Core.TyCon
import GHC.Core.Class     ( Class, mkClass )
import GHC.Core.Map.Type  ( TypeMap, emptyTypeMap, extendTypeMap, lookupTypeMap )
import qualified GHC.Core.TyCo.Rep as TyCoRep (Type(TyConApp))

import GHC.Types.TyThing
import GHC.Types.SourceText
import GHC.Types.Var ( VarBndr (Bndr), tyVarName )
import GHC.Types.RepType
import GHC.Types.Name.Reader
import GHC.Types.Name as Name
import GHC.Types.Name.Env ( lookupNameEnv_NF, mkNameEnv )
import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Types.Unique.Set

import {-# SOURCE #-} GHC.Tc.Types.Origin
  ( FixedRuntimeRepOrigin(..), mkFRRUnboxedTuple, mkFRRUnboxedSum )
import {-# SOURCE #-} GHC.Tc.Utils.TcType
  ( ConcreteTvOrigin(..), ConcreteTyVars, noConcreteTyVars )

import GHC.Settings.Constants ( mAX_TUPLE_SIZE, mAX_CTUPLE_SIZE, mAX_SUM_SIZE )
import GHC.Unit.Module        ( Module )

import Data.Array
import GHC.Data.FastString
import GHC.Data.BooleanFormula ( mkAnd )

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import qualified Data.ByteString.Char8 as BS

import Data.Foldable
import Data.List        ( elemIndex, intersperse )
import Numeric          ( showInt )

import Data.Char (ord, isDigit)
import Control.Applicative ((<|>))

alpha_tyvar :: [TyVar]
alpha_tyvar = [alphaTyVar]

alpha_ty :: [Type]
alpha_ty = [alphaTy]

{-
Note [Wired-in Types and Type Constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This module include a lot of wired-in types and type constructors. Here,
these are presented in a tabular format to make it easier to find the
wired-in type identifier corresponding to a known Haskell type. Data
constructors are nested under their corresponding types with two spaces
of indentation.

Identifier              Type    Haskell name          Notes
----------------------------------------------------------------------------
liftedTypeKindTyCon     TyCon   GHC.Types.Type        Synonym for: TYPE LiftedRep
unliftedTypeKindTyCon   TyCon   GHC.Types.Type        Synonym for: TYPE UnliftedRep
liftedRepTyCon          TyCon   GHC.Types.LiftedRep   Synonym for: 'BoxedRep 'Lifted
unliftedRepTyCon        TyCon   GHC.Types.LiftedRep   Synonym for: 'BoxedRep 'Unlifted
levityTyCon             TyCon   GHC.Types.Levity      Data type
  liftedDataConTyCon    TyCon   GHC.Types.Lifted      Data constructor
  unliftedDataConTyCon  TyCon   GHC.Types.Unlifted    Data constructor
vecCountTyCon           TyCon   GHC.Types.VecCount    Data type
  vec2DataConTy         Type    GHC.Types.Vec2        Data constructor
  vec4DataConTy         Type    GHC.Types.Vec4        Data constructor
  vec8DataConTy         Type    GHC.Types.Vec8        Data constructor
  vec16DataConTy        Type    GHC.Types.Vec16       Data constructor
  vec32DataConTy        Type    GHC.Types.Vec32       Data constructor
  vec64DataConTy        Type    GHC.Types.Vec64       Data constructor
runtimeRepTyCon         TyCon   GHC.Types.RuntimeRep  Data type
  boxedRepDataConTyCon  TyCon   GHC.Types.BoxedRep    Data constructor
  intRepDataConTy       Type    GHC.Types.IntRep      Data constructor
  doubleRepDataConTy    Type    GHC.Types.DoubleRep   Data constructor
  floatRepDataConTy     Type    GHC.Types.FloatRep    Data constructor
boolTyCon               TyCon   GHC.Types.Bool        Data type
  trueDataCon           DataCon GHC.Types.True        Data constructor
  falseDataCon          DataCon GHC.Types.False       Data constructor
  promotedTrueDataCon   TyCon   GHC.Types.True        Data constructor
  promotedFalseDataCon  TyCon   GHC.Types.False       Data constructor

************************************************************************
*                                                                      *
\subsection{Wired in type constructors}
*                                                                      *
************************************************************************

If you change which things are wired in, make sure you change their
names in GHC.Builtin.Names, so they use wTcQual, wDataQual, etc

-}


-- This list is used only to define GHC.Builtin.Utils.wiredInThings. That in turn
-- is used to initialise the name environment carried around by the renamer.
-- This means that if we look up the name of a TyCon (or its implicit binders)
-- that occurs in this list that name will be assigned the wired-in key we
-- define here.
--
-- Because of their infinite nature, this list excludes
--   * tuples, including boxed, unboxed and constraint tuples
---       (mkTupleTyCon, unitTyCon, pairTyCon)
--   * unboxed sums (sumTyCon)
-- See Note [Infinite families of known-key names] in GHC.Builtin.Names
--
-- See also Note [Known-key names]
wiredInTyCons :: [TyCon]

wiredInTyCons = map (dataConTyCon . snd) boxingDataCons
             ++ [ -- Units are not treated like other tuples, because they
                  -- are defined in GHC.Base, and there's only a few of them. We
                  -- put them in wiredInTyCons so that they will pre-populate
                  -- the name cache, so the parser in isBuiltInOcc_maybe doesn't
                  -- need to look out for them.
                  unitTyCon
                , unboxedUnitTyCon

                -- Solo (i.e., the boxed 1-tuple) is also not treated
                -- like other tuples (i.e. we /do/ include it here),
                -- since it does not use special syntax like other tuples
                -- See Note [One-tuples] (Wrinkle: Make boxed one-tuple names
                -- have known keys) in GHC.Builtin.Types.
                , soloTyCon

                , anyTyCon
                , boolTyCon
                , charTyCon
                , stringTyCon
                , doubleTyCon
                , floatTyCon
                , intTyCon
                , wordTyCon
                , listTyCon
                , orderingTyCon
                , maybeTyCon
                , heqTyCon
                , eqTyCon
                , coercibleTyCon
                , typeSymbolKindCon
                , runtimeRepTyCon
                , levityTyCon
                , vecCountTyCon
                , vecElemTyCon
                , constraintKindTyCon
                , liftedTypeKindTyCon
                , unliftedTypeKindTyCon
                , multiplicityTyCon
                , naturalTyCon
                , integerTyCon
                , liftedRepTyCon
                , unliftedRepTyCon
                , zeroBitRepTyCon
                , zeroBitTypeTyCon
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
eqTyConName, eqDataConName, eqSCSelIdName :: Name
eqTyConName   = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "~")   eqTyConKey   eqTyCon
eqDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "Eq#") eqDataConKey eqDataCon
eqSCSelIdName = mkWiredInIdName gHC_TYPES (fsLit "eq_sel") eqSCSelIdKey eqSCSelId

eqTyCon_RDR :: RdrName
eqTyCon_RDR = nameRdrName eqTyConName

-- See Note [Kind-changing of (~) and Coercible]
-- in libraries/ghc-prim/GHC/Types.hs
heqTyConName, heqDataConName, heqSCSelIdName :: Name
heqTyConName   = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "~~")   heqTyConKey      heqTyCon
heqDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "HEq#") heqDataConKey heqDataCon
heqSCSelIdName = mkWiredInIdName gHC_TYPES (fsLit "heq_sel") heqSCSelIdKey heqSCSelId

-- See Note [Kind-changing of (~) and Coercible] in libraries/ghc-prim/GHC/Types.hs
coercibleTyConName, coercibleDataConName, coercibleSCSelIdName :: Name
coercibleTyConName   = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Coercible")  coercibleTyConKey   coercibleTyCon
coercibleDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "MkCoercible") coercibleDataConKey coercibleDataCon
coercibleSCSelIdName = mkWiredInIdName gHC_TYPES (fsLit "coercible_sel") coercibleSCSelIdKey coercibleSCSelId

charTyConName, charDataConName, intTyConName, intDataConName, stringTyConName :: Name
charTyConName     = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Char")   charTyConKey charTyCon
charDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "C#")     charDataConKey charDataCon
stringTyConName   = mkWiredInTyConName   UserSyntax gHC_INTERNAL_BASE  (fsLit "String") stringTyConKey stringTyCon
intTyConName      = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Int")    intTyConKey   intTyCon
intDataConName    = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "I#")     intDataConKey  intDataCon

boolTyConName, falseDataConName, trueDataConName :: Name
boolTyConName     = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Bool") boolTyConKey boolTyCon
falseDataConName  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "False") falseDataConKey falseDataCon
trueDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "True")  trueDataConKey  trueDataCon

listTyConName, nilDataConName, consDataConName :: Name
listTyConName     = mkWiredInTyConName   UserSyntax    gHC_TYPES (fsLit "List") listTyConKey listTyCon
nilDataConName    = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit "[]") nilDataConKey nilDataCon
consDataConName   = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit ":") consDataConKey consDataCon

maybeTyConName, nothingDataConName, justDataConName :: Name
maybeTyConName     = mkWiredInTyConName   UserSyntax gHC_INTERNAL_MAYBE (fsLit "Maybe")
                                          maybeTyConKey maybeTyCon
nothingDataConName = mkWiredInDataConName UserSyntax gHC_INTERNAL_MAYBE (fsLit "Nothing")
                                          nothingDataConKey nothingDataCon
justDataConName    = mkWiredInDataConName UserSyntax gHC_INTERNAL_MAYBE (fsLit "Just")
                                          justDataConKey justDataCon

wordTyConName, wordDataConName, word8DataConName :: Name
wordTyConName      = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Word")   wordTyConKey     wordTyCon
wordDataConName    = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "W#")     wordDataConKey   wordDataCon
word8DataConName   = mkWiredInDataConName UserSyntax gHC_INTERNAL_WORD  (fsLit "W8#")    word8DataConKey  word8DataCon

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

It's used to instantiate un-constrained type variables after type checking. For
example, 'length' has type

  length :: forall a. [a] -> Int

and the list datacon for the empty list has type

  [] :: forall a. [a]

In order to compose these two terms as @length []@ a type
application is required, but there is no constraint on the
choice.  In this situation GHC uses 'Any',

> length @(Any @Type) ([] @(Any @Type))

Above, we print kinds explicitly, as if with -fprint-explicit-kinds.

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

-- | Make a fake, recovery 'TyCon' from an existing one.
-- Used when recovering from errors in type declarations
makeRecoveryTyCon :: TyCon -> TyCon
makeRecoveryTyCon tc
  = mkTcTyCon (tyConName tc)
              bndrs res_kind
              noTcTyConScopedTyVars
              True             -- Fully generalised
              flavour          -- Keep old flavour
  where
    flavour = tyConFlavour tc
    [kv] = mkTemplateKindVars [liftedTypeKind]
    (bndrs, res_kind)
       = case flavour of
           PromotedDataConFlavour -> ([mkNamedTyConBinder Inferred kv], mkTyVarTy kv)
           _ -> (tyConBinders tc, tyConResKind tc)
        -- For data types we have already validated their kind, so it
        -- makes sense to keep it. For promoted data constructors we haven't,
        -- so we recover with kind (forall k. k).  Otherwise consider
        --     data T a where { MkT :: Show a => T a }
        -- If T is for some reason invalid, we don't want to fall over
        -- at (promoted) use-sites of MkT.

-- Kinds
typeSymbolKindConName :: Name
typeSymbolKindConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Symbol") typeSymbolKindConNameKey typeSymbolKindCon


boolTyCon_RDR, false_RDR, true_RDR, intTyCon_RDR, charTyCon_RDR, stringTyCon_RDR,
    intDataCon_RDR, listTyCon_RDR, consDataCon_RDR :: RdrName
boolTyCon_RDR   = nameRdrName boolTyConName
false_RDR       = nameRdrName falseDataConName
true_RDR        = nameRdrName trueDataConName
intTyCon_RDR    = nameRdrName intTyConName
charTyCon_RDR   = nameRdrName charTyConName
stringTyCon_RDR = nameRdrName stringTyConName
intDataCon_RDR  = nameRdrName intDataConName
listTyCon_RDR   = nameRdrName listTyConName
consDataCon_RDR = nameRdrName consDataConName

{-
************************************************************************
*                                                                      *
\subsection{mkWiredInTyCon}
*                                                                      *
************************************************************************
-}

-- This function assumes that the types it creates have all parameters at
-- Representational role, and that there is no kind polymorphism.
pcTyCon :: Name -> Maybe CType -> [TyVar] -> [DataCon] -> TyCon
pcTyCon name cType tyvars cons
  = mkAlgTyCon name
                (mkAnonTyConBinders tyvars)
                liftedTypeKind
                (map (const Representational) tyvars)
                cType
                []              -- No stupid theta
                (mkDataTyConRhs cons)
                (VanillaAlgTyCon (mkPrelTyConRepName name))
                False           -- Not in GADT syntax

pcDataCon :: Name -> [TyVar] -> [Type] -> TyCon -> DataCon
pcDataCon n univs tys
  = pcRepPolyDataCon n univs noConcreteTyVars tys

pcRepPolyDataCon :: Name -> [TyVar] -> ConcreteTyVars
                 -> [Type] -> TyCon -> DataCon
pcRepPolyDataCon n univs conc_tvs tys
  = pcDataConWithFixity False n
      univs
      []    -- no ex_tvs
      conc_tvs
      univs -- the univs are precisely the user-written tyvars
      []    -- No theta
      (map linear tys)

pcDataConConstraint :: Name -> [TyVar] -> ThetaType -> TyCon -> DataCon
-- Used for data constructors whose arguments are all constraints.
-- Notably constraint tuples, Eq# etc.
pcDataConConstraint n univs theta
  = pcDataConWithFixity False n
      univs
      []           -- No ex_tvs
      noConcreteTyVars
      univs        -- The univs are precisely the user-written tyvars
      theta        -- All constraint arguments
      []           -- No value arguments

-- Used for RuntimeRep and friends; things with PromDataConInfo
pcSpecialDataCon :: Name -> [Type] -> TyCon -> PromDataConInfo -> DataCon
pcSpecialDataCon dc_name arg_tys tycon rri
  = pcDataConWithFixity' False dc_name
                         (dataConWorkerUnique (nameUnique dc_name)) rri
                         [] [] noConcreteTyVars [] [] (map linear arg_tys) tycon

pcDataConWithFixity :: Bool      -- ^ declared infix?
                    -> Name      -- ^ datacon name
                    -> [TyVar]   -- ^ univ tyvars
                    -> [TyCoVar] -- ^ ex tycovars
                    -> ConcreteTyVars
                                 -- ^ concrete tyvars
                    -> [TyCoVar] -- ^ user-written tycovars
                    -> ThetaType
                    -> [Scaled Type]    -- ^ args
                    -> TyCon
                    -> DataCon
pcDataConWithFixity infx n = pcDataConWithFixity' infx n
                                 (dataConWorkerUnique (nameUnique n)) NoPromInfo
-- The Name's unique is the first of two free uniques;
-- the first is used for the datacon itself,
-- the second is used for the "worker name"
--
-- To support this the mkPreludeDataConUnique function "allocates"
-- one DataCon unique per pair of Ints.

pcDataConWithFixity' :: Bool -> Name -> Unique -> PromDataConInfo
                     -> [TyVar] -> [TyCoVar]
                     -> ConcreteTyVars
                     -> [TyCoVar]
                     -> ThetaType -> [Scaled Type] -> TyCon -> DataCon
-- The Name should be in the DataName name space; it's the name
-- of the DataCon itself.
--
-- IMPORTANT NOTE:
--    if you try to wire-in a /GADT/ data constructor you will
--    find it hard (we did).  You will need wrapper and worker
--    Names, a DataConBoxer, DataConRep, EqSpec, etc.
--    Try hard not to wire-in GADT data types. You will live
--    to regret doing so (we do).

pcDataConWithFixity' declared_infix dc_name wrk_key rri
                     tyvars ex_tyvars conc_tyvars user_tyvars theta arg_tys tycon
  = data_con
  where
    tag_map = mkTyConTagMap tycon
    -- This constructs the constructor Name to ConTag map once per
    -- constructor, which is quadratic. It's OK here, because it's
    -- only called for wired in data types that don't have a lot of
    -- constructors. It's also likely that GHC will lift tag_map, since
    -- we call pcDataConWithFixity' with static TyCons in the same module.
    -- See Note [Constructor tag allocation] and #14657
    data_con = mkDataCon dc_name declared_infix prom_info
                (map (const no_bang) arg_tys)
                []      -- No labelled fields
                tyvars ex_tyvars
                conc_tyvars
                (mkTyVarBinders SpecifiedSpec user_tyvars)
                []      -- No equality spec
                theta
                arg_tys (mkTyConApp tycon (mkTyVarTys tyvars))
                rri
                tycon
                (lookupNameEnv_NF tag_map dc_name)
                []      -- No stupid theta
                (mkDataConWorkId wrk_name data_con)
                NoDataConRep    -- Wired-in types are too simple to need wrappers

    no_bang = mkHsSrcBang NoSourceText NoSrcUnpack NoSrcStrict

    wrk_name = mkDataConWorkerName data_con wrk_key

    prom_info = mkPrelTyConRepName dc_name

mkDataConWorkerName :: DataCon -> Unique -> Name
mkDataConWorkerName data_con wrk_key =
    mkWiredInName modu wrk_occ wrk_key
                  (AnId (dataConWorkId data_con)) UserSyntax
  where
    modu     = assert (isExternalName dc_name) $
               nameModule dc_name
    dc_name = dataConName data_con
    dc_occ  = nameOccName dc_name
    wrk_occ = mkDataConWorkerOcc dc_occ


{-
************************************************************************
*                                                                      *
              Symbol
*                                                                      *
************************************************************************
-}

typeSymbolKindCon :: TyCon
-- data Symbol
typeSymbolKindCon = pcTyCon typeSymbolKindConName Nothing [] []

typeSymbolKind :: Kind
typeSymbolKind = mkTyConTy typeSymbolKindCon


{-
************************************************************************
*                                                                      *
                Stuff for dealing with tuples
*                                                                      *
************************************************************************

Note [How tuples work]
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
    - Data type declarations in GHC.Types
      but no actual declaration and no info table

* ConstraintTuples
    - A wired-in type.
    - Declared as classes in GHC.Classes, e.g.
         class (c1,c2) => CTuple2 c1 c2
    - Given constraints: the superclasses automatically become available
    - Wanted constraints: there is a built-in instance
         instance (c1,c2) => CTuple2 c1 c2
      See GHC.Tc.Instance.Class.matchCTuple
    - Currently just go up to 64; beyond that
      you have to use manual nesting
    - Unlike BoxedTuples and UnboxedTuples, which only wire
      in type constructors and data constructors, ConstraintTuples also wire in
      superclass selector functions. For instance, $p1CTuple2 and $p2CTuple2 are
      the selectors for the binary constraint tuple.
    - The parenthesis syntax for grouping constraints in contexts is not treated
      as a constraint tuple. The parser starts with a tuple type, then a
      postprocessing action extracts the individual constraints as a list and
      stores them in the context field of types like HsQualTy.

* In quite a lot of places things are restricted just to
  BoxedTuple/UnboxedTuple, and then we used BasicTypes.Boxity to distinguish
  E.g. tupleTyCon has a Boxity argument

* When looking up an OccName in the original-name cache
  (GHC.Iface.Env.lookupOrigNameCache), we spot the tuple OccName to make sure
  we get the right wired-in name.  This guy can't tell the difference
  between BoxedTuple and ConstraintTuple (same OccName!), so tuples
  are not serialised into interface files using OccNames at all.

* Serialization to interface files works via the usual mechanism for known-key
  things: instead of serializing the OccName we just serialize the key. During
  deserialization we lookup the Name associated with the unique with the logic
  in GHC.Builtin.Uniques. See Note [Symbol table representation of names] for details.

See also Note [Known-key names] in GHC.Builtin.Names.

Note [One-tuples]
~~~~~~~~~~~~~~~~~
GHC supports both boxed and unboxed one-tuples:
 - Unboxed one-tuples are sometimes useful when returning a
   single value after CPR analysis
 - A boxed one-tuple is used by GHC.HsToCore.Utils.mkSelectorBinds, when
   there is just one binder
Basically it keeps everything uniform.

However the /naming/ of the type/data constructors for one-tuples is a
bit odd:
  3-tuples:  Tuple3   (,,)#
  2-tuples:  Tuple2   (,)#
  1-tuples:  ??
  0-tuples:  Unit     ()#

Zero-tuples have used up the logical name. So we use 'Solo' and 'Solo#'
for one-tuples.  So in ghc-prim:GHC.Tuple we see the declarations:
  data Unit = ()
  data Solo a = MkSolo a
  data Tuple2 a b = (a,b)

There is no way to write a boxed one-tuple in Haskell using tuple syntax.
They can, however, be written using other methods:

1. They can be written directly by importing them from GHC.Tuple.
2. They can be generated by way of Template Haskell or in `deriving` code.

There is nothing special about one-tuples in Core; in particular, they have no
custom pretty-printing, just using `Solo`.

Note that there is *not* a unary constraint tuple, unlike for other forms of
tuples. See [Ignore unary constraint tuples] in GHC.Tc.Gen.HsType for more
details.

See also Note [Flattening one-tuples] in GHC.Core.Make and
Note [Don't flatten tuples from HsSyn] in GHC.Core.Make.

-----
-- Wrinkle: Make boxed one-tuple names have known keys
-----

We make boxed one-tuple names have known keys so that `data Solo a = MkSolo a`,
defined in GHC.Tuple, will be used when one-tuples are spliced in through
Template Haskell. This program (from #18097) crucially relies on this:

  case $( tupE [ [| "ok" |] ] ) of MkSolo x -> putStrLn x

Unless Solo has a known key, the type of `$( tupE [ [| "ok" |] ] )` (an
ExplicitTuple of length 1) will not match the type of Solo (an ordinary
data constructor used in a pattern). Making Solo known-key allows GHC to make
this connection.

Unlike Solo, every other tuple is /not/ known-key
(see Note [Infinite families of known-key names] in GHC.Builtin.Names). The
main reason for this exception is that other tuples are written with special
syntax, and as a result, they are renamed using a special `isBuiltInOcc_maybe`
function (see Note [Built-in syntax and the OrigNameCache] in GHC.Types.Name.Cache).
In contrast, Solo is just an ordinary data type with no special syntax, so it
doesn't really make sense to handle it in `isBuiltInOcc_maybe`. Making Solo
known-key is the next-best way to teach the internals of the compiler about it.
-}

-- | Built-in syntax isn't "in scope" so these OccNames map to wired-in Names
-- with BuiltInSyntax. However, this should only be necessary while resolving
-- names produced by Template Haskell splices since we take care to encode
-- built-in syntax names specially in interface files. See
-- Note [Symbol table representation of names] in GHC.Iface.Binary.
--
-- Moreover, there is no need to include names of things that the user can't
-- write (e.g. type representation bindings like $tc(,,,)).
isBuiltInOcc_maybe :: OccName -> Maybe Name
isBuiltInOcc_maybe occ =
    case name of
      "[]" -> Just $ choose_ns listTyConName nilDataConName
      ":"    -> Just consDataConName

      -- function tycon
      "FUN"  -> Just fUNTyConName
      "->"  -> Just unrestrictedFunTyConName

      -- tuple data/tycon
      -- We deliberately exclude Solo (the boxed 1-tuple).
      -- See Note [One-tuples] (Wrinkle: Make boxed one-tuple names have known keys)
      "()"    -> Just $ tup_name Boxed 0
      _ | Just rest <- "(" `BS.stripPrefix` name
        , (commas, rest') <- BS.span (==',') rest
        , ")" <- rest'
             -> Just $ tup_name Boxed (1+BS.length commas)

      -- unboxed tuple data/tycon
      "(##)"  -> Just $ tup_name Unboxed 0
      "(# #)" -> Just $ tup_name Unboxed 1
      _ | Just rest <- "(#" `BS.stripPrefix` name
        , (commas, rest') <- BS.span (==',') rest
        , "#)" <- rest'
             -> Just $ tup_name Unboxed (1+BS.length commas)

      -- unboxed sum tycon
      _ | Just rest <- "(#" `BS.stripPrefix` name
        , (nb_pipes, rest') <- span_pipes rest
        , "#)" <- rest'
             -> Just $ tyConName $ sumTyCon (1+nb_pipes)

      -- unboxed sum datacon
      _ | Just rest <- "(#" `BS.stripPrefix` name
        , (nb_pipes1, rest') <- span_pipes rest
        , Just rest'' <- "_" `BS.stripPrefix` rest'
        , (nb_pipes2, rest''') <- span_pipes rest''
        , "#)" <- rest'''
             -> let arity = nb_pipes1 + nb_pipes2 + 1
                    alt = nb_pipes1 + 1
                in Just $ dataConName $ sumDataCon alt arity

      _ -> Nothing
  where
    name = bytesFS $ occNameFS occ

    span_pipes :: BS.ByteString -> (Int, BS.ByteString)
    span_pipes = go 0
      where
        go nb_pipes bs = case BS.uncons bs of
          Just ('|',rest) -> go (nb_pipes + 1) rest
          Just (' ',rest) -> go nb_pipes       rest
          _               -> (nb_pipes, bs)

    choose_ns :: Name -> Name -> Name
    choose_ns tc dc
      | isTcClsNameSpace ns   = tc
      | isDataConNameSpace ns = dc
      | otherwise             = pprPanic "tup_name" (ppr occ <+> parens (pprNameSpace ns))
      where ns = occNameSpace occ

    tup_name boxity arity
      = choose_ns (getName (tupleTyCon   boxity arity))
                  (getName (tupleDataCon boxity arity))

isTupleTyOcc_maybe :: Module -> OccName -> Maybe Name
isTupleTyOcc_maybe mod occ
  | mod == gHC_INTERNAL_TUPLE || mod == gHC_TYPES
  = match_occ
  where
    match_occ
      | occ == occName unitTyConName = Just unitTyConName
      | occ == occName soloTyConName = Just soloTyConName
      | occ == occName unboxedUnitTyConName = Just unboxedUnitTyConName
      | occ == occName unboxedSoloTyConName = Just unboxedSoloTyConName
      | otherwise = isTupleNTyOcc_maybe occ
isTupleTyOcc_maybe _ _ = Nothing

isCTupleOcc_maybe :: Module -> OccName -> Maybe Name
isCTupleOcc_maybe mod occ
  | mod == gHC_CLASSES
  = match_occ
  where
    match_occ
      | occ == occName (cTupleTyConName 0) = Just (cTupleTyConName 0)
      | occ == occName (cTupleTyConName 1) = Just (cTupleTyConName 1)
      | 'C':'T':'u':'p':'l':'e' : rest <- occNameString occ
      , Just (BoxedTuple, num) <- arity_and_boxity rest
      , num >= 2 && num <= 64
           = Just $ cTupleTyConName num
      | otherwise = Nothing

isCTupleOcc_maybe _ _ = Nothing

-- | This is only for Tuple<n>, not for Unit or Solo
isTupleNTyOcc_maybe :: OccName -> Maybe Name
isTupleNTyOcc_maybe occ =
  case occNameString occ of
    'T':'u':'p':'l':'e':str | Just (sort, n) <- arity_and_boxity str, n > 1
      -> Just (tupleTyConName sort n)
    _ -> Nothing

isSumTyOcc_maybe :: Module -> OccName -> Maybe Name
isSumTyOcc_maybe mod occ | mod == gHC_TYPES =
  isSumNTyOcc_maybe occ
isSumTyOcc_maybe _ _ = Nothing

isSumNTyOcc_maybe :: OccName -> Maybe Name
isSumNTyOcc_maybe occ =
  case occNameString occ of
    'S':'u':'m':str | Just (UnboxedTuple, n) <- arity_and_boxity str, n > 1
      -> Just (tyConName (sumTyCon n))
    _ -> Nothing

-- | See Note [Small Ints parsing]
--
-- Analyze a string as the suffix of an OccName of a tuple or sum tycon to
-- determine its arity and boxity (based on the presence of a @#@).
arity_and_boxity :: String -> Maybe (TupleSort, Int)
arity_and_boxity s = case s of
  c1 : t1 | isDigit c1 -> case t1 of
    [] -> Just (BoxedTuple, digit_to_int c1)
    ['#'] -> Just (UnboxedTuple, digit_to_int c1)
    c2 : t2 | isDigit c2 ->
      let ar = digit_to_int c1 * 10 + digit_to_int c2
      in case t2 of
        [] -> Just (BoxedTuple, ar)
        ['#'] -> Just (UnboxedTuple, ar)
        _ -> Nothing
    _ -> Nothing
  _ -> Nothing
  where
    digit_to_int :: Char -> Int
    digit_to_int c = ord c - ord '0'

{-
Note [Small Ints parsing]
~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, tuples in Haskell have a maximum arity of 64.
To parse strings of length 1 and 2 more efficiently, we
can utilize an ad-hoc solution that matches their characters.
This results in a speedup of up to 40 times compared to using
`readMaybe @Int` on my machine.
-}

-- When resolving names produced by Template Haskell (see thOrigRdrName
-- in GHC.ThToHs), we want ghc-prim:GHC.Types.List to yield an Exact name, not
-- an Orig name.
--
-- This matters for pretty-printing under ListTuplePuns. If we don't do it,
-- then -ddump-splices will print ''[] as ''GHC.Types.List.
--
-- Test case: th/T13776
--
isPunOcc_maybe :: Module -> OccName -> Maybe Name
isPunOcc_maybe mod occ
  | mod == gHC_TYPES, occ == occName listTyConName
  = Just listTyConName
  | mod == gHC_TYPES, occ == occName unboxedSoloDataConName
  = Just unboxedSoloDataConName
  | otherwise
  = isTupleTyOcc_maybe mod occ <|>
    isCTupleOcc_maybe  mod occ <|>
    isSumTyOcc_maybe   mod occ

mkTupleOcc :: NameSpace -> Boxity -> Arity -> OccName
-- No need to cache these, the caching is done in mk_tuple
mkTupleOcc ns Boxed   ar = mkOccName ns (mkBoxedTupleStr ns ar)
mkTupleOcc ns Unboxed ar = mkOccName ns (mkUnboxedTupleStr ns ar)

mkCTupleOcc :: NameSpace -> Arity -> OccName
mkCTupleOcc ns ar = mkOccName ns (mkConstraintTupleStr ar)

mkTupleStr :: Boxity -> NameSpace -> Arity -> String
mkTupleStr Boxed   = mkBoxedTupleStr
mkTupleStr Unboxed = mkUnboxedTupleStr

mkBoxedTupleStr :: NameSpace -> Arity -> String
mkBoxedTupleStr ns 0
  | isDataConNameSpace ns = "()"
  | otherwise             = "Unit"
mkBoxedTupleStr ns 1
  | isDataConNameSpace ns = "MkSolo"  -- See Note [One-tuples]
  | otherwise             = "Solo"
mkBoxedTupleStr ns ar
  | isDataConNameSpace ns = '(' : commas ar ++ ")"
  | otherwise             = "Tuple" ++ showInt ar ""


mkUnboxedTupleStr :: NameSpace -> Arity -> String
mkUnboxedTupleStr ns 0
  | isDataConNameSpace ns = "(##)"
  | otherwise             = "Unit#"
mkUnboxedTupleStr ns 1
  | isDataConNameSpace ns = "MkSolo#"  -- See Note [One-tuples]
  | otherwise             = "Solo#"
mkUnboxedTupleStr ns ar
  | isDataConNameSpace ns = "(#" ++ commas ar ++ "#)"
  | otherwise             = "Tuple" ++ show ar ++ "#"

mkConstraintTupleStr :: Arity -> String
mkConstraintTupleStr 0 = "CUnit"
mkConstraintTupleStr 1 = "CSolo"
mkConstraintTupleStr ar = "CTuple" ++ show ar

commas :: Arity -> String
commas ar = replicate (ar-1) ','

cTupleTyCon :: Arity -> TyCon
cTupleTyCon i
  | i > mAX_CTUPLE_SIZE = fstOf3 (mk_ctuple i) -- Build one specially
  | otherwise           = fstOf3 (cTupleArr ! i)

cTupleTyConName :: Arity -> Name
cTupleTyConName a = tyConName (cTupleTyCon a)

cTupleTyConNames :: [Name]
cTupleTyConNames = map cTupleTyConName (0 : [2..mAX_CTUPLE_SIZE])

cTupleTyConKeys :: UniqueSet
cTupleTyConKeys = fromListUniqueSet $ map getUnique cTupleTyConNames

isCTupleTyConName :: Name -> Bool
isCTupleTyConName n
 = assertPpr (isExternalName n) (ppr n) $
   getUnique n `memberUniqueSet` cTupleTyConKeys

-- | If the given name is that of a constraint tuple, return its arity.
cTupleTyConNameArity_maybe :: Name -> Maybe Arity
cTupleTyConNameArity_maybe n
  | not (isCTupleTyConName n) = Nothing
  | otherwise = fmap adjustArity (n `elemIndex` cTupleTyConNames)
  where
    -- Since `cTupleTyConNames` jumps straight from the `0` to the `2`
    -- case, we have to adjust accordingly our calculated arity.
    adjustArity a = if a > 0 then a + 1 else a

cTupleDataCon :: Arity -> DataCon
cTupleDataCon i
  | i > mAX_CTUPLE_SIZE = sndOf3 (mk_ctuple i) -- Build one specially
  | otherwise           = sndOf3 (cTupleArr ! i)

cTupleDataConName :: Arity -> Name
cTupleDataConName i = dataConName (cTupleDataCon i)

cTupleDataConNames :: [Name]
cTupleDataConNames = map cTupleDataConName (0 : [2..mAX_CTUPLE_SIZE])

cTupleSelId :: ConTag -- Superclass position
            -> Arity  -- Arity
            -> Id
cTupleSelId sc_pos arity
  | sc_pos > arity
  = panic ("cTupleSelId: index out of bounds: superclass position: "
           ++ show sc_pos ++ " > arity " ++ show arity)

  | sc_pos <= 0
  = panic ("cTupleSelId: Superclass positions start from 1. "
           ++ "(superclass position: " ++ show sc_pos
           ++ ", arity: " ++ show arity ++ ")")

  | arity < 1
  = panic ("cTupleSelId: Arity starts from 1. "
           ++ "(superclass position: " ++ show sc_pos
           ++ ", arity: " ++ show arity ++ ")")

  | arity > mAX_CTUPLE_SIZE
  = thdOf3 (mk_ctuple arity) ! (sc_pos - 1)  -- Build one specially

  | otherwise
  = thdOf3 (cTupleArr ! arity) ! (sc_pos - 1)

cTupleSelIdName :: ConTag -- Superclass position
                -> Arity  -- Arity
                -> Name
cTupleSelIdName sc_pos arity = idName (cTupleSelId sc_pos arity)

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

tupleDataConName :: Boxity -> Arity -> Name
tupleDataConName sort i = dataConName (tupleDataCon sort i)

mkPromotedPairTy :: Kind -> Kind -> Type -> Type -> Type
mkPromotedPairTy k1 k2 t1 t2 = mkTyConApp (promotedTupleDataCon Boxed 2) [k1,k2,t1,t2]

isPromotedPairType :: Type -> Maybe (Type, Type)
isPromotedPairType t
  | Just (tc, [_,_,x,y]) <- splitTyConApp_maybe t
  , tc == promotedTupleDataCon Boxed 2
  = Just (x, y)
  | otherwise = Nothing

boxedTupleArr, unboxedTupleArr :: Array Int (TyCon,DataCon)
boxedTupleArr   = listArray (0,mAX_TUPLE_SIZE) [mk_tuple Boxed   i | i <- [0..mAX_TUPLE_SIZE]]
unboxedTupleArr = listArray (0,mAX_TUPLE_SIZE) [mk_tuple Unboxed i | i <- [0..mAX_TUPLE_SIZE]]

-- | Cached type constructors, data constructors, and superclass selectors for
-- constraint tuples. The outer array is indexed by the arity of the constraint
-- tuple and the inner array is indexed by the superclass position.
cTupleArr :: Array Int (TyCon, DataCon, Array Int Id)
cTupleArr = listArray (0,mAX_CTUPLE_SIZE) [mk_ctuple i | i <- [0..mAX_CTUPLE_SIZE]]
  -- Although GHC does not make use of unary constraint tuples
  -- (see Note [Ignore unary constraint tuples] in GHC.Tc.Gen.HsType),
  -- this array creates one anyway. This is primarily motivated by the fact
  -- that (1) the indices of an Array must be contiguous, and (2) we would like
  -- the index of a constraint tuple in this Array to correspond to its Arity.
  -- We could envision skipping over the unary constraint tuple and having index
  -- 1 correspond to a 2-constraint tuple (and so on), but that's more
  -- complicated than it's worth.

-- | Given the TupleRep/SumRep tycon and list of RuntimeReps of the unboxed
-- tuple/sum arguments, produces the return kind of an unboxed tuple/sum type
-- constructor. @unboxedTupleSumKind [IntRep, LiftedRep] --> TYPE (TupleRep/SumRep
-- [IntRep, LiftedRep])@
unboxedTupleSumKind :: TyCon -> [Type] -> Kind
unboxedTupleSumKind tc rr_tys
  = mkTYPEapp (mkTyConApp tc [mkPromotedListTy runtimeRepTy rr_tys])

-- | Specialization of 'unboxedTupleSumKind' for tuples
unboxedTupleKind :: [Type] -> Kind
unboxedTupleKind = unboxedTupleSumKind tupleRepDataConTyCon

mk_tuple :: Boxity -> Int -> (TyCon,DataCon)
mk_tuple Boxed arity = (tycon, tuple_con)
  where
    tycon = mkTupleTyCon tc_name tc_binders tc_res_kind tuple_con
                         BoxedTuple flavour

    tc_binders  = mkTemplateAnonTyConBinders (replicate arity liftedTypeKind)
    tc_res_kind = liftedTypeKind
    flavour     = VanillaAlgTyCon (mkPrelTyConRepName tc_name)

    dc_tvs     = binderVars tc_binders
    dc_arg_tys = mkTyVarTys dc_tvs
    tuple_con  = pcDataCon dc_name dc_tvs dc_arg_tys tycon

    boxity  = Boxed
    modu    = gHC_INTERNAL_TUPLE
    tc_name = mkWiredInName modu (mkTupleOcc tcName boxity arity) tc_uniq
                         (ATyCon tycon) UserSyntax
    dc_name = mkWiredInName modu (mkTupleOcc dataName boxity arity) dc_uniq
                            (AConLike (RealDataCon tuple_con)) BuiltInSyntax
    tc_uniq = mkTupleTyConUnique   boxity arity
    dc_uniq = mkTupleDataConUnique boxity arity

mk_tuple Unboxed arity = (tycon, tuple_con)
  where
    tycon = mkTupleTyCon tc_name tc_binders tc_res_kind tuple_con
                         UnboxedTuple flavour

    -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
    -- Kind:  forall (k1:RuntimeRep) (k2:RuntimeRep). TYPE k1 -> TYPE k2 -> TYPE (TupleRep [k1, k2])
    tc_binders = mkTemplateTyConBinders (replicate arity runtimeRepTy)
                                        (\ks -> map mkTYPEapp ks)

    tc_res_kind = unboxedTupleKind rr_tys
    flavour     = VanillaAlgTyCon (mkPrelTyConRepName tc_name)

    dc_tvs               = binderVars tc_binders
    (rr_tvs, dc_arg_tvs) = splitAt arity dc_tvs
    rr_tys               = mkTyVarTys rr_tvs
    dc_arg_tys           = mkTyVarTys dc_arg_tvs
    tuple_con            = pcRepPolyDataCon dc_name dc_tvs conc_tvs dc_arg_tys tycon
    conc_tvs =
      mkNameEnv
        [ (tyVarName rr_tv, ConcreteFRR $ FixedRuntimeRepOrigin ty $ mkFRRUnboxedTuple pos)
        | rr_tv <- rr_tvs
        | ty <- dc_arg_tys
        | pos <- [1..arity] ]

    boxity  = Unboxed
    modu    = gHC_TYPES
    tc_name = mkWiredInName modu (mkTupleOcc tcName boxity arity) tc_uniq
                         (ATyCon tycon) UserSyntax
    dc_name = mkWiredInName modu (mkTupleOcc dataName boxity arity) dc_uniq
                            (AConLike (RealDataCon tuple_con)) BuiltInSyntax
    tc_uniq = mkTupleTyConUnique   boxity arity
    dc_uniq = mkTupleDataConUnique boxity arity

mk_ctuple :: Arity -> (TyCon, DataCon, Array ConTagZ Id)
mk_ctuple arity = (tycon, tuple_con, sc_sel_ids_arr)
  where
    tycon = mkClassTyCon tc_name binders roles
                         rhs klass
                         (mkPrelTyConRepName tc_name)

    klass     = mk_ctuple_class tycon sc_theta sc_sel_ids
    tuple_con = pcDataConConstraint dc_name tvs sc_theta tycon

    binders = mkTemplateAnonTyConBinders (replicate arity constraintKind)
    roles   = replicate arity Nominal
    rhs     = TupleTyCon{data_con = tuple_con, tup_sort = ConstraintTuple}

    modu    = gHC_CLASSES
    tc_name = mkWiredInName modu (mkCTupleOcc tcName arity) tc_uniq
                         (ATyCon tycon) UserSyntax
    dc_name = mkWiredInName modu (mkCTupleOcc dataName arity) dc_uniq
                            (AConLike (RealDataCon tuple_con)) BuiltInSyntax
    tc_uniq = mkCTupleTyConUnique   arity
    dc_uniq = mkCTupleDataConUnique arity

    tvs            = binderVars binders
    sc_theta       = map mkTyVarTy tvs
    sc_sel_ids     = [mk_sc_sel_id sc_pos | sc_pos <- [0..arity-1]]
    sc_sel_ids_arr = listArray (0,arity-1) sc_sel_ids

    mk_sc_sel_id sc_pos =
      let sc_sel_id_uniq = mkCTupleSelIdUnique sc_pos arity
          sc_sel_id_occ  = mkCTupleOcc tcName arity
          sc_sel_id_name = mkWiredInIdName
                             gHC_CLASSES
                             (occNameFS (mkSuperDictSelOcc sc_pos sc_sel_id_occ))
                             sc_sel_id_uniq
                             sc_sel_id
          sc_sel_id      = mkDictSelId sc_sel_id_name klass

      in sc_sel_id

unitTyCon :: TyCon
unitTyCon = tupleTyCon Boxed 0

unitTyConName :: Name
unitTyConName = tyConName unitTyCon

unitTyConKey :: Unique
unitTyConKey = getUnique unitTyCon

unitDataCon :: DataCon
unitDataCon   = head (tyConDataCons unitTyCon)

unitDataConId :: Id
unitDataConId = dataConWorkId unitDataCon

soloTyCon :: TyCon
soloTyCon = tupleTyCon Boxed 1

soloTyConName :: Name
soloTyConName = tyConName soloTyCon

pairTyCon :: TyCon
pairTyCon = tupleTyCon Boxed 2

unboxedUnitTy :: Type
unboxedUnitTy = mkTyConTy unboxedUnitTyCon

unboxedUnitTyCon :: TyCon
unboxedUnitTyCon = tupleTyCon Unboxed 0

unboxedUnitTyConName :: Name
unboxedUnitTyConName = tyConName unboxedUnitTyCon

unboxedUnitDataCon :: DataCon
unboxedUnitDataCon = tupleDataCon Unboxed 0

unboxedSoloTyCon :: TyCon
unboxedSoloTyCon = tupleTyCon Unboxed 1

unboxedSoloTyConName :: Name
unboxedSoloTyConName = tyConName unboxedSoloTyCon

unboxedSoloDataConName :: Name
unboxedSoloDataConName = tupleDataConName Unboxed 1

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
    str = "Sum" ++ show n ++ "#"

-- | OccName for i-th alternative of n-ary unboxed sum data constructor.
mkSumDataConOcc :: ConTag -> Arity -> OccName
mkSumDataConOcc alt n = mkOccName dataName str
  where
    -- No need to cache these, the caching is done in mk_sum
    str = '(' : '#' : ' ' : bars alt ++ '_' : bars (n - alt - 1) ++ " #)"
    bars i = intersperse ' ' $ replicate i '|'

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

-- | Specialization of 'unboxedTupleSumKind' for sums
unboxedSumKind :: [Type] -> Kind
unboxedSumKind = unboxedTupleSumKind sumRepDataConTyCon

-- | Create type constructor and data constructors for n-ary unboxed sum.
mk_sum :: Arity -> (TyCon, Array ConTagZ DataCon)
mk_sum arity = (tycon, sum_cons)
  where
    tycon   = mkSumTyCon tc_name tc_binders tc_res_kind (elems sum_cons)
                         UnboxedSumTyCon

    tc_binders = mkTemplateTyConBinders (replicate arity runtimeRepTy)
                                        (\ks -> map mkTYPEapp ks)

    tyvars = binderVars tc_binders

    tc_res_kind = unboxedSumKind rr_tys

    (rr_tvs, dc_arg_tvs) = splitAt arity tyvars
    rr_tys               = mkTyVarTys rr_tvs
    dc_arg_tys           = mkTyVarTys dc_arg_tvs

    conc_tvs =
      mkNameEnv
        [ (tyVarName rr_tv, ConcreteFRR $ FixedRuntimeRepOrigin ty $ mkFRRUnboxedSum (Just pos))
        | rr_tv <- rr_tvs
        | ty <- dc_arg_tys
        | pos <- [1..arity] ]

    tc_name = mkWiredInName gHC_TYPES (mkSumTyConOcc arity) tc_uniq
                            (ATyCon tycon) UserSyntax

    sum_cons = listArray (0,arity-1) [sum_con i | i <- [0..arity-1]]
    sum_con i =
      let dc = pcRepPolyDataCon dc_name
                  tyvars -- univ tyvars
                  conc_tvs
                  [dc_arg_tys !! i] -- arg types
                  tycon

          dc_name = mkWiredInName gHC_TYPES
                                  (mkSumDataConOcc i arity)
                                  (dc_uniq i)
                                  (AConLike (RealDataCon dc))
                                  BuiltInSyntax
      in dc

    tc_uniq   = mkSumTyConUnique   arity
    dc_uniq i = mkSumDataConUnique i arity

{-
************************************************************************
*                                                                      *
              Equality types and classes
*                                                                      *
********************************************************************* -}

-- See Note [The equality types story] in GHC.Builtin.Types.Prim
-- ((~~) :: forall k1 k2 (a :: k1) (b :: k2). a -> b -> Constraint)
--
-- It's tempting to put functional dependencies on (~~), but it's not
-- necessary because the functional-dependency coverage check looks
-- through superclasses, and (~#) is handled in that check.

eqTyCon,   heqTyCon,   coercibleTyCon   :: TyCon
eqClass,   heqClass,   coercibleClass   :: Class
eqDataCon, heqDataCon, coercibleDataCon :: DataCon
eqSCSelId, heqSCSelId, coercibleSCSelId :: Id

(eqTyCon, eqClass, eqDataCon, eqSCSelId)
  = (tycon, klass, datacon, sc_sel_id)
  where
    tycon     = mkClassTyCon eqTyConName binders roles
                             rhs klass
                             (mkPrelTyConRepName eqTyConName)
    klass     = mk_class tycon sc_pred sc_sel_id
    datacon   = pcDataConConstraint eqDataConName tvs [sc_pred] tycon

    -- Kind: forall k. k -> k -> Constraint
    binders   = mkTemplateTyConBinders [liftedTypeKind] (\[k] -> [k,k])
    roles     = [Nominal, Nominal, Nominal]
    rhs       = mkDataTyConRhs [datacon]

    tvs@[k,a,b] = binderVars binders
    sc_pred     = mkTyConApp eqPrimTyCon (mkTyVarTys [k,k,a,b])
    sc_sel_id   = mkDictSelId eqSCSelIdName klass

(heqTyCon, heqClass, heqDataCon, heqSCSelId)
  = (tycon, klass, datacon, sc_sel_id)
  where
    tycon     = mkClassTyCon heqTyConName binders roles
                             rhs klass
                             (mkPrelTyConRepName heqTyConName)
    klass     = mk_class tycon sc_pred sc_sel_id
    datacon   = pcDataConConstraint heqDataConName tvs [sc_pred] tycon

    -- Kind: forall k1 k2. k1 -> k2 -> Constraint
    binders   = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] id
    roles     = [Nominal, Nominal, Nominal, Nominal]
    rhs       = mkDataTyConRhs [datacon]

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
    datacon   = pcDataConConstraint coercibleDataConName tvs [sc_pred] tycon

    -- Kind: forall k. k -> k -> Constraint
    binders   = mkTemplateTyConBinders [liftedTypeKind] (\[k] -> [k,k])
    roles     = [Nominal, Representational, Representational]
    rhs       = mkDataTyConRhs [datacon]

    tvs@[k,a,b] = binderVars binders
    sc_pred     = mkTyConApp eqReprPrimTyCon (mkTyVarTys [k, k, a, b])
    sc_sel_id   = mkDictSelId coercibleSCSelIdName klass

mk_class :: TyCon -> PredType -> Id -> Class
mk_class tycon sc_pred sc_sel_id
  = mkClass (tyConName tycon) (tyConTyVars tycon) [] [sc_pred] [sc_sel_id]
            [] [] (mkAnd []) tycon

mk_ctuple_class :: TyCon -> ThetaType -> [Id] -> Class
mk_ctuple_class tycon sc_theta sc_sel_ids
  = mkClass (tyConName tycon) (tyConTyVars tycon) [] sc_theta sc_sel_ids
            [] [] (mkAnd []) tycon

{- *********************************************************************
*                                                                      *
                Multiplicity Polymorphism
*                                                                      *
********************************************************************* -}

{- Multiplicity polymorphism is implemented very similarly to representation
 polymorphism. We write in the multiplicity kind and the One and Many
 types which can appear in user programs. These are defined properly in GHC.Types.

data Multiplicity = One | Many
-}

multiplicityTyConName :: Name
multiplicityTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Multiplicity")
                          multiplicityTyConKey multiplicityTyCon

oneDataConName, manyDataConName :: Name
oneDataConName  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "One") oneDataConKey oneDataCon
manyDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "Many") manyDataConKey manyDataCon

multiplicityTy :: Type
multiplicityTy = mkTyConTy multiplicityTyCon

multiplicityTyCon :: TyCon
multiplicityTyCon = pcTyCon multiplicityTyConName Nothing []
                            [oneDataCon, manyDataCon]

oneDataCon, manyDataCon :: DataCon
oneDataCon = pcDataCon oneDataConName [] [] multiplicityTyCon
manyDataCon = pcDataCon manyDataConName [] [] multiplicityTyCon

oneDataConTy, manyDataConTy :: Type
oneDataConTy = mkTyConTy oneDataConTyCon
manyDataConTy = mkTyConTy manyDataConTyCon

oneDataConTyCon, manyDataConTyCon :: TyCon
oneDataConTyCon = promoteDataCon oneDataCon
manyDataConTyCon = promoteDataCon manyDataCon

multMulTyConName :: Name
multMulTyConName =
    mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "MultMul") multMulTyConKey multMulTyCon

multMulTyCon :: TyCon
multMulTyCon = mkFamilyTyCon multMulTyConName binders multiplicityTy Nothing
                         (BuiltInSynFamTyCon trivialBuiltInFamily)
                         Nothing
                         NotInjective
  where
    binders = mkTemplateAnonTyConBinders [multiplicityTy, multiplicityTy]

------------------------
-- type (->) :: forall (rep1 :: RuntimeRep) (rep2 :: RuntimeRep).
--              TYPE rep1 -> TYPE rep2 -> Type
-- type (->) = FUN 'Many
unrestrictedFunTyCon :: TyCon
unrestrictedFunTyCon
  = buildSynTyCon unrestrictedFunTyConName [] arrowKind []
                  (TyCoRep.TyConApp fUNTyCon [manyDataConTy])
  where
    arrowKind = mkTyConKind binders liftedTypeKind
    -- See also funTyCon
    binders = [ Bndr runtimeRep1TyVar (NamedTCB Inferred)
              , Bndr runtimeRep2TyVar (NamedTCB Inferred) ]
              ++ mkTemplateAnonTyConBinders [ mkTYPEapp runtimeRep1Ty
                                            , mkTYPEapp runtimeRep2Ty ]

unrestrictedFunTyConName :: Name
unrestrictedFunTyConName = mkWiredInTyConName BuiltInSyntax gHC_TYPES (fsLit "->")
                                              unrestrictedFunTyConKey unrestrictedFunTyCon


{- *********************************************************************
*                                                                      *
      Type synonyms (all declared in ghc-prim:GHC.Types)

         type CONSTRAINT   :: RuntimeRep -> Type -- primitive; cONSTRAINTKind
         type Constraint   = CONSTRAINT LiftedRep  :: Type    -- constraintKind

         type TYPE         :: RuntimeRep -> Type  -- primitive; tYPEKind
         type Type         = TYPE LiftedRep   :: Type         -- liftedTypeKind
         type UnliftedType = TYPE UnliftedRep :: Type         -- unliftedTypeKind

         type LiftedRep    = BoxedRep Lifted   :: RuntimeRep  -- liftedRepTy
         type UnliftedRep  = BoxedRep Unlifted :: RuntimeRep  -- unliftedRepTy

*                                                                      *
********************************************************************* -}

-- For these synonyms, see
-- Note [TYPE and CONSTRAINT] in GHC.Builtin.Types.Prim, and
-- Note [Using synonyms to compress types] in GHC.Core.Type

{- Note [Naked FunTy]
~~~~~~~~~~~~~~~~~~~~~
GHC.Core.TyCo.Rep.mkFunTy has assertions about the consistency of the argument
flag and arg/res types.  But when constructing the kinds of tYPETyCon and
cONSTRAINTTyCon we don't want to make these checks because
     TYPE :: RuntimeRep -> Type
i.e. TYPE :: RuntimeRep -> TYPE LiftedRep

so the check will loop infinitely.  Hence the use of a naked FunTy
constructor in tTYPETyCon and cONSTRAINTTyCon.
-}


----------------------
-- type Constraint = CONSTRAINT LiftedRep
constraintKindTyCon :: TyCon
constraintKindTyCon
  = buildSynTyCon constraintKindTyConName [] liftedTypeKind [] rhs
  where
    rhs = TyCoRep.TyConApp cONSTRAINTTyCon [liftedRepTy]

constraintKindTyConName :: Name
constraintKindTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Constraint")
                                             constraintKindTyConKey constraintKindTyCon

constraintKind :: Kind
constraintKind = mkTyConTy constraintKindTyCon

----------------------
-- type Type = TYPE LiftedRep
liftedTypeKindTyCon :: TyCon
liftedTypeKindTyCon
  = buildSynTyCon liftedTypeKindTyConName [] liftedTypeKind [] rhs
  where
    rhs = TyCoRep.TyConApp tYPETyCon [liftedRepTy]

liftedTypeKindTyConName :: Name
liftedTypeKindTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Type")
                                             liftedTypeKindTyConKey liftedTypeKindTyCon

liftedTypeKind, typeToTypeKind :: Type
liftedTypeKind = mkTyConTy liftedTypeKindTyCon
typeToTypeKind = liftedTypeKind `mkVisFunTyMany` liftedTypeKind

----------------------
-- type UnliftedType = TYPE ('BoxedRep 'Unlifted)
unliftedTypeKindTyCon :: TyCon
unliftedTypeKindTyCon
  = buildSynTyCon unliftedTypeKindTyConName [] liftedTypeKind [] rhs
  where
    rhs = TyCoRep.TyConApp tYPETyCon [unliftedRepTy]

unliftedTypeKindTyConName :: Name
unliftedTypeKindTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "UnliftedType")
                                                unliftedTypeKindTyConKey unliftedTypeKindTyCon

unliftedTypeKind :: Type
unliftedTypeKind = mkTyConTy unliftedTypeKindTyCon


{- *********************************************************************
*                                                                      *
      data Levity = Lifted | Unlifted
*                                                                      *
********************************************************************* -}

levityTyConName, liftedDataConName, unliftedDataConName :: Name
levityTyConName     = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Levity")   levityTyConKey     levityTyCon
liftedDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "Lifted")   liftedDataConKey   liftedDataCon
unliftedDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "Unlifted") unliftedDataConKey unliftedDataCon

levityTyCon :: TyCon
levityTyCon = pcTyCon levityTyConName Nothing [] [liftedDataCon,unliftedDataCon]

levityTy :: Type
levityTy = mkTyConTy levityTyCon

liftedDataCon, unliftedDataCon :: DataCon
liftedDataCon = pcSpecialDataCon liftedDataConName
    [] levityTyCon (Levity Lifted)
unliftedDataCon = pcSpecialDataCon unliftedDataConName
    [] levityTyCon (Levity Unlifted)

liftedDataConTyCon :: TyCon
liftedDataConTyCon = promoteDataCon liftedDataCon

unliftedDataConTyCon :: TyCon
unliftedDataConTyCon = promoteDataCon unliftedDataCon

liftedDataConTy :: Type
liftedDataConTy = mkTyConTy liftedDataConTyCon

unliftedDataConTy :: Type
unliftedDataConTy = mkTyConTy unliftedDataConTyCon


{- *********************************************************************
*                                                                      *
    See Note [Wiring in RuntimeRep]
        data RuntimeRep = VecRep VecCount VecElem
                        | TupleRep [RuntimeRep]
                        | SumRep [RuntimeRep]
                        | BoxedRep Levity
                        | IntRep | Int8Rep | ...etc...
*                                                                      *
********************************************************************* -}

{- Note [Wiring in RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The RuntimeRep type (and friends) in GHC.Types has a bunch of constructors,
making it a pain to wire in. To ease the pain somewhat, we use lists of
the different bits, like Uniques, Names, DataCons. These lists must be
kept in sync with each other. The rule is this: use the order as declared
in GHC.Types. All places where such lists exist should contain a reference
to this Note, so a search for this Note's name should find all the lists.

See also Note [Getting from RuntimeRep to PrimRep] in GHC.Types.RepType.
-}

runtimeRepTyCon :: TyCon
runtimeRepTyCon = pcTyCon runtimeRepTyConName Nothing []
    -- Here we list all the data constructors
    -- of the RuntimeRep data type
    (vecRepDataCon : tupleRepDataCon :
     sumRepDataCon : boxedRepDataCon :
     runtimeRepSimpleDataCons)

runtimeRepTy :: Type
runtimeRepTy = mkTyConTy runtimeRepTyCon

runtimeRepTyConName, vecRepDataConName, tupleRepDataConName, sumRepDataConName, boxedRepDataConName :: Name
runtimeRepTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "RuntimeRep") runtimeRepTyConKey runtimeRepTyCon

vecRepDataConName   = mk_runtime_rep_dc_name (fsLit "VecRep")   vecRepDataConKey   vecRepDataCon
tupleRepDataConName = mk_runtime_rep_dc_name (fsLit "TupleRep") tupleRepDataConKey tupleRepDataCon
sumRepDataConName   = mk_runtime_rep_dc_name (fsLit "SumRep")   sumRepDataConKey   sumRepDataCon
boxedRepDataConName = mk_runtime_rep_dc_name (fsLit "BoxedRep") boxedRepDataConKey boxedRepDataCon

mk_runtime_rep_dc_name :: FastString -> Unique -> DataCon -> Name
mk_runtime_rep_dc_name fs u dc = mkWiredInDataConName UserSyntax gHC_TYPES fs u dc

boxedRepDataCon :: DataCon
boxedRepDataCon = pcSpecialDataCon boxedRepDataConName
  [ levityTy ] runtimeRepTyCon (RuntimeRep prim_rep_fun)
  where
    -- See Note [Getting from RuntimeRep to PrimRep] in RepType
    prim_rep_fun [lev]
      = case tyConAppTyCon_maybe lev of
          Just tc -> case tyConPromDataConInfo tc of
            Levity l -> [BoxedRep (Just l)]
            _        -> [BoxedRep Nothing]
          Nothing    -> [BoxedRep Nothing]
    prim_rep_fun args
      = pprPanic "boxedRepDataCon" (ppr args)


boxedRepDataConTyCon :: TyCon
boxedRepDataConTyCon = promoteDataCon boxedRepDataCon

tupleRepDataCon :: DataCon
tupleRepDataCon = pcSpecialDataCon tupleRepDataConName [ mkListTy runtimeRepTy ]
                                   runtimeRepTyCon (RuntimeRep prim_rep_fun)
  where
    -- See Note [Getting from RuntimeRep to PrimRep] in GHC.Types.RepType
    prim_rep_fun [rr_ty_list]
      = concatMap (runtimeRepPrimRep doc) rr_tys
      where
        rr_tys = extractPromotedList rr_ty_list
        doc    = text "tupleRepDataCon" <+> ppr rr_tys
    prim_rep_fun args
      = pprPanic "tupleRepDataCon" (ppr args)

tupleRepDataConTyCon :: TyCon
tupleRepDataConTyCon = promoteDataCon tupleRepDataCon

sumRepDataCon :: DataCon
sumRepDataCon = pcSpecialDataCon sumRepDataConName [ mkListTy runtimeRepTy ]
                                 runtimeRepTyCon (RuntimeRep prim_rep_fun)
  where
    -- See Note [Getting from RuntimeRep to PrimRep] in GHC.Types.RepType
    prim_rep_fun [rr_ty_list]
      = map slotPrimRep (toList (ubxSumRepType prim_repss))
      where
        rr_tys     = extractPromotedList rr_ty_list
        doc        = text "sumRepDataCon" <+> ppr rr_tys
        prim_repss = map (runtimeRepPrimRep doc) rr_tys
    prim_rep_fun args
      = pprPanic "sumRepDataCon" (ppr args)

sumRepDataConTyCon :: TyCon
sumRepDataConTyCon = promoteDataCon sumRepDataCon

-- See Note [Wiring in RuntimeRep]
-- See Note [Getting from RuntimeRep to PrimRep] in GHC.Types.RepType
runtimeRepSimpleDataCons :: [DataCon]
runtimeRepSimpleDataCons
  = zipWith mk_runtime_rep_dc runtimeRepSimpleDataConKeys
            [ (fsLit "IntRep",    IntRep)
            , (fsLit "Int8Rep",   Int8Rep)
            , (fsLit "Int16Rep",  Int16Rep)
            , (fsLit "Int32Rep",  Int32Rep)
            , (fsLit "Int64Rep",  Int64Rep)
            , (fsLit "WordRep",   WordRep)
            , (fsLit "Word8Rep",  Word8Rep)
            , (fsLit "Word16Rep", Word16Rep)
            , (fsLit "Word32Rep", Word32Rep)
            , (fsLit "Word64Rep", Word64Rep)
            , (fsLit "AddrRep",   AddrRep)
            , (fsLit "FloatRep",  FloatRep)
            , (fsLit "DoubleRep", DoubleRep) ]
  where
    mk_runtime_rep_dc :: Unique -> (FastString, PrimRep) -> DataCon
    mk_runtime_rep_dc uniq (fs, primrep)
      = data_con
      where
        data_con = pcSpecialDataCon dc_name [] runtimeRepTyCon (RuntimeRep (\_ -> [primrep]))
        dc_name  = mk_runtime_rep_dc_name fs uniq data_con

-- See Note [Wiring in RuntimeRep]
intRepDataConTy,
  int8RepDataConTy, int16RepDataConTy, int32RepDataConTy, int64RepDataConTy,
  wordRepDataConTy,
  word8RepDataConTy, word16RepDataConTy, word32RepDataConTy, word64RepDataConTy,
  addrRepDataConTy,
  floatRepDataConTy, doubleRepDataConTy :: RuntimeRepType
[intRepDataConTy,
   int8RepDataConTy, int16RepDataConTy, int32RepDataConTy, int64RepDataConTy,
   wordRepDataConTy,
   word8RepDataConTy, word16RepDataConTy, word32RepDataConTy, word64RepDataConTy,
   addrRepDataConTy,
   floatRepDataConTy, doubleRepDataConTy
   ]
  = map (mkTyConTy . promoteDataCon) runtimeRepSimpleDataCons

----------------------
-- | @type ZeroBitRep = 'Tuple '[]
zeroBitRepTyCon :: TyCon
zeroBitRepTyCon
  = buildSynTyCon zeroBitRepTyConName [] runtimeRepTy [] rhs
  where
    rhs = TyCoRep.TyConApp tupleRepDataConTyCon [mkPromotedListTy runtimeRepTy []]

zeroBitRepTyConName :: Name
zeroBitRepTyConName  = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "ZeroBitRep")
                                          zeroBitRepTyConKey  zeroBitRepTyCon

zeroBitRepTy :: RuntimeRepType
zeroBitRepTy = mkTyConTy zeroBitRepTyCon

----------------------
-- @type ZeroBitType = TYPE ZeroBitRep
zeroBitTypeTyCon :: TyCon
zeroBitTypeTyCon
  = buildSynTyCon zeroBitTypeTyConName [] liftedTypeKind [] rhs
  where
    rhs = TyCoRep.TyConApp tYPETyCon [zeroBitRepTy]

zeroBitTypeTyConName :: Name
zeroBitTypeTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "ZeroBitType")
                                          zeroBitTypeTyConKey zeroBitTypeTyCon

zeroBitTypeKind :: Type
zeroBitTypeKind = mkTyConTy zeroBitTypeTyCon

----------------------
-- | @type LiftedRep = 'BoxedRep 'Lifted@
liftedRepTyCon :: TyCon
liftedRepTyCon
  = buildSynTyCon liftedRepTyConName [] runtimeRepTy [] rhs
  where
    rhs = TyCoRep.TyConApp boxedRepDataConTyCon [liftedDataConTy]

liftedRepTyConName :: Name
liftedRepTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "LiftedRep")
                                        liftedRepTyConKey liftedRepTyCon

liftedRepTy :: RuntimeRepType
liftedRepTy = mkTyConTy liftedRepTyCon

----------------------
-- | @type UnliftedRep = 'BoxedRep 'Unlifted@
unliftedRepTyCon :: TyCon
unliftedRepTyCon
  = buildSynTyCon unliftedRepTyConName [] runtimeRepTy [] rhs
  where
    rhs = TyCoRep.TyConApp boxedRepDataConTyCon [unliftedDataConTy]

unliftedRepTyConName :: Name
unliftedRepTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "UnliftedRep")
                                          unliftedRepTyConKey unliftedRepTyCon

unliftedRepTy :: RuntimeRepType
unliftedRepTy = mkTyConTy unliftedRepTyCon


{- *********************************************************************
*                                                                      *
         VecCount, VecElem
*                                                                      *
********************************************************************* -}

vecCountTyConName :: Name
vecCountTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "VecCount") vecCountTyConKey vecCountTyCon

vecElemTyConName :: Name
vecElemTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "VecElem") vecElemTyConKey vecElemTyCon

vecRepDataCon :: DataCon
vecRepDataCon = pcSpecialDataCon vecRepDataConName [ mkTyConTy vecCountTyCon
                                                   , mkTyConTy vecElemTyCon ]
                                 runtimeRepTyCon
                                 (RuntimeRep prim_rep_fun)
  where
    -- See Note [Getting from RuntimeRep to PrimRep] in GHC.Types.RepType
    prim_rep_fun [count, elem]
      | VecCount n <- tyConPromDataConInfo (tyConAppTyCon count)
      , VecElem  e <- tyConPromDataConInfo (tyConAppTyCon elem)
      = [VecRep n e]
    prim_rep_fun args
      = pprPanic "vecRepDataCon" (ppr args)

vecRepDataConTyCon :: TyCon
vecRepDataConTyCon = promoteDataCon vecRepDataCon

vecCountTyCon :: TyCon
vecCountTyCon = pcTyCon vecCountTyConName Nothing [] vecCountDataCons

-- See Note [Wiring in RuntimeRep]
vecCountDataCons :: [DataCon]
vecCountDataCons = zipWith mk_vec_count_dc [1..6] vecCountDataConKeys
  where
    mk_vec_count_dc logN key = con
      where
        n = 2^(logN :: Int)
        name = mk_runtime_rep_dc_name (fsLit ("Vec" ++ show n)) key con
        con = pcSpecialDataCon name [] vecCountTyCon (VecCount n)

-- See Note [Wiring in RuntimeRep]
vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy,
  vec64DataConTy :: Type
[vec2DataConTy, vec4DataConTy, vec8DataConTy, vec16DataConTy, vec32DataConTy,
  vec64DataConTy] = map (mkTyConTy . promoteDataCon) vecCountDataCons

vecElemTyCon :: TyCon
vecElemTyCon = pcTyCon vecElemTyConName Nothing [] vecElemDataCons

-- See Note [Wiring in RuntimeRep]
vecElemDataCons :: [DataCon]
vecElemDataCons = zipWith3 mk_vec_elem_dc
  [ fsLit "Int8ElemRep", fsLit "Int16ElemRep", fsLit "Int32ElemRep", fsLit "Int64ElemRep"
  , fsLit "Word8ElemRep", fsLit "Word16ElemRep", fsLit "Word32ElemRep", fsLit "Word64ElemRep"
  , fsLit "FloatElemRep", fsLit "DoubleElemRep" ]
  [ Int8ElemRep, Int16ElemRep, Int32ElemRep, Int64ElemRep
  , Word8ElemRep, Word16ElemRep, Word32ElemRep, Word64ElemRep
  , FloatElemRep, DoubleElemRep ]
    vecElemDataConKeys
  where
    mk_vec_elem_dc nameFs elemRep key = con
      where
        name = mk_runtime_rep_dc_name nameFs key con
        con = pcSpecialDataCon name [] vecElemTyCon (VecElem elemRep)

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

{- *********************************************************************
*                                                                      *
     The boxed primitive types: Char, Int, etc
*                                                                      *
********************************************************************* -}

charTy :: Type
charTy = mkTyConTy charTyCon

charTyCon :: TyCon
charTyCon   = pcTyCon charTyConName
                   (Just (CType NoSourceText Nothing
                                  (NoSourceText,fsLit "HsChar")))
                   [] [charDataCon]
charDataCon :: DataCon
charDataCon = pcDataCon charDataConName [] [charPrimTy] charTyCon

stringTy :: Type
stringTy = mkTyConTy stringTyCon

stringTyCon :: TyCon
-- We have this wired-in so that Haskell literal strings
-- get type String (in hsLitType), which in turn influences
-- inferred types and error messages
stringTyCon = buildSynTyCon stringTyConName
                            [] liftedTypeKind []
                            (mkListTy charTy)

intTy :: Type
intTy = mkTyConTy intTyCon

intTyCon :: TyCon
intTyCon = pcTyCon intTyConName
               (Just (CType NoSourceText Nothing (NoSourceText,fsLit "HsInt")))
                 [] [intDataCon]
intDataCon :: DataCon
intDataCon = pcDataCon intDataConName [] [intPrimTy] intTyCon

wordTy :: Type
wordTy = mkTyConTy wordTyCon

wordTyCon :: TyCon
wordTyCon = pcTyCon wordTyConName
            (Just (CType NoSourceText Nothing (NoSourceText, fsLit "HsWord")))
               [] [wordDataCon]
wordDataCon :: DataCon
wordDataCon = pcDataCon wordDataConName [] [wordPrimTy] wordTyCon

word8Ty :: Type
word8Ty = mkTyConTy word8TyCon

word8TyCon :: TyCon
word8TyCon = pcTyCon word8TyConName
                     (Just (CType NoSourceText Nothing
                            (NoSourceText, fsLit "HsWord8"))) []
                     [word8DataCon]
word8DataCon :: DataCon
word8DataCon = pcDataCon word8DataConName [] [word8PrimTy] word8TyCon

floatTy :: Type
floatTy = mkTyConTy floatTyCon

floatTyCon :: TyCon
floatTyCon   = pcTyCon floatTyConName
                      (Just (CType NoSourceText Nothing
                             (NoSourceText, fsLit "HsFloat"))) []
                      [floatDataCon]
floatDataCon :: DataCon
floatDataCon = pcDataCon         floatDataConName [] [floatPrimTy] floatTyCon

doubleTy :: Type
doubleTy = mkTyConTy doubleTyCon

doubleTyCon :: TyCon
doubleTyCon = pcTyCon doubleTyConName
                      (Just (CType NoSourceText Nothing
                             (NoSourceText,fsLit "HsDouble"))) []
                      [doubleDataCon]

doubleDataCon :: DataCon
doubleDataCon = pcDataCon doubleDataConName [] [doublePrimTy] doubleTyCon

{- *********************************************************************
*                                                                      *
              Boxing data constructors
*                                                                      *
********************************************************************* -}

{- Note [Boxing constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In ghc-prim:GHC.Types we have a family of data types, one for each RuntimeRep
that "box" unlifted values into a (boxed, lifted) value of kind Type. For example

  type Int8Box :: TYPE Int8Rep -> Type
  data Int8Box (a :: TYPE Int8Rep) = MkInt8Box a
    -- MkInt8Box :: forall (a :: TYPE Int8Rep). a -> Int8Box a

Then we can package an `Int8#` into an `Int8Box` with `MkInt8Box`.  We can also
package up a (lifted) Constraint as a value of kind Type.

There are a fixed number of RuntimeReps, so we only need a fixed number
of boxing types.  (For TupleRep we need to box recursively; not yet done,
see #22336.)

This is used:

* In desugaring, when we need to package up a bunch of values into a tuple,
  for example when desugaring arrows.  See Note [Big tuples] in GHC.Core.Make.

* In let-floating when we want to float an unlifted sub-expression.
  See Note [Floating MFEs of unlifted type] in GHC.Core.Opt.SetLevels

In this module we make wired-in data type declarations for all of
these boxing functions.  The goal is to define boxingDataCon_maybe.

Wrinkles
(W1) The runtime system has special treatment (e.g. commoning up during GC)
     for Int and Char values. See  Note [CHARLIKE and INTLIKE closures] and
     Note [Precomputed static closures] in the RTS.

     So we treat Int# and Char# specially, in specialBoxingDataCon_maybe
-}

data BoxingInfo b
  = BI_NoBoxNeeded   -- The type has kind Type, so there is nothing to do

  | BI_NoBoxAvailable  -- The type does not have kind Type, but sadly we
                       -- don't have a boxing data constructor either

  | BI_Box             -- The type does not have kind Type, and we do have a
                       -- boxing data constructor; here it is
      { bi_data_con   :: DataCon
      , bi_inst_con   :: Expr b
      , bi_boxed_type :: Type }
    -- e.g. BI_Box { bi_data_con = I#, bi_inst_con = I#, bi_boxed_type = Int }
    --        recall: data Int = I# Int#
    --
    --      BI_Box { bi_data_con = MkInt8Box, bi_inst_con = MkInt8Box @ty
    --             , bi_boxed_type = Int8Box ty }
    --        recall: data Int8Box (a :: TYPE Int8Rep) = MkIntBox a

boxingDataCon :: Type -> BoxingInfo b
-- ^ Given a type 'ty', if 'ty' is not of kind Type, return a data constructor that
--   will box it, and the type of the boxed thing, which /does/ now have kind Type.
-- See Note [Boxing constructors]
boxingDataCon ty
  | tcIsLiftedTypeKind kind
  = BI_NoBoxNeeded    -- Fast path for Type

  | Just box_con <- specialBoxingDataCon_maybe ty
  = BI_Box { bi_data_con = box_con, bi_inst_con = mkConApp box_con []
           , bi_boxed_type = tyConNullaryTy (dataConTyCon box_con) }

  | Just box_con <- lookupTypeMap boxingDataConMap kind
  = BI_Box { bi_data_con = box_con, bi_inst_con = mkConApp box_con [Type ty]
           , bi_boxed_type = mkTyConApp (dataConTyCon box_con) [ty] }

  | otherwise
  = BI_NoBoxAvailable

  where
    kind = typeKind ty

specialBoxingDataCon_maybe :: Type -> Maybe DataCon
-- ^ See Note [Boxing constructors] wrinkle (W1)
specialBoxingDataCon_maybe ty
  = case splitTyConApp_maybe ty of
      Just (tc, _) | tc `hasKey` intPrimTyConKey  -> Just intDataCon
                   | tc `hasKey` charPrimTyConKey -> Just charDataCon
      _ -> Nothing

boxingDataConMap :: TypeMap DataCon
-- See Note [Boxing constructors]
boxingDataConMap = foldl add emptyTypeMap boxingDataCons
  where
    add bdcm (kind, boxing_con) = extendTypeMap bdcm kind boxing_con

boxingDataCons :: [(Kind, DataCon)]
-- The Kind is the kind of types for which the DataCon is the right boxing
boxingDataCons = zipWith mkBoxingDataCon
  (map mkBoxingTyConUnique [1..])
  [ (mkTYPEapp wordRepDataConTy, fsLit "WordBox", fsLit "MkWordBox")
  , (mkTYPEapp intRepDataConTy,  fsLit "IntBox",  fsLit "MkIntBox")

  , (mkTYPEapp floatRepDataConTy,  fsLit "FloatBox",  fsLit "MkFloatBox")
  , (mkTYPEapp doubleRepDataConTy,  fsLit "DoubleBox",  fsLit "MkDoubleBox")

  , (mkTYPEapp int8RepDataConTy,  fsLit "Int8Box",  fsLit "MkInt8Box")
  , (mkTYPEapp int16RepDataConTy, fsLit "Int16Box", fsLit "MkInt16Box")
  , (mkTYPEapp int32RepDataConTy, fsLit "Int32Box", fsLit "MkInt32Box")
  , (mkTYPEapp int64RepDataConTy, fsLit "Int64Box", fsLit "MkInt64Box")

  , (mkTYPEapp word8RepDataConTy,  fsLit "Word8Box",   fsLit "MkWord8Box")
  , (mkTYPEapp word16RepDataConTy, fsLit "Word16Box",  fsLit "MkWord16Box")
  , (mkTYPEapp word32RepDataConTy, fsLit "Word32Box",  fsLit "MkWord32Box")
  , (mkTYPEapp word64RepDataConTy, fsLit "Word64Box",  fsLit "MkWord64Box")

  , (unliftedTypeKind, fsLit "LiftBox", fsLit "MkLiftBox")
  , (constraintKind,   fsLit "DictBox", fsLit "MkDictBox") ]

mkBoxingDataCon :: Unique -> (Kind, FastString, FastString) -> (Kind, DataCon)
mkBoxingDataCon uniq_tc (kind, fs_tc, fs_dc)
  = (kind, dc)
  where
    uniq_dc = boxingDataConUnique uniq_tc

    (tv:_) = mkTemplateTyVars (repeat kind)
    tc = pcTyCon tc_name Nothing [tv] [dc]
    tc_name = mkWiredInTyConName UserSyntax gHC_TYPES fs_tc uniq_tc tc

    dc | isConstraintKind kind
       = pcDataConConstraint dc_name [tv] [mkTyVarTy tv] tc
       | otherwise
       = pcDataCon           dc_name [tv] [mkTyVarTy tv] tc
    dc_name = mkWiredInDataConName UserSyntax gHC_TYPES fs_dc uniq_dc dc

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
boolTyCon = pcTyCon boolTyConName
                    (Just (CType NoSourceText Nothing
                           (NoSourceText, fsLit "HsBool")))
                    [] [falseDataCon, trueDataCon]

falseDataCon, trueDataCon :: DataCon
falseDataCon = pcDataCon falseDataConName [] [] boolTyCon
trueDataCon  = pcDataCon trueDataConName  [] [] boolTyCon

falseDataConId, trueDataConId :: Id
falseDataConId = dataConWorkId falseDataCon
trueDataConId  = dataConWorkId trueDataCon

orderingTyCon :: TyCon
orderingTyCon = pcTyCon orderingTyConName Nothing
                        [] [ordLTDataCon, ordEQDataCon, ordGTDataCon]

ordLTDataCon, ordEQDataCon, ordGTDataCon :: DataCon
ordLTDataCon = pcDataCon ordLTDataConName  [] [] orderingTyCon
ordEQDataCon = pcDataCon ordEQDataConName  [] [] orderingTyCon
ordGTDataCon = pcDataCon ordGTDataConName  [] [] orderingTyCon

ordLTDataConId, ordEQDataConId, ordGTDataConId :: Id
ordLTDataConId = dataConWorkId ordLTDataCon
ordEQDataConId = dataConWorkId ordEQDataCon
ordGTDataConId = dataConWorkId ordGTDataCon

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
listTyCon = pcTyCon listTyConName Nothing [alphaTyVar] [nilDataCon, consDataCon]

-- See also Note [Empty lists] in GHC.Hs.Expr.
nilDataCon :: DataCon
nilDataCon  = pcDataCon nilDataConName alpha_tyvar [] listTyCon

consDataCon :: DataCon
consDataCon = pcDataConWithFixity True {- Declared infix -}
               consDataConName
               alpha_tyvar [] noConcreteTyVars alpha_tyvar []
               (map linear [alphaTy, mkTyConApp listTyCon alpha_ty])
               listTyCon

-- Interesting: polymorphic recursion would help here.
-- We can't use (mkListTy alphaTy) in the defn of consDataCon, else mkListTy
-- gets the over-specific type (Type -> Type)

-- Wired-in type Maybe

maybeTyCon :: TyCon
maybeTyCon = pcTyCon maybeTyConName Nothing alpha_tyvar
                     [nothingDataCon, justDataCon]

nothingDataCon :: DataCon
nothingDataCon = pcDataCon nothingDataConName alpha_tyvar [] maybeTyCon

justDataCon :: DataCon
justDataCon = pcDataCon justDataConName alpha_tyvar [alphaTy] maybeTyCon

mkPromotedMaybeTy :: Kind -> Maybe Type -> Type
mkPromotedMaybeTy k (Just x) = mkTyConApp promotedJustDataCon [k,x]
mkPromotedMaybeTy k Nothing  = mkTyConApp promotedNothingDataCon [k]

mkMaybeTy :: Type -> Kind
mkMaybeTy t = mkTyConApp maybeTyCon [t]

isPromotedMaybeTy :: Type -> Maybe (Maybe Type)
isPromotedMaybeTy t
  | Just (tc,[_,x]) <- splitTyConApp_maybe t, tc == promotedJustDataCon = return $ Just x
  | Just (tc,[_])   <- splitTyConApp_maybe t, tc == promotedNothingDataCon = return $ Nothing
  | otherwise = Nothing


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
-- RuntimeRep specifications. Boxed 1-tuples are flattened.
-- See Note [One-tuples]
mkTupleTy :: Boxity -> [Type] -> Type
-- Special case for *boxed* 1-tuples, which are represented by the type itself
mkTupleTy Boxed   [ty] = ty
mkTupleTy boxity  tys  = mkTupleTy1 boxity tys

-- | Make a tuple type. The list of types should /not/ include any
-- RuntimeRep specifications. Boxed 1-tuples are *not* flattened.
-- See Note [One-tuples] and Note [Don't flatten tuples from HsSyn]
-- in "GHC.Core.Make"
mkTupleTy1 :: Boxity -> [Type] -> Type
mkTupleTy1 Boxed   tys  = mkTyConApp (tupleTyCon Boxed (length tys)) tys
mkTupleTy1 Unboxed tys  = mkTyConApp (tupleTyCon Unboxed (length tys))
                                     (map getRuntimeRep tys ++ tys)

-- | Build the type of a small tuple that holds the specified type of thing
-- Flattens 1-tuples. See Note [One-tuples].
mkBoxedTupleTy :: [Type] -> Type
mkBoxedTupleTy tys = mkTupleTy Boxed tys

unitTy :: Type
unitTy = mkTupleTy Boxed []

-- Make a constraint tuple, flattening a 1-tuple as usual
-- If we get a constraint tuple that is bigger than the pre-built
--   ones (in ghc-prim:GHC.Tuple), then just make one up anyway; it won't
--   have an info table in the RTS, so we can't use it at runtime.  But
--   this is used only in filling in extra-constraint wildcards, so it
--   never is used at runtime anyway
--   See GHC.Tc.Gen.HsType Note [Extra-constraint holes in partial type signatures]
mkConstraintTupleTy :: [Type] -> Type
mkConstraintTupleTy [ty] = ty
mkConstraintTupleTy tys = mkTyConApp (cTupleTyCon (length tys)) tys


{- *********************************************************************
*                                                                      *
            The sum types
*                                                                      *
************************************************************************
-}

mkSumTy :: [Type] -> Type
mkSumTy tys = mkTyConApp (sumTyCon (length tys))
                         (map getRuntimeRep tys ++ tys)

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
promotedLTDataCon     = promoteDataCon ordLTDataCon
promotedEQDataCon     = promoteDataCon ordEQDataCon
promotedGTDataCon     = promoteDataCon ordGTDataCon

-- Promoted List
promotedConsDataCon, promotedNilDataCon :: TyCon
promotedConsDataCon   = promoteDataCon consDataCon
promotedNilDataCon    = promoteDataCon nilDataCon

-- | Make a *promoted* list.
mkPromotedListTy :: Kind   -- ^ of the elements of the list
                 -> [Type] -- ^ elements
                 -> Type
mkPromotedListTy k tys
  = foldr cons nil tys
  where
    cons :: Type  -- element
         -> Type  -- list
         -> Type
    cons elt list = mkTyConApp promotedConsDataCon [k, elt, list]

    nil :: Type
    nil = mkTyConApp promotedNilDataCon [k]

-- | Extract the elements of a promoted list. Panics if the type is not a
-- promoted list
extractPromotedList :: Type    -- ^ The promoted list
                    -> [Type]
extractPromotedList tys = go tys
  where
    go list_ty
      | Just (tc, [_k, t, ts]) <- splitTyConApp_maybe list_ty
      = assert (tc `hasKey` consDataConKey) $
        t : go ts

      | Just (tc, [_k]) <- splitTyConApp_maybe list_ty
      = assert (tc `hasKey` nilDataConKey)
        []

      | otherwise
      = pprPanic "extractPromotedList" (ppr tys)

---------------------------------------
-- ghc-bignum
---------------------------------------

integerTyConName
   , integerISDataConName
   , integerIPDataConName
   , integerINDataConName
   :: Name
integerTyConName
   = mkWiredInTyConName
      UserSyntax
      gHC_INTERNAL_NUM_INTEGER
      (fsLit "Integer")
      integerTyConKey
      integerTyCon
integerISDataConName
   = mkWiredInDataConName
      UserSyntax
      gHC_INTERNAL_NUM_INTEGER
      (fsLit "IS")
      integerISDataConKey
      integerISDataCon
integerIPDataConName
   = mkWiredInDataConName
      UserSyntax
      gHC_INTERNAL_NUM_INTEGER
      (fsLit "IP")
      integerIPDataConKey
      integerIPDataCon
integerINDataConName
   = mkWiredInDataConName
      UserSyntax
      gHC_INTERNAL_NUM_INTEGER
      (fsLit "IN")
      integerINDataConKey
      integerINDataCon

integerTy :: Type
integerTy = mkTyConTy integerTyCon

integerTyCon :: TyCon
integerTyCon = pcTyCon integerTyConName Nothing []
                  [integerISDataCon, integerIPDataCon, integerINDataCon]

integerISDataCon :: DataCon
integerISDataCon = pcDataCon integerISDataConName [] [intPrimTy] integerTyCon

integerIPDataCon :: DataCon
integerIPDataCon = pcDataCon integerIPDataConName [] [byteArrayPrimTy] integerTyCon

integerINDataCon :: DataCon
integerINDataCon = pcDataCon integerINDataConName [] [byteArrayPrimTy] integerTyCon

naturalTyConName
   , naturalNSDataConName
   , naturalNBDataConName
   :: Name
naturalTyConName
   = mkWiredInTyConName
      UserSyntax
      gHC_INTERNAL_NUM_NATURAL
      (fsLit "Natural")
      naturalTyConKey
      naturalTyCon
naturalNSDataConName
   = mkWiredInDataConName
      UserSyntax
      gHC_INTERNAL_NUM_NATURAL
      (fsLit "NS")
      naturalNSDataConKey
      naturalNSDataCon
naturalNBDataConName
   = mkWiredInDataConName
      UserSyntax
      gHC_INTERNAL_NUM_NATURAL
      (fsLit "NB")
      naturalNBDataConKey
      naturalNBDataCon

naturalTy :: Type
naturalTy = mkTyConTy naturalTyCon

naturalTyCon :: TyCon
naturalTyCon = pcTyCon naturalTyConName Nothing []
                  [naturalNSDataCon, naturalNBDataCon]

naturalNSDataCon :: DataCon
naturalNSDataCon = pcDataCon naturalNSDataConName [] [wordPrimTy] naturalTyCon

naturalNBDataCon :: DataCon
naturalNBDataCon = pcDataCon naturalNBDataConName [] [byteArrayPrimTy] naturalTyCon


-- | Replaces constraint tuple names with corresponding boxed ones.
filterCTuple :: RdrName -> RdrName
filterCTuple (Exact n)
  | Just arity <- cTupleTyConNameArity_maybe n
  = Exact $ tupleTyConName BoxedTuple arity
filterCTuple rdr = rdr

{-
************************************************************************
*                                                                      *
   Semi-builtin names
*                                                                      *
************************************************************************

Note [pretendNameIsInScope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, we filter out instances that mention types whose names are
not in scope. However, in the situations listed below, we make an exception
for some commonly used names, such as Data.Kind.Type, which may not actually
be in scope but should be treated as though they were in scope.
This includes built-in names, as well as a few extra names such as
'Type', 'TYPE', 'BoxedRep', etc.

Situations in which we apply this special logic:

  - GHCi's :info command, see GHC.Runtime.Eval.getInfo.
    This fixes #1581.

  - When reporting instance overlap errors. Not doing so could mean
    that we would omit instances for typeclasses like

      type Cls :: k -> Constraint
      class Cls a

    because BoxedRep/Lifted were not in scope.
    See GHC.Tc.Errors.potentialInstancesErrMsg.
    This fixes one of the issues reported in #20465.
-}

-- | Should this name be considered in-scope, even though it technically isn't?
--
-- This ensures that we don't filter out information because, e.g.,
-- Data.Kind.Type isn't imported.
--
-- See Note [pretendNameIsInScope].
pretendNameIsInScope :: Name -> Bool
pretendNameIsInScope n
  = isBuiltInSyntax n
  || isTupleTyConName n
  || isSumTyConName n
  || isCTupleTyConName n
  || any (n `hasKey`)
    [ liftedTypeKindTyConKey, unliftedTypeKindTyConKey
    , liftedDataConKey, unliftedDataConKey
    , tYPETyConKey
    , cONSTRAINTTyConKey
    , runtimeRepTyConKey, boxedRepDataConKey
    , eqTyConKey
    , listTyConKey
    , oneDataConKey
    , manyDataConKey
    , fUNTyConKey, unrestrictedFunTyConKey ]
