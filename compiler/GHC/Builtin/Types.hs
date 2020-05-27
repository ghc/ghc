{-
(c) The GRASP Project, Glasgow University, 1994-1998

Wired-in knowledge about {\em non-primitive} types
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | This module is about types that can be defined in Haskell, but which
--   must be wired into the compiler nonetheless.  C.f module "GHC.Builtin.Types.Prim"
module GHC.Builtin.Types (
        -- * Helper functions defined here
        mkWiredInTyConName, -- This is used in GHC.Builtin.Types.Literals to define the
                            -- built-in functions for evaluation.

        mkWiredInIdName,    -- used in GHC.Types.Id.Make

        -- * All wired in things
        wiredInTyCons, isBuiltInOcc_maybe,

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
        boxingDataCon_maybe,

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
        mkListTy, mkPromotedListTy,

        -- * Maybe
        maybeTyCon, maybeTyConName,
        nothingDataCon, nothingDataConName, promotedNothingDataCon,
        justDataCon, justDataConName, promotedJustDataCon,
        maybeType, maybeKind, isMaybeType,

        -- * Tuples
        mkTupleTy, mkTupleTy1, mkBoxedTupleTy, mkTupleStr,
        tupleTyCon, tupleDataCon, tupleTyConName, tupleDataConName,
        promotedTupleDataCon,
        unitTyCon, unitDataCon, unitDataConId, unitTy, unitTyConKey,
        pairTyCon, pair, isTupleType,
        unboxedUnitTy,
        unboxedUnitTyCon, unboxedUnitDataCon,
        unboxedTupleKind, unboxedSumKind,
        filterCTuple,

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
        isLiftedTypeKindTyConName, liftedTypeKind,
        typeToTypeKind, constraintKind,
        liftedTypeKindTyCon, constraintKindTyCon,  constraintKindTyConName,
        liftedTypeKindTyConName,

        -- * Equality predicates
        heqTyCon, heqTyConName, heqClass, heqDataCon,
        eqTyCon, eqTyConName, eqClass, eqDataCon, eqTyCon_RDR,
        coercibleTyCon, coercibleTyConName, coercibleDataCon, coercibleClass,

        -- * RuntimeRep and friends
        runtimeRepTyCon, vecCountTyCon, vecElemTyCon,

        runtimeRepTy, liftedRepTy, liftedRepDataCon, liftedRepDataConTyCon,

        vecRepDataConTyCon, tupleRepDataConTyCon, sumRepDataConTyCon,

        liftedRepDataConTy, unliftedRepDataConTy,
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
        naturalNBDataCon, naturalNBDataConName

    ) where

#include "HsVersions.h"

import GHC.Prelude

import {-# SOURCE #-} GHC.Types.Id.Make ( mkDataConWorkId, mkDictSelId )

-- friends:
import GHC.Builtin.Names
import GHC.Builtin.Types.Prim
import GHC.Builtin.Uniques

-- others:
import GHC.Core.Coercion.Axiom
import GHC.Types.Id
import GHC.Types.TyThing
import GHC.Types.SourceText
import GHC.Types.Var (VarBndr (Bndr))
import GHC.Settings.Constants ( mAX_TUPLE_SIZE, mAX_CTUPLE_SIZE, mAX_SUM_SIZE )
import GHC.Unit.Module        ( Module )
import GHC.Core.Type
import qualified GHC.Core.TyCo.Rep as TyCoRep (Type(TyConApp))
import GHC.Types.RepType
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Core.TyCon
import GHC.Core.Class     ( Class, mkClass )
import GHC.Types.Name.Reader
import GHC.Types.Name as Name
import GHC.Types.Name.Env ( NameEnv, mkNameEnv, lookupNameEnv, lookupNameEnv_NF )
import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Types.Unique.Set
import Data.Array
import GHC.Data.FastString
import GHC.Data.BooleanFormula ( mkAnd )

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import qualified Data.ByteString.Char8 as BS

import Data.List        ( elemIndex )

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

See also Note [Getting from RuntimeRep to PrimRep] in GHC.Types.RepType.

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

wiredInTyCons = [ -- Units are not treated like other tuples, because they
                  -- are defined in GHC.Base, and there's only a few of them. We
                  -- put them in wiredInTyCons so that they will pre-populate
                  -- the name cache, so the parser in isBuiltInOcc_maybe doesn't
                  -- need to look out for them.
                  unitTyCon
                , unboxedUnitTyCon
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
                , vecCountTyCon
                , vecElemTyCon
                , constraintKindTyCon
                , liftedTypeKindTyCon
                , multiplicityTyCon
                , naturalTyCon
                , integerTyCon
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

{- Note [eqTyCon (~) is built-in syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The (~) type operator used in equality constraints (a~b) is considered built-in
syntax. This has a few consequences:

* The user is not allowed to define their own type constructors with this name:

    ghci> class a ~ b
    <interactive>:1:1: error: Illegal binding of built-in syntax: ~

* Writing (a ~ b) does not require enabling -XTypeOperators. It does, however,
  require -XGADTs or -XTypeFamilies.

* The (~) type operator is always in scope. It doesn't need to be imported,
  and it cannot be hidden.

* We have a bunch of special cases in the compiler to arrange all of the above.

There's no particular reason for (~) to be special, but fixing this would be a
breaking change.
-}
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
stringTyConName   = mkWiredInTyConName   UserSyntax gHC_BASE  (fsLit "String") stringTyConKey stringTyCon
intTyConName      = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Int")    intTyConKey   intTyCon
intDataConName    = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "I#")     intDataConKey  intDataCon

boolTyConName, falseDataConName, trueDataConName :: Name
boolTyConName     = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Bool") boolTyConKey boolTyCon
falseDataConName  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "False") falseDataConKey falseDataCon
trueDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "True")  trueDataConKey  trueDataCon

listTyConName, nilDataConName, consDataConName :: Name
listTyConName     = mkWiredInTyConName   BuiltInSyntax gHC_TYPES (fsLit "[]") listTyConKey listTyCon
nilDataConName    = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit "[]") nilDataConKey nilDataCon
consDataConName   = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit ":") consDataConKey consDataCon

maybeTyConName, nothingDataConName, justDataConName :: Name
maybeTyConName     = mkWiredInTyConName   UserSyntax gHC_MAYBE (fsLit "Maybe")
                                          maybeTyConKey maybeTyCon
nothingDataConName = mkWiredInDataConName UserSyntax gHC_MAYBE (fsLit "Nothing")
                                          nothingDataConKey nothingDataCon
justDataConName    = mkWiredInDataConName UserSyntax gHC_MAYBE (fsLit "Just")
                                          justDataConKey justDataCon

wordTyConName, wordDataConName, word8DataConName :: Name
wordTyConName      = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Word")   wordTyConKey     wordTyCon
wordDataConName    = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "W#")     wordDataConKey   wordDataCon
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
    if we have (Any @(TYPE rr)) then rr must be 'LiftedRep.  See
    Note [TYPE and RuntimeRep] in GHC.Builtin.Types.Prim.  This is a convenient
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

constraintKindTyConName :: Name
constraintKindTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Constraint") constraintKindTyConKey   constraintKindTyCon

liftedTypeKindTyConName :: Name
liftedTypeKindTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Type") liftedTypeKindTyConKey liftedTypeKindTyCon

multiplicityTyConName :: Name
multiplicityTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Multiplicity")
                          multiplicityTyConKey multiplicityTyCon

oneDataConName, manyDataConName :: Name
oneDataConName = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit "One") oneDataConKey oneDataCon
manyDataConName = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit "Many") manyDataConKey manyDataCon
 -- It feels wrong to have One and Many be BuiltInSyntax. But otherwise,
 -- `Many`, in particular, is considered out of scope unless an appropriate
 -- file is open. The problem with this is that `Many` appears implicitly in
 -- types every time there is an `(->)`, hence out-of-scope errors get
 -- reported. Making them built-in make it so that they are always considered in
 -- scope.

runtimeRepTyConName, vecRepDataConName, tupleRepDataConName, sumRepDataConName :: Name
runtimeRepTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "RuntimeRep") runtimeRepTyConKey runtimeRepTyCon
vecRepDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "VecRep") vecRepDataConKey vecRepDataCon
tupleRepDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "TupleRep") tupleRepDataConKey tupleRepDataCon
sumRepDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "SumRep") sumRepDataConKey sumRepDataCon

-- See Note [Wiring in RuntimeRep]
runtimeRepSimpleDataConNames :: [Name]
runtimeRepSimpleDataConNames
  = zipWith3Lazy mk_special_dc_name
      [ fsLit "LiftedRep", fsLit "UnliftedRep"
      , fsLit "IntRep"
      , fsLit "Int8Rep", fsLit "Int16Rep", fsLit "Int32Rep", fsLit "Int64Rep"
      , fsLit "WordRep"
      , fsLit "Word8Rep", fsLit "Word16Rep", fsLit "Word32Rep", fsLit "Word64Rep"
      , fsLit "AddrRep"
      , fsLit "FloatRep", fsLit "DoubleRep"
      ]
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
                (mkAnonTyConBinders VisArg tyvars)
                liftedTypeKind
                (map (const Representational) tyvars)
                cType
                []              -- No stupid theta
                (mkDataTyConRhs cons)
                (VanillaAlgTyCon (mkPrelTyConRepName name))
                False           -- Not in GADT syntax

pcDataCon :: Name -> [TyVar] -> [Type] -> TyCon -> DataCon
pcDataCon n univs tys = pcDataConW n univs (map linear tys)

pcDataConW :: Name -> [TyVar] -> [Scaled Type] -> TyCon -> DataCon
pcDataConW n univs tys = pcDataConWithFixity False n univs
                      []    -- no ex_tvs
                      univs -- the univs are precisely the user-written tyvars
                      tys

pcDataConWithFixity :: Bool      -- ^ declared infix?
                    -> Name      -- ^ datacon name
                    -> [TyVar]   -- ^ univ tyvars
                    -> [TyCoVar] -- ^ ex tycovars
                    -> [TyCoVar] -- ^ user-written tycovars
                    -> [Scaled Type]    -- ^ args
                    -> TyCon
                    -> DataCon
pcDataConWithFixity infx n = pcDataConWithFixity' infx n (dataConWorkerUnique (nameUnique n))
                                                  NoRRI
-- The Name's unique is the first of two free uniques;
-- the first is used for the datacon itself,
-- the second is used for the "worker name"
--
-- To support this the mkPreludeDataConUnique function "allocates"
-- one DataCon unique per pair of Ints.

pcDataConWithFixity' :: Bool -> Name -> Unique -> RuntimeRepInfo
                     -> [TyVar] -> [TyCoVar] -> [TyCoVar]
                     -> [Scaled Type] -> TyCon -> DataCon
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
                     tyvars ex_tyvars user_tyvars arg_tys tycon
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
                (mkTyVarBinders SpecifiedSpec user_tyvars)
                []      -- No equality spec
                []      -- No theta
                arg_tys (mkTyConApp tycon (mkTyVarTys tyvars))
                rri
                tycon
                (lookupNameEnv_NF tag_map dc_name)
                []      -- No stupid theta
                (mkDataConWorkId wrk_name data_con)
                NoDataConRep    -- Wired-in types are too simple to need wrappers

    no_bang = HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict

    wrk_name = mkDataConWorkerName data_con wrk_key

    prom_info = mkPrelTyConRepName dc_name

mkDataConWorkerName :: DataCon -> Unique -> Name
mkDataConWorkerName data_con wrk_key =
    mkWiredInName modu wrk_occ wrk_key
                  (AnId (dataConWorkId data_con)) UserSyntax
  where
    modu     = ASSERT( isExternalName dc_name )
               nameModule dc_name
    dc_name = dataConName data_con
    dc_occ  = nameOccName dc_name
    wrk_occ = mkDataConWorkerOcc dc_occ

-- used for RuntimeRep and friends
pcSpecialDataCon :: Name -> [Type] -> TyCon -> RuntimeRepInfo -> DataCon
pcSpecialDataCon dc_name arg_tys tycon rri
  = pcDataConWithFixity' False dc_name (dataConWorkerUnique (nameUnique dc_name)) rri
                         [] [] [] (map linear arg_tys) tycon

{-
************************************************************************
*                                                                      *
      Kinds
*                                                                      *
************************************************************************
-}

typeSymbolKindCon :: TyCon
-- data Symbol
typeSymbolKindCon = pcTyCon typeSymbolKindConName Nothing [] []

typeSymbolKind :: Kind
typeSymbolKind = mkTyConTy typeSymbolKindCon

constraintKindTyCon :: TyCon
-- 'TyCon.isConstraintKindCon' assumes that this is an AlgTyCon!
constraintKindTyCon = pcTyCon constraintKindTyConName Nothing [] []

-- See Note [Prefer Type over TYPE 'LiftedRep] in GHC.Core.TyCo.Rep.
liftedTypeKind, typeToTypeKind, constraintKind :: Kind
liftedTypeKind   = TyCoRep.TyConApp liftedTypeKindTyCon []
typeToTypeKind   = liftedTypeKind `mkVisFunTyMany` liftedTypeKind
constraintKind   = mkTyConApp constraintKindTyCon []

{-
************************************************************************
*                                                                      *
                Stuff for dealing with tuples
*                                                                      *
************************************************************************

Note [How tuples work]  See also Note [Known-key names] in GHC.Builtin.Names
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
    - A wired-in type.
    - Declared as classes in GHC.Classes, e.g.
         class (c1,c2) => (c1,c2)
    - Given constraints: the superclasses automatically become available
    - Wanted constraints: there is a built-in instance
         instance (c1,c2) => (c1,c2)
      See GHC.Tc.Instance.Class.matchCTuple
    - Currently just go up to 64; beyond that
      you have to use manual nesting
    - Their OccNames look like (%,,,%), so they can easily be
      distinguished from term tuples.  But (following Haskell) we
      pretty-print saturated constraint tuples with round parens;
      see BasicTypes.tupleParens.
    - Unlike BoxedTuples and UnboxedTuples, which only wire
      in type constructors and data constructors, ConstraintTuples also wire in
      superclass selector functions. For instance, $p1(%,%) and $p2(%,%) are
      the selectors for the binary constraint tuple.

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
  3-tuples:  (,,)   (,,)#
  2-tuples:  (,)    (,)#
  1-tuples:  ??
  0-tuples:  ()     ()#

Zero-tuples have used up the logical name. So we use 'Solo' and 'Solo#'
for one-tuples.  So in ghc-prim:GHC.Tuple we see the declarations:
  data ()     = ()
  data Solo a = Solo a
  data (a,b)  = (a,b)

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

We make boxed one-tuple names have known keys so that `data Solo a = Solo a`,
defined in GHC.Tuple, will be used when one-tuples are spliced in through
Template Haskell. This program (from #18097) crucially relies on this:

  case $( tupE [ [| "ok" |] ] ) of Solo x -> putStrLn x

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
-- Note [Symbol table representation of names].
--
-- Moreover, there is no need to include names of things that the user can't
-- write (e.g. type representation bindings like $tc(,,,)).
isBuiltInOcc_maybe :: OccName -> Maybe Name
isBuiltInOcc_maybe occ =
    case name of
      "[]" -> Just $ choose_ns listTyConName nilDataConName
      ":"    -> Just consDataConName

      -- equality tycon
      "~"    -> Just eqTyConName

      -- function tycon
      "FUN"  -> Just funTyConName
      "->"  -> Just unrestrictedFunTyConName

      -- boxed tuple data/tycon
      -- We deliberately exclude Solo (the boxed 1-tuple).
      -- See Note [One-tuples] (Wrinkle: Make boxed one-tuple names have known keys)
      "()"    -> Just $ tup_name Boxed 0
      _ | Just rest <- "(" `BS.stripPrefix` name
        , (commas, rest') <- BS.span (==',') rest
        , ")" <- rest'
             -> Just $ tup_name Boxed (1+BS.length commas)

      -- unboxed tuple data/tycon
      "(##)"  -> Just $ tup_name Unboxed 0
      "Solo#" -> Just $ tup_name Unboxed 1
      _ | Just rest <- "(#" `BS.stripPrefix` name
        , (commas, rest') <- BS.span (==',') rest
        , "#)" <- rest'
             -> Just $ tup_name Unboxed (1+BS.length commas)

      -- unboxed sum tycon
      _ | Just rest <- "(#" `BS.stripPrefix` name
        , (pipes, rest') <- BS.span (=='|') rest
        , "#)" <- rest'
             -> Just $ tyConName $ sumTyCon (1+BS.length pipes)

      -- unboxed sum datacon
      _ | Just rest <- "(#" `BS.stripPrefix` name
        , (pipes1, rest') <- BS.span (=='|') rest
        , Just rest'' <- "_" `BS.stripPrefix` rest'
        , (pipes2, rest''') <- BS.span (=='|') rest''
        , "#)" <- rest'''
             -> let arity = BS.length pipes1 + BS.length pipes2 + 1
                    alt = BS.length pipes1 + 1
                in Just $ dataConName $ sumDataCon alt arity
      _ -> Nothing
  where
    name = bytesFS $ occNameFS occ

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

mkTupleStr :: Boxity -> Arity -> String
mkTupleStr Boxed   = mkBoxedTupleStr
mkTupleStr Unboxed = mkUnboxedTupleStr

mkBoxedTupleStr :: Arity -> String
mkBoxedTupleStr 0  = "()"
mkBoxedTupleStr 1  = "Solo"   -- See Note [One-tuples]
mkBoxedTupleStr ar = '(' : commas ar ++ ")"

mkUnboxedTupleStr :: Arity -> String
mkUnboxedTupleStr 0  = "(##)"
mkUnboxedTupleStr 1  = "Solo#"  -- See Note [One-tuples]
mkUnboxedTupleStr ar = "(#" ++ commas ar ++ "#)"

mkConstraintTupleStr :: Arity -> String
mkConstraintTupleStr 0  = "(%%)"
mkConstraintTupleStr 1  = "Solo%"   -- See Note [One-tuples]
mkConstraintTupleStr ar = "(%" ++ commas ar ++ "%)"

commas :: Arity -> String
commas ar = take (ar-1) (repeat ',')

cTupleTyCon :: Arity -> TyCon
cTupleTyCon i
  | i > mAX_CTUPLE_SIZE = fstOf3 (mk_ctuple i) -- Build one specially
  | otherwise           = fstOf3 (cTupleArr ! i)

cTupleTyConName :: Arity -> Name
cTupleTyConName a = tyConName (cTupleTyCon a)

cTupleTyConNames :: [Name]
cTupleTyConNames = map cTupleTyConName (0 : [2..mAX_CTUPLE_SIZE])

cTupleTyConKeys :: UniqSet Unique
cTupleTyConKeys = mkUniqSet $ map getUnique cTupleTyConNames

isCTupleTyConName :: Name -> Bool
isCTupleTyConName n
 = ASSERT2( isExternalName n, ppr n )
   getUnique n `elementOfUniqSet` cTupleTyConKeys

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

  | arity < 2
  = panic ("cTupleSelId: Arity starts from 2. "
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

pair :: Kind -> Kind -> Type -> Type -> Type
pair u x y z = mkTyConApp (promotedTupleDataCon Boxed 2) [u,x,y,z]

isTupleType :: Type -> Maybe (Type, Type)
isTupleType tc
  | Just (tc1, [x,y]) <- splitTyConApp_maybe tc, tc1 == (promotedTupleDataCon Boxed 2) = Just (x, y)
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
  = tYPE (mkTyConApp tc [mkPromotedListTy runtimeRepTy rr_tys])

-- | Specialization of 'unboxedTupleSumKind' for tuples
unboxedTupleKind :: [Type] -> Kind
unboxedTupleKind = unboxedTupleSumKind tupleRepDataConTyCon

mk_tuple :: Boxity -> Int -> (TyCon,DataCon)
mk_tuple Boxed arity = (tycon, tuple_con)
  where
    tycon = mkTupleTyCon tc_name tc_binders tc_res_kind tc_arity tuple_con
                         BoxedTuple flavour

    tc_binders  = mkTemplateAnonTyConBinders (replicate arity liftedTypeKind)
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

    -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
    -- Kind:  forall (k1:RuntimeRep) (k2:RuntimeRep). TYPE k1 -> TYPE k2 -> #
    tc_binders = mkTemplateTyConBinders (replicate arity runtimeRepTy)
                                        (\ks -> map tYPE ks)

    tc_res_kind = unboxedTupleKind rr_tys

    tc_arity    = arity * 2
    flavour     = UnboxedAlgTyCon $ Just (mkPrelTyConRepName tc_name)

    dc_tvs               = binderVars tc_binders
    (rr_tys, dc_arg_tys) = splitAt arity (mkTyVarTys dc_tvs)
    tuple_con            = pcDataCon dc_name dc_tvs dc_arg_tys tycon

    boxity  = Unboxed
    modu    = gHC_PRIM
    tc_name = mkWiredInName modu (mkTupleOcc tcName boxity arity) tc_uniq
                         (ATyCon tycon) BuiltInSyntax
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
    tuple_con = pcDataConW dc_name tvs (map unrestricted sc_theta) tycon

    binders = mkTemplateAnonTyConBinders (replicate arity constraintKind)
    roles   = replicate arity Nominal
    rhs     = TupleTyCon{data_con = tuple_con, tup_sort = ConstraintTuple}

    modu    = gHC_CLASSES
    tc_name = mkWiredInName modu (mkCTupleOcc tcName arity) tc_uniq
                         (ATyCon tycon) BuiltInSyntax
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

unitTyConKey :: Unique
unitTyConKey = getUnique unitTyCon

unitDataCon :: DataCon
unitDataCon   = head (tyConDataCons unitTyCon)

unitDataConId :: Id
unitDataConId = dataConWorkId unitDataCon

pairTyCon :: TyCon
pairTyCon = tupleTyCon Boxed 2

unboxedUnitTy :: Type
unboxedUnitTy = mkTyConApp unboxedUnitTyCon []

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

-- | Specialization of 'unboxedTupleSumKind' for sums
unboxedSumKind :: [Type] -> Kind
unboxedSumKind = unboxedTupleSumKind sumRepDataConTyCon

-- | Create type constructor and data constructors for n-ary unboxed sum.
mk_sum :: Arity -> (TyCon, Array ConTagZ DataCon)
mk_sum arity = (tycon, sum_cons)
  where
    tycon   = mkSumTyCon tc_name tc_binders tc_res_kind (arity * 2) tyvars (elems sum_cons)
                         (UnboxedAlgTyCon rep_name)

    -- Unboxed sums are currently not Typeable due to efficiency concerns. See #13276.
    rep_name = Nothing -- Just $ mkPrelTyConRepName tc_name

    tc_binders = mkTemplateTyConBinders (replicate arity runtimeRepTy)
                                        (\ks -> map tYPE ks)

    tyvars = binderVars tc_binders

    tc_res_kind = unboxedSumKind rr_tys

    (rr_tys, tyvar_tys) = splitAt arity (mkTyVarTys tyvars)

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
    datacon   = pcDataConW eqDataConName tvs [unrestricted sc_pred] tycon

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
    datacon   = pcDataConW heqDataConName tvs [unrestricted sc_pred] tycon

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
    datacon   = pcDataConW coercibleDataConName tvs [unrestricted sc_pred] tycon

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

{- Multiplicity polymorphism is implemented very similarly to levity
 polymorphism. We write in the multiplicity kind and the One and Many
 types which can appear in user programs. These are defined properly in GHC.Types.

data Multiplicity = One | Many
-}

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

unrestrictedFunTy :: Type
unrestrictedFunTy = functionWithMultiplicity manyDataConTy

unrestrictedFunTyCon :: TyCon
unrestrictedFunTyCon = buildSynTyCon unrestrictedFunTyConName [] arrowKind [] unrestrictedFunTy
  where arrowKind = mkTyConKind binders liftedTypeKind
        -- See also funTyCon
        binders = [ Bndr runtimeRep1TyVar (NamedTCB Inferred)
                  , Bndr runtimeRep2TyVar (NamedTCB Inferred)
                  ]
                  ++ mkTemplateAnonTyConBinders [ tYPE runtimeRep1Ty
                                                , tYPE runtimeRep2Ty
                                                ]

unrestrictedFunTyConName :: Name
unrestrictedFunTyConName = mkWiredInTyConName BuiltInSyntax gHC_TYPES (fsLit "->") unrestrictedFunTyConKey unrestrictedFunTyCon

{- *********************************************************************
*                                                                      *
                Kinds and RuntimeRep
*                                                                      *
********************************************************************* -}

-- For information about the usage of the following type,
-- see Note [TYPE and RuntimeRep] in module GHC.Builtin.Types.Prim
runtimeRepTy :: Type
runtimeRepTy = mkTyConTy runtimeRepTyCon

-- Type synonyms; see Note [TYPE and RuntimeRep] in GHC.Builtin.Types.Prim
-- and Note [Prefer Type over TYPE 'LiftedRep] in GHC.Core.TyCo.Rep.
-- type Type = tYPE 'LiftedRep
liftedTypeKindTyCon :: TyCon
liftedTypeKindTyCon   = buildSynTyCon liftedTypeKindTyConName
                                       [] liftedTypeKind [] rhs
  where rhs = TyCoRep.TyConApp tYPETyCon [liftedRepTy]

runtimeRepTyCon :: TyCon
runtimeRepTyCon = pcTyCon runtimeRepTyConName Nothing []
                          (vecRepDataCon : tupleRepDataCon :
                           sumRepDataCon : runtimeRepSimpleDataCons)

vecRepDataCon :: DataCon
vecRepDataCon = pcSpecialDataCon vecRepDataConName [ mkTyConTy vecCountTyCon
                                                   , mkTyConTy vecElemTyCon ]
                                 runtimeRepTyCon
                                 (RuntimeRep prim_rep_fun)
  where
    -- See Note [Getting from RuntimeRep to PrimRep] in GHC.Types.RepType
    prim_rep_fun [count, elem]
      | VecCount n <- tyConRuntimeRepInfo (tyConAppTyCon count)
      , VecElem  e <- tyConRuntimeRepInfo (tyConAppTyCon elem)
      = [VecRep n e]
    prim_rep_fun args
      = pprPanic "vecRepDataCon" (ppr args)

vecRepDataConTyCon :: TyCon
vecRepDataConTyCon = promoteDataCon vecRepDataCon

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
      = map slotPrimRep (ubxSumRepType prim_repss)
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
liftedRepDataCon :: DataCon
runtimeRepSimpleDataCons@(liftedRepDataCon : _)
  = zipWithLazy mk_runtime_rep_dc
    [ LiftedRep, UnliftedRep
    , IntRep
    , Int8Rep, Int16Rep, Int32Rep, Int64Rep
    , WordRep
    , Word8Rep, Word16Rep, Word32Rep, Word64Rep
    , AddrRep
    , FloatRep, DoubleRep
    ]
    runtimeRepSimpleDataConNames
  where
    mk_runtime_rep_dc primrep name
      = pcSpecialDataCon name [] runtimeRepTyCon (RuntimeRep (\_ -> [primrep]))

-- See Note [Wiring in RuntimeRep]
liftedRepDataConTy, unliftedRepDataConTy,
  intRepDataConTy,
  int8RepDataConTy, int16RepDataConTy, int32RepDataConTy, int64RepDataConTy,
  wordRepDataConTy,
  word8RepDataConTy, word16RepDataConTy, word32RepDataConTy, word64RepDataConTy,
  addrRepDataConTy,
  floatRepDataConTy, doubleRepDataConTy :: Type
[liftedRepDataConTy, unliftedRepDataConTy,
   intRepDataConTy,
   int8RepDataConTy, int16RepDataConTy, int32RepDataConTy, int64RepDataConTy,
   wordRepDataConTy,
   word8RepDataConTy, word16RepDataConTy, word32RepDataConTy, word64RepDataConTy,
   addrRepDataConTy,
   floatRepDataConTy, doubleRepDataConTy
   ]
  = map (mkTyConTy . promoteDataCon) runtimeRepSimpleDataCons

vecCountTyCon :: TyCon
vecCountTyCon = pcTyCon vecCountTyConName Nothing [] vecCountDataCons

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
vecElemTyCon = pcTyCon vecElemTyConName Nothing [] vecElemDataCons

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

liftedRepDataConTyCon :: TyCon
liftedRepDataConTyCon = promoteDataCon liftedRepDataCon

-- The type ('LiftedRep)
liftedRepTy :: Type
liftedRepTy = liftedRepDataConTy

{- *********************************************************************
*                                                                      *
     The boxed primitive types: Char, Int, etc
*                                                                      *
********************************************************************* -}

boxingDataCon_maybe :: TyCon -> Maybe DataCon
--    boxingDataCon_maybe Char# = C#
--    boxingDataCon_maybe Int#  = I#
--    ... etc ...
-- See Note [Boxing primitive types]
boxingDataCon_maybe tc
  = lookupNameEnv boxing_constr_env (tyConName tc)

boxing_constr_env :: NameEnv DataCon
boxing_constr_env
  = mkNameEnv [(charPrimTyConName  , charDataCon  )
              ,(intPrimTyConName   , intDataCon   )
              ,(wordPrimTyConName  , wordDataCon  )
              ,(floatPrimTyConName , floatDataCon )
              ,(doublePrimTyConName, doubleDataCon) ]

{- Note [Boxing primitive types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a handful of primitive types (Int, Char, Word, Float, Double),
we can readily box and an unboxed version (Int#, Char# etc) using
the corresponding data constructor.  This is useful in a couple
of places, notably let-floating -}


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
stringTy = mkTyConApp stringTyCon []

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
               alpha_tyvar [] alpha_tyvar
               (map linear [alphaTy, mkTyConApp listTyCon alpha_ty]) listTyCon
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

maybeType :: Kind -> Maybe Type -> Type
maybeType k (Just x) = mkTyConApp promotedJustDataCon [k,x]
maybeType k (Nothing) = mkTyConApp promotedNothingDataCon [k]

maybeKind :: Type -> Kind
maybeKind t = mkTyConApp maybeTyCon [t]

isMaybeType :: Type -> Maybe (Maybe Type)
isMaybeType tc
  | Just (tc1,[t]) <- splitTyConApp_maybe tc, tc1 == promotedJustDataCon = return $ Just t
  | Just (tc1,[]) <- splitTyConApp_maybe tc, tc1 == promotedNothingDataCon = return $ Nothing
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
      = ASSERT( tc `hasKey` consDataConKey )
        t : go ts

      | Just (tc, [_k]) <- splitTyConApp_maybe list_ty
      = ASSERT( tc `hasKey` nilDataConKey )
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
      gHC_NUM_INTEGER
      (fsLit "Integer")
      integerTyConKey
      integerTyCon
integerISDataConName
   = mkWiredInDataConName
      UserSyntax
      gHC_NUM_INTEGER
      (fsLit "IS")
      integerISDataConKey
      integerISDataCon
integerIPDataConName
   = mkWiredInDataConName
      UserSyntax
      gHC_NUM_INTEGER
      (fsLit "IP")
      integerIPDataConKey
      integerIPDataCon
integerINDataConName
   = mkWiredInDataConName
      UserSyntax
      gHC_NUM_INTEGER
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
      gHC_NUM_NATURAL
      (fsLit "Natural")
      naturalTyConKey
      naturalTyCon
naturalNSDataConName
   = mkWiredInDataConName
      UserSyntax
      gHC_NUM_NATURAL
      (fsLit "NS")
      naturalNSDataConKey
      naturalNSDataCon
naturalNBDataConName
   = mkWiredInDataConName
      UserSyntax
      gHC_NUM_NATURAL
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
