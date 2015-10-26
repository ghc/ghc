{-
(c) The GRASP Project, Glasgow University, 1994-1998

\section[TysWiredIn]{Wired-in knowledge about {\em non-primitive} types}
-}

{-# LANGUAGE CPP #-}

-- | This module is about types that can be defined in Haskell, but which
--   must be wired into the compiler nonetheless.  C.f module TysPrim
module TysWiredIn (
        -- * All wired in things
        wiredInTyCons, isBuiltInOcc_maybe,

        -- * Bool
        boolTy, boolTyCon, boolTyCon_RDR, boolTyConName,
        trueDataCon,  trueDataConId,  true_RDR,
        falseDataCon, falseDataConId, false_RDR,
        promotedBoolTyCon, promotedFalseDataCon, promotedTrueDataCon,

        -- * Ordering
        ltDataCon, ltDataConId,
        eqDataCon, eqDataConId,
        gtDataCon, gtDataConId,
        promotedOrderingTyCon,
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

        mkListTy, mkPromotedListTy,

        -- * Maybe
        maybeTyCon, maybeTyConName,
        nothingDataCon, nothingDataConName, justDataCon, justDataConName,

        -- * Tuples
        mkTupleTy, mkBoxedTupleTy,
        tupleTyCon, tupleDataCon, tupleTyConName,
        promotedTupleTyCon, promotedTupleDataCon,
        unitTyCon, unitDataCon, unitDataConId, unitTy, unitTyConKey,
        pairTyCon,
        unboxedUnitTyCon, unboxedUnitDataCon,
        unboxedSingletonTyCon, unboxedSingletonDataCon,
        unboxedPairTyCon, unboxedPairDataCon,
        cTupleTyConName, cTupleTyConNames, isCTupleTyConName,

        -- * Kinds
        typeNatKindCon, typeNatKind, typeSymbolKindCon, typeSymbolKind,

        -- * Parallel arrays
        mkPArrTy,
        parrTyCon, parrFakeCon, isPArrTyCon, isPArrFakeCon,
        parrTyCon_RDR, parrTyConName,

        -- * Equality predicates
        eqTyCon_RDR, eqTyCon, eqTyConName, eqBoxDataCon,
        coercibleTyCon, coercibleDataCon, coercibleClass,

        -- * Implicit Parameters
        ipTyCon, ipDataCon, ipClass,

        callStackTyCon,

        mkWiredInTyConName -- This is used in TcTypeNats to define the
                           -- built-in functions for evaluation.
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} MkId( mkDataConWorkId )

-- friends:
import PrelNames
import TysPrim

-- others:
import CoAxiom
import Coercion
import Constants        ( mAX_TUPLE_SIZE, mAX_CTUPLE_SIZE )
import Module           ( Module )
import Type             ( mkTyConApp )
import DataCon
import ConLike
import Var
import TyCon
import Class            ( Class, mkClass )
import TypeRep
import RdrName
import Name
import NameSet          ( NameSet, mkNameSet, elemNameSet )
import BasicTypes       ( Arity, RecFlag(..), Boxity(..),
                           TupleSort(..) )
import ForeignCall
import SrcLoc           ( noSrcSpan )
import Unique
import Data.Array
import FastString
import Outputable
import Util
import BooleanFormula   ( mkAnd )

alpha_tyvar :: [TyVar]
alpha_tyvar = [alphaTyVar]

alpha_ty :: [Type]
alpha_ty = [alphaTy]

{-
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
-- parameter TyCons. Instead, we have a hack in lookupOrigNameCache to deal with
-- these names.
--
-- See also Note [Known-key names]
wiredInTyCons :: [TyCon]

wiredInTyCons = [ unitTyCon     -- Not treated like other tuples, because
                                -- it's defined in GHC.Base, and there's only
                                -- one of it.  We put it in wiredInTyCons so
                                -- that it'll pre-populate the name cache, so
                                -- the special case in lookupOrigNameCache
                                -- doesn't need to look out for it
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
              , eqTyCon
              , coercibleTyCon
              , typeNatKindCon
              , typeSymbolKindCon
              , ipTyCon
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

mkWiredInCoAxiomName :: BuiltInSyntax -> Module -> FastString -> Unique
                     -> CoAxiom Branched -> Name
mkWiredInCoAxiomName built_in modu fs unique ax
  = mkWiredInName modu (mkTcOccFS fs) unique
                  (ACoAxiom ax)        -- Relevant CoAxiom
                  built_in

-- See Note [Kind-changing of (~) and Coercible]
eqTyConName, eqBoxDataConName :: Name
eqTyConName      = mkWiredInTyConName   BuiltInSyntax gHC_TYPES (fsLit "~")   eqTyConKey      eqTyCon
eqBoxDataConName = mkWiredInDataConName UserSyntax    gHC_TYPES (fsLit "Eq#") eqBoxDataConKey eqBoxDataCon

-- See Note [Kind-changing of (~) and Coercible]
coercibleTyConName, coercibleDataConName :: Name
coercibleTyConName   = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Coercible")  coercibleTyConKey   coercibleTyCon
coercibleDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "MkCoercible") coercibleDataConKey coercibleDataCon

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

-- Kinds
typeNatKindConName, typeSymbolKindConName :: Name
typeNatKindConName    = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Nat")    typeNatKindConNameKey    typeNatKindCon
typeSymbolKindConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Symbol") typeSymbolKindConNameKey typeSymbolKindCon

parrTyConName, parrDataConName :: Name
parrTyConName   = mkWiredInTyConName   BuiltInSyntax
                    gHC_PARR' (fsLit "[::]") parrTyConKey parrTyCon
parrDataConName = mkWiredInDataConName UserSyntax
                    gHC_PARR' (fsLit "PArr") parrDataConKey parrDataCon

boolTyCon_RDR, false_RDR, true_RDR, intTyCon_RDR, charTyCon_RDR,
    intDataCon_RDR, listTyCon_RDR, consDataCon_RDR, parrTyCon_RDR, eqTyCon_RDR :: RdrName
boolTyCon_RDR   = nameRdrName boolTyConName
false_RDR       = nameRdrName falseDataConName
true_RDR        = nameRdrName trueDataConName
intTyCon_RDR    = nameRdrName intTyConName
charTyCon_RDR   = nameRdrName charTyConName
intDataCon_RDR  = nameRdrName intDataConName
listTyCon_RDR   = nameRdrName listTyConName
consDataCon_RDR = nameRdrName consDataConName
parrTyCon_RDR   = nameRdrName parrTyConName
eqTyCon_RDR     = nameRdrName eqTyConName

{-
************************************************************************
*                                                                      *
\subsection{mkWiredInTyCon}
*                                                                      *
************************************************************************
-}

pcNonRecDataTyCon :: Name -> Maybe CType -> [TyVar] -> [DataCon] -> TyCon
-- Not an enumeration, not promotable
pcNonRecDataTyCon = pcTyCon False NonRecursive False

-- This function assumes that the types it creates have all parameters at
-- Representational role!
pcTyCon :: Bool -> RecFlag -> Bool -> Name -> Maybe CType -> [TyVar] -> [DataCon] -> TyCon
pcTyCon is_enum is_rec is_prom name cType tyvars cons
  = buildAlgTyCon name
        tyvars
        (map (const Representational) tyvars)
        cType
        []              -- No stupid theta
        (DataTyCon cons is_enum)
        is_rec
        is_prom
        False           -- Not in GADT syntax
        NoParentTyCon

pcDataCon :: Name -> [TyVar] -> [Type] -> TyCon -> DataCon
pcDataCon = pcDataConWithFixity False

pcDataConWithFixity :: Bool -> Name -> [TyVar] -> [Type] -> TyCon -> DataCon
pcDataConWithFixity infx n = pcDataConWithFixity' infx n (incrUnique (nameUnique n))
-- The Name's unique is the first of two free uniques;
-- the first is used for the datacon itself,
-- the second is used for the "worker name"
--
-- To support this the mkPreludeDataConUnique function "allocates"
-- one DataCon unique per pair of Ints.

pcDataConWithFixity' :: Bool -> Name -> Unique -> [TyVar] -> [Type] -> TyCon -> DataCon
-- The Name should be in the DataName name space; it's the name
-- of the DataCon itself.

pcDataConWithFixity' declared_infix dc_name wrk_key tyvars arg_tys tycon
  = data_con
  where
    data_con = mkDataCon dc_name declared_infix
                (map (const no_bang) arg_tys)
                []      -- No labelled fields
                tyvars
                []      -- No existential type variables
                []      -- No equality spec
                []      -- No theta
                arg_tys (mkTyConApp tycon (mkTyVarTys tyvars))
                tycon
                []      -- No stupid theta
                (mkDataConWorkId wrk_name data_con)
                NoDataConRep    -- Wired-in types are too simple to need wrappers

    no_bang = HsSrcBang Nothing NoSrcUnpack NoSrcStrict

    modu     = ASSERT( isExternalName dc_name )
               nameModule dc_name
    wrk_occ  = mkDataConWorkerOcc (nameOccName dc_name)
    wrk_name = mkWiredInName modu wrk_occ wrk_key
                             (AnId (dataConWorkId data_con)) UserSyntax

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
typeNatKindCon    = pcTyCon False NonRecursive True typeNatKindConName    Nothing [] []
typeSymbolKindCon = pcTyCon False NonRecursive True typeSymbolKindConName Nothing [] []

typeNatKind, typeSymbolKind :: Kind
typeNatKind    = TyConApp (promoteTyCon typeNatKindCon)    []
typeSymbolKind = TyConApp (promoteTyCon typeSymbolKindCon) []

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
  betweeen BoxedTuple and ConstraintTuple (same OccName!), so tuples
  are not serialised into interface files using OccNames at all.
-}

isBuiltInOcc_maybe :: OccName -> Maybe Name
-- Built in syntax isn't "in scope" so these OccNames
-- map to wired-in Names with BuiltInSyntax
isBuiltInOcc_maybe occ
  = case occNameString occ of
        "[]"             -> choose_ns listTyConName nilDataConName
        ":"              -> Just consDataConName
        "[::]"           -> Just parrTyConName
        "()"             -> tup_name Boxed      0
        "(##)"           -> tup_name Unboxed    0
        '(':',':rest     -> parse_tuple Boxed   2 rest
        '(':'#':',':rest -> parse_tuple Unboxed 2 rest
        _other           -> Nothing
  where
    ns = occNameSpace occ

    parse_tuple sort n rest
      | (',' : rest2) <- rest   = parse_tuple sort (n+1) rest2
      | tail_matches sort rest  = tup_name sort n
      | otherwise               = Nothing

    tail_matches Boxed   ")" = True
    tail_matches Unboxed "#)" = True
    tail_matches _       _    = False

    tup_name boxity arity
      = choose_ns (getName (tupleTyCon   boxity arity))
                  (getName (tupleDataCon boxity arity))

    choose_ns tc dc
      | isTcClsNameSpace ns   = Just tc
      | isDataConNameSpace ns = Just dc
      | otherwise             = pprPanic "tup_name" (ppr occ)

mkTupleOcc :: NameSpace -> Boxity -> Arity -> OccName
mkTupleOcc ns sort ar = mkOccName ns str
  where
    -- No need to cache these, the caching is done in mk_tuple
    str = case sort of
                Unboxed    -> '(' : '#' : commas ++ "#)"
                Boxed      -> '(' : commas ++ ")"

    commas = take (ar-1) (repeat ',')

mkCTupleOcc :: NameSpace -> Arity -> OccName
mkCTupleOcc ns ar = mkOccName ns str
  where
    str    = "(%" ++ commas ++ "%)"
    commas = take (ar-1) (repeat ',')

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

promotedTupleTyCon :: Boxity -> Arity -> TyCon
promotedTupleTyCon boxity i = promoteTyCon (tupleTyCon boxity i)

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
mk_tuple boxity arity = (tycon, tuple_con)
  where
        tycon   = mkTupleTyCon tc_name tc_kind arity tyvars tuple_con
                               tup_sort
                               prom_tc NoParentTyCon

        tup_sort = case boxity of
                      Boxed   -> BoxedTuple
                      Unboxed -> UnboxedTuple

        prom_tc = case boxity of
                    Boxed   -> Just (mkPromotedTyCon tycon (promoteKind tc_kind))
                    Unboxed -> Nothing

        modu = case boxity of
                    Boxed -> gHC_TUPLE
                    Unboxed -> gHC_PRIM

        tc_name = mkWiredInName modu (mkTupleOcc tcName boxity arity) tc_uniq
                                (ATyCon tycon) BuiltInSyntax
        tc_kind = mkArrowKinds (map tyVarKind tyvars) res_kind

        res_kind = case boxity of
                     Boxed   -> liftedTypeKind
                     Unboxed -> unliftedTypeKind

        tyvars = take arity $ case boxity of
                   Boxed   -> alphaTyVars
                   Unboxed -> openAlphaTyVars

        tuple_con = pcDataCon dc_name tyvars tyvar_tys tycon
        tyvar_tys = mkTyVarTys tyvars
        dc_name   = mkWiredInName modu (mkTupleOcc dataName boxity arity) dc_uniq
                                  (AConLike (RealDataCon tuple_con)) BuiltInSyntax
        tc_uniq   = mkTupleTyConUnique   boxity arity
        dc_uniq   = mkTupleDataConUnique boxity arity

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

unboxedUnitTyCon :: TyCon
unboxedUnitTyCon = tupleTyCon Unboxed 0

unboxedUnitDataCon :: DataCon
unboxedUnitDataCon = tupleDataCon   Unboxed 0

unboxedSingletonTyCon :: TyCon
unboxedSingletonTyCon = tupleTyCon Unboxed 1

unboxedSingletonDataCon :: DataCon
unboxedSingletonDataCon = tupleDataCon Unboxed 1

unboxedPairTyCon :: TyCon
unboxedPairTyCon = tupleTyCon Unboxed 2

unboxedPairDataCon :: DataCon
unboxedPairDataCon = tupleDataCon Unboxed 2

{-
************************************************************************
*                                                                      *
     The ``boxed primitive'' types (@Char@, @Int@, etc)
*                                                                      *
************************************************************************
-}

charTy :: Type
charTy = mkTyConTy charTyCon

charTyCon :: TyCon
charTyCon   = pcNonRecDataTyCon charTyConName
                       (Just (CType "" Nothing ("HsChar",fsLit "HsChar")))
                       [] [charDataCon]
charDataCon :: DataCon
charDataCon = pcDataCon charDataConName [] [charPrimTy] charTyCon

stringTy :: Type
stringTy = mkListTy charTy -- convenience only

intTy :: Type
intTy = mkTyConTy intTyCon

intTyCon :: TyCon
intTyCon = pcNonRecDataTyCon intTyConName
                            (Just (CType "" Nothing ("HsInt",fsLit "HsInt"))) []
                            [intDataCon]
intDataCon :: DataCon
intDataCon = pcDataCon intDataConName [] [intPrimTy] intTyCon

wordTy :: Type
wordTy = mkTyConTy wordTyCon

wordTyCon :: TyCon
wordTyCon = pcNonRecDataTyCon wordTyConName
                      (Just (CType "" Nothing ("HsWord", fsLit "HsWord"))) []
                      [wordDataCon]
wordDataCon :: DataCon
wordDataCon = pcDataCon wordDataConName [] [wordPrimTy] wordTyCon

word8Ty :: Type
word8Ty = mkTyConTy word8TyCon

word8TyCon :: TyCon
word8TyCon = pcNonRecDataTyCon word8TyConName
                      (Just (CType "" Nothing ("HsWord8", fsLit "HsWord8"))) []
                      [word8DataCon]
word8DataCon :: DataCon
word8DataCon = pcDataCon word8DataConName [] [wordPrimTy] word8TyCon

floatTy :: Type
floatTy = mkTyConTy floatTyCon

floatTyCon :: TyCon
floatTyCon   = pcNonRecDataTyCon floatTyConName
                      (Just (CType "" Nothing ("HsFloat", fsLit "HsFloat"))) []
                      [floatDataCon]
floatDataCon :: DataCon
floatDataCon = pcDataCon         floatDataConName [] [floatPrimTy] floatTyCon

doubleTy :: Type
doubleTy = mkTyConTy doubleTyCon

doubleTyCon :: TyCon
doubleTyCon = pcNonRecDataTyCon doubleTyConName
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
boolTyCon = pcTyCon True NonRecursive True boolTyConName
                    (Just (CType "" Nothing ("HsBool", fsLit "HsBool")))
                    [] [falseDataCon, trueDataCon]

falseDataCon, trueDataCon :: DataCon
falseDataCon = pcDataCon falseDataConName [] [] boolTyCon
trueDataCon  = pcDataCon trueDataConName  [] [] boolTyCon

falseDataConId, trueDataConId :: Id
falseDataConId = dataConWorkId falseDataCon
trueDataConId  = dataConWorkId trueDataCon

orderingTyCon :: TyCon
orderingTyCon = pcTyCon True NonRecursive True orderingTyConName Nothing
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
listTyCon = pcTyCon False Recursive True
                    listTyConName Nothing alpha_tyvar [nilDataCon, consDataCon]

mkPromotedListTy :: Type -> Type
mkPromotedListTy ty = mkTyConApp promotedListTyCon [ty]

promotedListTyCon :: TyCon
promotedListTyCon = promoteTyCon listTyCon

nilDataCon :: DataCon
nilDataCon  = pcDataCon nilDataConName alpha_tyvar [] listTyCon

consDataCon :: DataCon
consDataCon = pcDataConWithFixity True {- Declared infix -}
               consDataConName
               alpha_tyvar [alphaTy, mkTyConApp listTyCon alpha_ty] listTyCon
-- Interesting: polymorphic recursion would help here.
-- We can't use (mkListTy alphaTy) in the defn of consDataCon, else mkListTy
-- gets the over-specific type (Type -> Type)

-- Wired-in type Maybe

maybeTyCon :: TyCon
maybeTyCon = pcTyCon True NonRecursive True maybeTyConName Nothing alpha_tyvar
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

mkTupleTy :: Boxity -> [Type] -> Type
-- Special case for *boxed* 1-tuples, which are represented by the type itself
mkTupleTy Boxed  [ty] = ty
mkTupleTy boxity tys  = mkTyConApp (tupleTyCon boxity (length tys)) tys

-- | Build the type of a small tuple that holds the specified type of thing
mkBoxedTupleTy :: [Type] -> Type
mkBoxedTupleTy tys = mkTupleTy Boxed tys

unitTy :: Type
unitTy = mkTupleTy Boxed []


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
parrTyCon  = pcNonRecDataTyCon parrTyConName Nothing alpha_tyvar [parrDataCon]

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

promotedBoolTyCon, promotedFalseDataCon, promotedTrueDataCon :: TyCon
promotedBoolTyCon     = promoteTyCon boolTyCon
promotedTrueDataCon   = promoteDataCon trueDataCon
promotedFalseDataCon  = promoteDataCon falseDataCon

-- Promoted Ordering

promotedOrderingTyCon
  , promotedLTDataCon
  , promotedEQDataCon
  , promotedGTDataCon
  :: TyCon
promotedOrderingTyCon = promoteTyCon orderingTyCon
promotedLTDataCon     = promoteDataCon ltDataCon
promotedEQDataCon     = promoteDataCon eqDataCon
promotedGTDataCon     = promoteDataCon gtDataCon

{- *********************************************************************
*                                                                      *
                         Type equalities
*                                                                      *
********************************************************************* -}

eqTyCon :: TyCon
eqTyCon = mkAlgTyCon eqTyConName
            (ForAllTy kv $ mkArrowKinds [k, k] constraintKind)
            [kv, a, b]
            [Nominal, Nominal, Nominal]
            Nothing
            []      -- No stupid theta
            (DataTyCon [eqBoxDataCon] False)
            NoParentTyCon
            NonRecursive
            False
            Nothing   -- No parent for constraint-kinded types
  where
    kv = kKiVar
    k = mkTyVarTy kv
    [a,b] = mkTemplateTyVars [k,k]

eqBoxDataCon :: DataCon
eqBoxDataCon = pcDataCon eqBoxDataConName args [TyConApp eqPrimTyCon (map mkTyVarTy args)] eqTyCon
  where
    kv = kKiVar
    k = mkTyVarTy kv
    [a,b] = mkTemplateTyVars [k,k]
    args = [kv, a, b]


coercibleTyCon :: TyCon
coercibleTyCon = mkClassTyCon
    coercibleTyConName kind tvs [Nominal, Representational, Representational]
    rhs coercibleClass NonRecursive
  where kind = (ForAllTy kv $ mkArrowKinds [k, k] constraintKind)
        kv = kKiVar
        k = mkTyVarTy kv
        [a,b] = mkTemplateTyVars [k,k]
        tvs = [kv, a, b]
        rhs = DataTyCon [coercibleDataCon] False

coercibleDataCon :: DataCon
coercibleDataCon = pcDataCon coercibleDataConName args [TyConApp eqReprPrimTyCon (map mkTyVarTy args)] coercibleTyCon
  where
    kv = kKiVar
    k = mkTyVarTy kv
    [a,b] = mkTemplateTyVars [k,k]
    args = [kv, a, b]

coercibleClass :: Class
coercibleClass = mkClass (tyConTyVars coercibleTyCon) [] [] [] [] [] (mkAnd []) coercibleTyCon

{-
Note [The Implicit Parameter class]

Implicit parameters `?x :: a` are desugared into dictionaries for the
class `IP "x" a`, which is defined (in GHC.Classes) as

  class IP (x :: Symbol) a | x -> a

This class is wired-in so that `error` and `undefined`, which have
wired-in types, can use the implicit-call-stack feature to provide
a call-stack alongside the error message.
-}

ipDataConName, ipTyConName, ipCoName :: Name
ipDataConName = mkWiredInDataConName UserSyntax gHC_CLASSES (fsLit "IP")
                  ipDataConKey ipDataCon
ipTyConName   = mkWiredInTyConName UserSyntax gHC_CLASSES (fsLit "IP")
                  ipTyConKey ipTyCon
ipCoName      = mkWiredInCoAxiomName BuiltInSyntax gHC_CLASSES (fsLit "NTCo:IP")
                  ipCoNameKey (toBranchedAxiom ipCoAxiom)

-- See Note [The Implicit Parameter class]
ipTyCon :: TyCon
ipTyCon = mkClassTyCon ipTyConName kind [ip,a] [] rhs ipClass NonRecursive
  where
    kind = mkArrowKinds [typeSymbolKind, liftedTypeKind] constraintKind
    [ip,a] = mkTemplateTyVars [typeSymbolKind, liftedTypeKind]
    rhs = NewTyCon ipDataCon (mkTyVarTy a) ([], mkTyVarTy a) ipCoAxiom

ipCoAxiom :: CoAxiom Unbranched
ipCoAxiom = mkNewTypeCo ipCoName ipTyCon [ip,a] [Nominal, Nominal] (mkTyVarTy a)
  where
    [ip,a] = mkTemplateTyVars [typeSymbolKind, liftedTypeKind]

ipDataCon :: DataCon
ipDataCon = pcDataCon ipDataConName [ip,a] ts ipTyCon
  where
    [ip,a] = mkTemplateTyVars [typeSymbolKind, liftedTypeKind]
    ts  = [mkTyVarTy a]

ipClass :: Class
ipClass = mkClass (tyConTyVars ipTyCon) [([ip], [a])] [] [] [] [] (mkAnd [])
            ipTyCon
  where
    [ip, a] = tyConTyVars ipTyCon

-- this is a fake version of the CallStack TyCon so we can refer to it
-- in MkCore.errorTy
callStackTyCon :: TyCon
callStackTyCon = pcNonRecDataTyCon callStackTyConName Nothing [] []
