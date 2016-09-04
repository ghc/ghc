{-
    %
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


TcGenDeriv: Generating derived instance declarations

This module is nominally ``subordinate'' to @TcDeriv@, which is the
``official'' interface to deriving-related things.

This is where we do all the grimy bindings' generation.
-}

{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TcGenDeriv (
        BagDerivStuff, DerivStuff(..),

        hasBuiltinDeriving,
        FFoldType(..), functorLikeTraverse,
        deepSubtypesContaining, foldDataConArgs,
        mkCoerceClassMethEqn,
        gen_Newtype_binds,
        genAuxBinds,
        ordOpTbl, boxConTbl, litConTbl,
        mkRdrFunBind
    ) where

#include "HsVersions.h"


import LoadIface( loadInterfaceForName )
import HscTypes( lookupFixity, mi_fix )
import TcRnMonad
import HsSyn
import RdrName
import BasicTypes
import Module( getModule )
import DataCon
import Name
import Fingerprint
import Encoding

import DynFlags
import PrelInfo
import FamInstEnv( FamInst )
import PrelNames
import THNames
import Module ( moduleName, moduleNameString
              , moduleUnitId, unitIdString )
import MkId ( coerceId )
import PrimOp
import SrcLoc
import TyCon
import TcType
import TysPrim
import TysWiredIn
import Type
import Class
import TyCoRep
import VarSet
import VarEnv
import State
import Util
import Var
import Outputable
import Lexeme
import FastString
import Pair
import Bag
import StaticFlags( opt_PprStyle_Debug )

import ListSetOps ( assocMaybe )
import Data.List  ( partition, intersperse )
import Data.Maybe ( catMaybes, isJust )

type BagDerivStuff = Bag DerivStuff

data AuxBindSpec
  = DerivCon2Tag TyCon  -- The con2Tag for given TyCon
  | DerivTag2Con TyCon  -- ...ditto tag2Con
  | DerivMaxTag  TyCon  -- ...and maxTag
  deriving( Eq )
  -- All these generate ZERO-BASED tag operations
  -- I.e first constructor has tag 0

data DerivStuff     -- Please add this auxiliary stuff
  = DerivAuxBind AuxBindSpec

  -- Generics and DeriveAnyClass
  | DerivFamInst FamInst               -- New type family instances

  -- New top-level auxiliary bindings
  | DerivHsBind (LHsBind RdrName, LSig RdrName) -- Also used for SYB

{-
************************************************************************
*                                                                      *
                Class deriving diagnostics
*                                                                      *
************************************************************************

Only certain blessed classes can be used in a deriving clause. These classes
are listed below in the definition of hasBuiltinDeriving (with the exception
of Generic and Generic1, which are handled separately in TcGenGenerics).

A class might be able to be used in a deriving clause if it -XDeriveAnyClass
is willing to support it. The canDeriveAnyClass function checks if this is
the case.
-}

hasBuiltinDeriving :: Class
                   -> Maybe (SrcSpan
                             -> TyCon
                             -> TcM (LHsBinds RdrName, BagDerivStuff))
hasBuiltinDeriving clas
  = assocMaybe gen_list (getUnique clas)
  where
    gen_list :: [(Unique, SrcSpan -> TyCon -> TcM (LHsBinds RdrName, BagDerivStuff))]
    gen_list = [ (eqClassKey,          simple gen_Eq_binds)
               , (ordClassKey,         simple gen_Ord_binds)
               , (enumClassKey,        simple gen_Enum_binds)
               , (boundedClassKey,     simple gen_Bounded_binds)
               , (ixClassKey,          simple gen_Ix_binds)
               , (showClassKey,        with_fix_env gen_Show_binds)
               , (readClassKey,        with_fix_env gen_Read_binds)
               , (dataClassKey,        gen_Data_binds)
               , (functorClassKey,     simple gen_Functor_binds)
               , (foldableClassKey,    simple gen_Foldable_binds)
               , (traversableClassKey, simple gen_Traversable_binds)
               , (liftClassKey,        simple gen_Lift_binds) ]

    simple gen_fn loc tc
      = return (gen_fn loc tc)

    with_fix_env gen_fn loc tc
      = do { fix_env <- getDataConFixityFun tc
           ; return (gen_fn fix_env loc tc) }

getDataConFixityFun :: TyCon -> TcM (Name -> Fixity)
-- If the TyCon is locally defined, we want the local fixity env;
-- but if it is imported (which happens for standalone deriving)
-- we need to get the fixity env from the interface file
-- c.f. RnEnv.lookupFixity, and Trac #9830
getDataConFixityFun tc
  = do { this_mod <- getModule
       ; if nameIsLocalOrFrom this_mod name
         then do { fix_env <- getFixityEnv
                 ; return (lookupFixity fix_env) }
         else do { iface <- loadInterfaceForName doc name
                            -- Should already be loaded!
                 ; return (mi_fix iface . nameOccName) } }
  where
    name = tyConName tc
    doc = text "Data con fixities for" <+> ppr name


{-
************************************************************************
*                                                                      *
                Eq instances
*                                                                      *
************************************************************************

Here are the heuristics for the code we generate for @Eq@. Let's
assume we have a data type with some (possibly zero) nullary data
constructors and some ordinary, non-nullary ones (the rest, also
possibly zero of them).  Here's an example, with both \tr{N}ullary and
\tr{O}rdinary data cons.

  data Foo ... = N1 | N2 ... | Nn | O1 a b | O2 Int | O3 Double b b | ...

* For the ordinary constructors (if any), we emit clauses to do The
  Usual Thing, e.g.,:

    (==) (O1 a1 b1)    (O1 a2 b2)    = a1 == a2 && b1 == b2
    (==) (O2 a1)       (O2 a2)       = a1 == a2
    (==) (O3 a1 b1 c1) (O3 a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2

  Note: if we're comparing unlifted things, e.g., if 'a1' and
  'a2' are Float#s, then we have to generate
       case (a1 `eqFloat#` a2) of r -> r
  for that particular test.

* If there are a lot of (more than en) nullary constructors, we emit a
  catch-all clause of the form:

      (==) a b  = case (con2tag_Foo a) of { a# ->
                  case (con2tag_Foo b) of { b# ->
                  case (a# ==# b#)     of {
                    r -> r }}}

  If con2tag gets inlined this leads to join point stuff, so
  it's better to use regular pattern matching if there aren't too
  many nullary constructors.  "Ten" is arbitrary, of course

* If there aren't any nullary constructors, we emit a simpler
  catch-all:

     (==) a b  = False

* For the @(/=)@ method, we normally just use the default method.
  If the type is an enumeration type, we could/may/should? generate
  special code that calls @con2tag_Foo@, much like for @(==)@ shown
  above.

We thought about doing this: If we're also deriving 'Ord' for this
tycon, we generate:
  instance ... Eq (Foo ...) where
    (==) a b  = case (compare a b) of { _LT -> False; _EQ -> True ; _GT -> False}
    (/=) a b  = case (compare a b) of { _LT -> True ; _EQ -> False; _GT -> True }
However, that requires that (Ord <whatever>) was put in the context
for the instance decl, which it probably wasn't, so the decls
produced don't get through the typechecker.
-}

gen_Eq_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Eq_binds loc tycon
  = (method_binds, aux_binds)
  where
    all_cons = tyConDataCons tycon
    (nullary_cons, non_nullary_cons) = partition isNullarySrcDataCon all_cons

    -- If there are ten or more (arbitrary number) nullary constructors,
    -- use the con2tag stuff.  For small types it's better to use
    -- ordinary pattern matching.
    (tag_match_cons, pat_match_cons)
       | nullary_cons `lengthExceeds` 10 = (nullary_cons, non_nullary_cons)
       | otherwise                       = ([],           all_cons)

    no_tag_match_cons = null tag_match_cons

    fall_through_eqn
      | no_tag_match_cons   -- All constructors have arguments
      = case pat_match_cons of
          []  -> []   -- No constructors; no fall-though case
          [_] -> []   -- One constructor; no fall-though case
          _   ->      -- Two or more constructors; add fall-through of
                      --       (==) _ _ = False
                 [([nlWildPat, nlWildPat], false_Expr)]

      | otherwise -- One or more tag_match cons; add fall-through of
                  -- extract tags compare for equality
      = [([a_Pat, b_Pat],
         untag_Expr tycon [(a_RDR,ah_RDR), (b_RDR,bh_RDR)]
                    (genPrimOpApp (nlHsVar ah_RDR) eqInt_RDR (nlHsVar bh_RDR)))]

    aux_binds | no_tag_match_cons = emptyBag
              | otherwise         = unitBag $ DerivAuxBind $ DerivCon2Tag tycon

    method_binds = listToBag [eq_bind, ne_bind]
    eq_bind = mk_FunBind loc eq_RDR (map pats_etc pat_match_cons ++ fall_through_eqn)
    ne_bind = mk_easy_FunBind loc ne_RDR [a_Pat, b_Pat] (
                        nlHsApp (nlHsVar not_RDR) (nlHsPar (nlHsVarApps eq_RDR [a_RDR, b_RDR])))

    ------------------------------------------------------------------
    pats_etc data_con
      = let
            con1_pat = nlConVarPat data_con_RDR as_needed
            con2_pat = nlConVarPat data_con_RDR bs_needed

            data_con_RDR = getRdrName data_con
            con_arity   = length tys_needed
            as_needed   = take con_arity as_RDRs
            bs_needed   = take con_arity bs_RDRs
            tys_needed  = dataConOrigArgTys data_con
        in
        ([con1_pat, con2_pat], nested_eq_expr tys_needed as_needed bs_needed)
      where
        nested_eq_expr []  [] [] = true_Expr
        nested_eq_expr tys as bs
          = foldl1 and_Expr (zipWith3Equal "nested_eq" nested_eq tys as bs)
          where
            nested_eq ty a b = nlHsPar (eq_Expr tycon ty (nlHsVar a) (nlHsVar b))

{-
************************************************************************
*                                                                      *
        Ord instances
*                                                                      *
************************************************************************

Note [Generating Ord instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose constructors are K1..Kn, and some are nullary.
The general form we generate is:

* Do case on first argument
        case a of
          K1 ... -> rhs_1
          K2 ... -> rhs_2
          ...
          Kn ... -> rhs_n
          _ -> nullary_rhs

* To make rhs_i
     If i = 1, 2, n-1, n, generate a single case.
        rhs_2    case b of
                   K1 {}  -> LT
                   K2 ... -> ...eq_rhs(K2)...
                   _      -> GT

     Otherwise do a tag compare against the bigger range
     (because this is the one most likely to succeed)
        rhs_3    case tag b of tb ->
                 if 3 <# tg then GT
                 else case b of
                         K3 ... -> ...eq_rhs(K3)....
                         _      -> LT

* To make eq_rhs(K), which knows that
    a = K a1 .. av
    b = K b1 .. bv
  we just want to compare (a1,b1) then (a2,b2) etc.
  Take care on the last field to tail-call into comparing av,bv

* To make nullary_rhs generate this
     case con2tag a of a# ->
     case con2tag b of ->
     a# `compare` b#

Several special cases:

* Two or fewer nullary constructors: don't generate nullary_rhs

* Be careful about unlifted comparisons.  When comparing unboxed
  values we can't call the overloaded functions.
  See function unliftedOrdOp

Note [Game plan for deriving Ord]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's a bad idea to define only 'compare', and build the other binary
comparisons on top of it; see Trac #2130, #4019.  Reason: we don't
want to laboriously make a three-way comparison, only to extract a
binary result, something like this:
     (>) (I# x) (I# y) = case <# x y of
                            True -> False
                            False -> case ==# x y of
                                       True  -> False
                                       False -> True

This being said, we can get away with generating full code only for
'compare' and '<' thus saving us generation of other three operators.
Other operators can be cheaply expressed through '<':
a <= b = not $ b < a
a > b = b < a
a >= b = not $ a < b

So for sufficiently small types (few constructors, or all nullary)
we generate all methods; for large ones we just use 'compare'.

-}

data OrdOp = OrdCompare | OrdLT | OrdLE | OrdGE | OrdGT

------------
ordMethRdr :: OrdOp -> RdrName
ordMethRdr op
  = case op of
       OrdCompare -> compare_RDR
       OrdLT      -> lt_RDR
       OrdLE      -> le_RDR
       OrdGE      -> ge_RDR
       OrdGT      -> gt_RDR

------------
ltResult :: OrdOp -> LHsExpr RdrName
-- Knowing a<b, what is the result for a `op` b?
ltResult OrdCompare = ltTag_Expr
ltResult OrdLT      = true_Expr
ltResult OrdLE      = true_Expr
ltResult OrdGE      = false_Expr
ltResult OrdGT      = false_Expr

------------
eqResult :: OrdOp -> LHsExpr RdrName
-- Knowing a=b, what is the result for a `op` b?
eqResult OrdCompare = eqTag_Expr
eqResult OrdLT      = false_Expr
eqResult OrdLE      = true_Expr
eqResult OrdGE      = true_Expr
eqResult OrdGT      = false_Expr

------------
gtResult :: OrdOp -> LHsExpr RdrName
-- Knowing a>b, what is the result for a `op` b?
gtResult OrdCompare = gtTag_Expr
gtResult OrdLT      = false_Expr
gtResult OrdLE      = false_Expr
gtResult OrdGE      = true_Expr
gtResult OrdGT      = true_Expr

------------
gen_Ord_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Ord_binds loc tycon
  | null tycon_data_cons        -- No data-cons => invoke bale-out case
  = (unitBag $ mk_FunBind loc compare_RDR [], emptyBag)
  | otherwise
  = (unitBag (mkOrdOp OrdCompare) `unionBags` other_ops, aux_binds)
  where
    aux_binds | single_con_type = emptyBag
              | otherwise       = unitBag $ DerivAuxBind $ DerivCon2Tag tycon

        -- Note [Game plan for deriving Ord]
    other_ops | (last_tag - first_tag) <= 2     -- 1-3 constructors
                || null non_nullary_cons        -- Or it's an enumeration
              = listToBag [mkOrdOp OrdLT, lE, gT, gE]
              | otherwise
              = emptyBag

    negate_expr = nlHsApp (nlHsVar not_RDR)
    lE = mk_easy_FunBind loc le_RDR [a_Pat, b_Pat] $
        negate_expr (nlHsApp (nlHsApp (nlHsVar lt_RDR) b_Expr) a_Expr)
    gT = mk_easy_FunBind loc gt_RDR [a_Pat, b_Pat] $
        nlHsApp (nlHsApp (nlHsVar lt_RDR) b_Expr) a_Expr
    gE = mk_easy_FunBind loc ge_RDR [a_Pat, b_Pat] $
        negate_expr (nlHsApp (nlHsApp (nlHsVar lt_RDR) a_Expr) b_Expr)

    get_tag con = dataConTag con - fIRST_TAG
        -- We want *zero-based* tags, because that's what
        -- con2Tag returns (generated by untag_Expr)!

    tycon_data_cons = tyConDataCons tycon
    single_con_type = isSingleton tycon_data_cons
    (first_con : _) = tycon_data_cons
    (last_con : _)  = reverse tycon_data_cons
    first_tag       = get_tag first_con
    last_tag        = get_tag last_con

    (nullary_cons, non_nullary_cons) = partition isNullarySrcDataCon tycon_data_cons


    mkOrdOp :: OrdOp -> LHsBind RdrName
    -- Returns a binding   op a b = ... compares a and b according to op ....
    mkOrdOp op = mk_easy_FunBind loc (ordMethRdr op) [a_Pat, b_Pat] (mkOrdOpRhs op)

    mkOrdOpRhs :: OrdOp -> LHsExpr RdrName
    mkOrdOpRhs op       -- RHS for comparing 'a' and 'b' according to op
      | length nullary_cons <= 2  -- Two nullary or fewer, so use cases
      = nlHsCase (nlHsVar a_RDR) $
        map (mkOrdOpAlt op) tycon_data_cons
        -- i.e.  case a of { C1 x y -> case b of C1 x y -> ....compare x,y...
        --                   C2 x   -> case b of C2 x -> ....comopare x.... }

      | null non_nullary_cons    -- All nullary, so go straight to comparing tags
      = mkTagCmp op

      | otherwise                -- Mixed nullary and non-nullary
      = nlHsCase (nlHsVar a_RDR) $
        (map (mkOrdOpAlt op) non_nullary_cons
         ++ [mkHsCaseAlt nlWildPat (mkTagCmp op)])


    mkOrdOpAlt :: OrdOp -> DataCon -> LMatch RdrName (LHsExpr RdrName)
    -- Make the alternative  (Ki a1 a2 .. av ->
    mkOrdOpAlt op data_con
      = mkHsCaseAlt (nlConVarPat data_con_RDR as_needed)
                    (mkInnerRhs op data_con)
      where
        as_needed    = take (dataConSourceArity data_con) as_RDRs
        data_con_RDR = getRdrName data_con

    mkInnerRhs op data_con
      | single_con_type
      = nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con ]

      | tag == first_tag
      = nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con
                                 , mkHsCaseAlt nlWildPat (ltResult op) ]
      | tag == last_tag
      = nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con
                                 , mkHsCaseAlt nlWildPat (gtResult op) ]

      | tag == first_tag + 1
      = nlHsCase (nlHsVar b_RDR) [ mkHsCaseAlt (nlConWildPat first_con)
                                             (gtResult op)
                                 , mkInnerEqAlt op data_con
                                 , mkHsCaseAlt nlWildPat (ltResult op) ]
      | tag == last_tag - 1
      = nlHsCase (nlHsVar b_RDR) [ mkHsCaseAlt (nlConWildPat last_con)
                                             (ltResult op)
                                 , mkInnerEqAlt op data_con
                                 , mkHsCaseAlt nlWildPat (gtResult op) ]

      | tag > last_tag `div` 2  -- lower range is larger
      = untag_Expr tycon [(b_RDR, bh_RDR)] $
        nlHsIf (genPrimOpApp (nlHsVar bh_RDR) ltInt_RDR tag_lit)
               (gtResult op) $  -- Definitely GT
        nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con
                                 , mkHsCaseAlt nlWildPat (ltResult op) ]

      | otherwise               -- upper range is larger
      = untag_Expr tycon [(b_RDR, bh_RDR)] $
        nlHsIf (genPrimOpApp (nlHsVar bh_RDR) gtInt_RDR tag_lit)
               (ltResult op) $  -- Definitely LT
        nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con
                                 , mkHsCaseAlt nlWildPat (gtResult op) ]
      where
        tag     = get_tag data_con
        tag_lit = noLoc (HsLit (HsIntPrim "" (toInteger tag)))

    mkInnerEqAlt :: OrdOp -> DataCon -> LMatch RdrName (LHsExpr RdrName)
    -- First argument 'a' known to be built with K
    -- Returns a case alternative  Ki b1 b2 ... bv -> compare (a1,a2,...) with (b1,b2,...)
    mkInnerEqAlt op data_con
      = mkHsCaseAlt (nlConVarPat data_con_RDR bs_needed) $
        mkCompareFields tycon op (dataConOrigArgTys data_con)
      where
        data_con_RDR = getRdrName data_con
        bs_needed    = take (dataConSourceArity data_con) bs_RDRs

    mkTagCmp :: OrdOp -> LHsExpr RdrName
    -- Both constructors known to be nullary
    -- genreates (case data2Tag a of a# -> case data2Tag b of b# -> a# `op` b#
    mkTagCmp op = untag_Expr tycon [(a_RDR, ah_RDR),(b_RDR, bh_RDR)] $
                  unliftedOrdOp tycon intPrimTy op ah_RDR bh_RDR

mkCompareFields :: TyCon -> OrdOp -> [Type] -> LHsExpr RdrName
-- Generates nested comparisons for (a1,a2...) against (b1,b2,...)
-- where the ai,bi have the given types
mkCompareFields tycon op tys
  = go tys as_RDRs bs_RDRs
  where
    go []   _      _          = eqResult op
    go [ty] (a:_)  (b:_)
      | isUnliftedType ty     = unliftedOrdOp tycon ty op a b
      | otherwise             = genOpApp (nlHsVar a) (ordMethRdr op) (nlHsVar b)
    go (ty:tys) (a:as) (b:bs) = mk_compare ty a b
                                  (ltResult op)
                                  (go tys as bs)
                                  (gtResult op)
    go _ _ _ = panic "mkCompareFields"

    -- (mk_compare ty a b) generates
    --    (case (compare a b) of { LT -> <lt>; EQ -> <eq>; GT -> <bt> })
    -- but with suitable special cases for
    mk_compare ty a b lt eq gt
      | isUnliftedType ty
      = unliftedCompare lt_op eq_op a_expr b_expr lt eq gt
      | otherwise
      = nlHsCase (nlHsPar (nlHsApp (nlHsApp (nlHsVar compare_RDR) a_expr) b_expr))
          [mkHsCaseAlt (nlNullaryConPat ltTag_RDR) lt,
           mkHsCaseAlt (nlNullaryConPat eqTag_RDR) eq,
           mkHsCaseAlt (nlNullaryConPat gtTag_RDR) gt]
      where
        a_expr = nlHsVar a
        b_expr = nlHsVar b
        (lt_op, _, eq_op, _, _) = primOrdOps "Ord" tycon ty

unliftedOrdOp :: TyCon -> Type -> OrdOp -> RdrName -> RdrName -> LHsExpr RdrName
unliftedOrdOp tycon ty op a b
  = case op of
       OrdCompare -> unliftedCompare lt_op eq_op a_expr b_expr
                                     ltTag_Expr eqTag_Expr gtTag_Expr
       OrdLT      -> wrap lt_op
       OrdLE      -> wrap le_op
       OrdGE      -> wrap ge_op
       OrdGT      -> wrap gt_op
  where
   (lt_op, le_op, eq_op, ge_op, gt_op) = primOrdOps "Ord" tycon ty
   wrap prim_op = genPrimOpApp a_expr prim_op b_expr
   a_expr = nlHsVar a
   b_expr = nlHsVar b

unliftedCompare :: RdrName -> RdrName
                -> LHsExpr RdrName -> LHsExpr RdrName   -- What to cmpare
                -> LHsExpr RdrName -> LHsExpr RdrName -> LHsExpr RdrName  -- Three results
                -> LHsExpr RdrName
-- Return (if a < b then lt else if a == b then eq else gt)
unliftedCompare lt_op eq_op a_expr b_expr lt eq gt
  = nlHsIf (ascribeBool $ genPrimOpApp a_expr lt_op b_expr) lt $
                        -- Test (<) first, not (==), because the latter
                        -- is true less often, so putting it first would
                        -- mean more tests (dynamically)
        nlHsIf (ascribeBool $ genPrimOpApp a_expr eq_op b_expr) eq gt
  where
    ascribeBool e = nlExprWithTySig e (toLHsSigWcType boolTy)

nlConWildPat :: DataCon -> LPat RdrName
-- The pattern (K {})
nlConWildPat con = noLoc (ConPatIn (noLoc (getRdrName con))
                                   (RecCon (HsRecFields { rec_flds = []
                                                        , rec_dotdot = Nothing })))

{-
************************************************************************
*                                                                      *
        Enum instances
*                                                                      *
************************************************************************

@Enum@ can only be derived for enumeration types.  For a type
\begin{verbatim}
data Foo ... = N1 | N2 | ... | Nn
\end{verbatim}

we use both @con2tag_Foo@ and @tag2con_Foo@ functions, as well as a
@maxtag_Foo@ variable (all generated by @gen_tag_n_con_binds@).

\begin{verbatim}
instance ... Enum (Foo ...) where
    succ x   = toEnum (1 + fromEnum x)
    pred x   = toEnum (fromEnum x - 1)

    toEnum i = tag2con_Foo i

    enumFrom a = map tag2con_Foo [con2tag_Foo a .. maxtag_Foo]

    -- or, really...
    enumFrom a
      = case con2tag_Foo a of
          a# -> map tag2con_Foo (enumFromTo (I# a#) maxtag_Foo)

   enumFromThen a b
     = map tag2con_Foo [con2tag_Foo a, con2tag_Foo b .. maxtag_Foo]

    -- or, really...
    enumFromThen a b
      = case con2tag_Foo a of { a# ->
        case con2tag_Foo b of { b# ->
        map tag2con_Foo (enumFromThenTo (I# a#) (I# b#) maxtag_Foo)
        }}
\end{verbatim}

For @enumFromTo@ and @enumFromThenTo@, we use the default methods.
-}

gen_Enum_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Enum_binds loc tycon
  = (method_binds, aux_binds)
  where
    method_binds = listToBag [
                        succ_enum,
                        pred_enum,
                        to_enum,
                        enum_from,
                        enum_from_then,
                        from_enum
                    ]
    aux_binds = listToBag $ map DerivAuxBind
                  [DerivCon2Tag tycon, DerivTag2Con tycon, DerivMaxTag tycon]

    occ_nm = getOccString tycon

    succ_enum
      = mk_easy_FunBind loc succ_RDR [a_Pat] $
        untag_Expr tycon [(a_RDR, ah_RDR)] $
        nlHsIf (nlHsApps eq_RDR [nlHsVar (maxtag_RDR tycon),
                               nlHsVarApps intDataCon_RDR [ah_RDR]])
             (illegal_Expr "succ" occ_nm "tried to take `succ' of last tag in enumeration")
             (nlHsApp (nlHsVar (tag2con_RDR tycon))
                    (nlHsApps plus_RDR [nlHsVarApps intDataCon_RDR [ah_RDR],
                                        nlHsIntLit 1]))

    pred_enum
      = mk_easy_FunBind loc pred_RDR [a_Pat] $
        untag_Expr tycon [(a_RDR, ah_RDR)] $
        nlHsIf (nlHsApps eq_RDR [nlHsIntLit 0,
                               nlHsVarApps intDataCon_RDR [ah_RDR]])
             (illegal_Expr "pred" occ_nm "tried to take `pred' of first tag in enumeration")
             (nlHsApp (nlHsVar (tag2con_RDR tycon))
                           (nlHsApps plus_RDR [nlHsVarApps intDataCon_RDR [ah_RDR],
                                               nlHsLit (HsInt "-1" (-1))]))

    to_enum
      = mk_easy_FunBind loc toEnum_RDR [a_Pat] $
        nlHsIf (nlHsApps and_RDR
                [nlHsApps ge_RDR [nlHsVar a_RDR, nlHsIntLit 0],
                 nlHsApps le_RDR [nlHsVar a_RDR, nlHsVar (maxtag_RDR tycon)]])
             (nlHsVarApps (tag2con_RDR tycon) [a_RDR])
             (illegal_toEnum_tag occ_nm (maxtag_RDR tycon))

    enum_from
      = mk_easy_FunBind loc enumFrom_RDR [a_Pat] $
          untag_Expr tycon [(a_RDR, ah_RDR)] $
          nlHsApps map_RDR
                [nlHsVar (tag2con_RDR tycon),
                 nlHsPar (enum_from_to_Expr
                            (nlHsVarApps intDataCon_RDR [ah_RDR])
                            (nlHsVar (maxtag_RDR tycon)))]

    enum_from_then
      = mk_easy_FunBind loc enumFromThen_RDR [a_Pat, b_Pat] $
          untag_Expr tycon [(a_RDR, ah_RDR), (b_RDR, bh_RDR)] $
          nlHsApp (nlHsVarApps map_RDR [tag2con_RDR tycon]) $
            nlHsPar (enum_from_then_to_Expr
                    (nlHsVarApps intDataCon_RDR [ah_RDR])
                    (nlHsVarApps intDataCon_RDR [bh_RDR])
                    (nlHsIf  (nlHsApps gt_RDR [nlHsVarApps intDataCon_RDR [ah_RDR],
                                               nlHsVarApps intDataCon_RDR [bh_RDR]])
                           (nlHsIntLit 0)
                           (nlHsVar (maxtag_RDR tycon))
                           ))

    from_enum
      = mk_easy_FunBind loc fromEnum_RDR [a_Pat] $
          untag_Expr tycon [(a_RDR, ah_RDR)] $
          (nlHsVarApps intDataCon_RDR [ah_RDR])

{-
************************************************************************
*                                                                      *
        Bounded instances
*                                                                      *
************************************************************************
-}

gen_Bounded_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Bounded_binds loc tycon
  | isEnumerationTyCon tycon
  = (listToBag [ min_bound_enum, max_bound_enum ], emptyBag)
  | otherwise
  = ASSERT(isSingleton data_cons)
    (listToBag [ min_bound_1con, max_bound_1con ], emptyBag)
  where
    data_cons = tyConDataCons tycon

    ----- enum-flavored: ---------------------------
    min_bound_enum = mkHsVarBind loc minBound_RDR (nlHsVar data_con_1_RDR)
    max_bound_enum = mkHsVarBind loc maxBound_RDR (nlHsVar data_con_N_RDR)

    data_con_1     = head data_cons
    data_con_N     = last data_cons
    data_con_1_RDR = getRdrName data_con_1
    data_con_N_RDR = getRdrName data_con_N

    ----- single-constructor-flavored: -------------
    arity          = dataConSourceArity data_con_1

    min_bound_1con = mkHsVarBind loc minBound_RDR $
                     nlHsVarApps data_con_1_RDR (nOfThem arity minBound_RDR)
    max_bound_1con = mkHsVarBind loc maxBound_RDR $
                     nlHsVarApps data_con_1_RDR (nOfThem arity maxBound_RDR)

{-
************************************************************************
*                                                                      *
        Ix instances
*                                                                      *
************************************************************************

Deriving @Ix@ is only possible for enumeration types and
single-constructor types.  We deal with them in turn.

For an enumeration type, e.g.,
\begin{verbatim}
    data Foo ... = N1 | N2 | ... | Nn
\end{verbatim}
things go not too differently from @Enum@:
\begin{verbatim}
instance ... Ix (Foo ...) where
    range (a, b)
      = map tag2con_Foo [con2tag_Foo a .. con2tag_Foo b]

    -- or, really...
    range (a, b)
      = case (con2tag_Foo a) of { a# ->
        case (con2tag_Foo b) of { b# ->
        map tag2con_Foo (enumFromTo (I# a#) (I# b#))
        }}

    -- Generate code for unsafeIndex, because using index leads
    -- to lots of redundant range tests
    unsafeIndex c@(a, b) d
      = case (con2tag_Foo d -# con2tag_Foo a) of
               r# -> I# r#

    inRange (a, b) c
      = let
            p_tag = con2tag_Foo c
        in
        p_tag >= con2tag_Foo a && p_tag <= con2tag_Foo b

    -- or, really...
    inRange (a, b) c
      = case (con2tag_Foo a)   of { a_tag ->
        case (con2tag_Foo b)   of { b_tag ->
        case (con2tag_Foo c)   of { c_tag ->
        if (c_tag >=# a_tag) then
          c_tag <=# b_tag
        else
          False
        }}}
\end{verbatim}
(modulo suitable case-ification to handle the unlifted tags)

For a single-constructor type (NB: this includes all tuples), e.g.,
\begin{verbatim}
    data Foo ... = MkFoo a b Int Double c c
\end{verbatim}
we follow the scheme given in Figure~19 of the Haskell~1.2 report
(p.~147).
-}

gen_Ix_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)

gen_Ix_binds loc tycon
  | isEnumerationTyCon tycon
  = ( enum_ixes
    , listToBag $ map DerivAuxBind
                   [DerivCon2Tag tycon, DerivTag2Con tycon, DerivMaxTag tycon])
  | otherwise
  = (single_con_ixes, unitBag (DerivAuxBind (DerivCon2Tag tycon)))
  where
    --------------------------------------------------------------
    enum_ixes = listToBag [ enum_range, enum_index, enum_inRange ]

    enum_range
      = mk_easy_FunBind loc range_RDR [nlTuplePat [a_Pat, b_Pat] Boxed] $
          untag_Expr tycon [(a_RDR, ah_RDR)] $
          untag_Expr tycon [(b_RDR, bh_RDR)] $
          nlHsApp (nlHsVarApps map_RDR [tag2con_RDR tycon]) $
              nlHsPar (enum_from_to_Expr
                        (nlHsVarApps intDataCon_RDR [ah_RDR])
                        (nlHsVarApps intDataCon_RDR [bh_RDR]))

    enum_index
      = mk_easy_FunBind loc unsafeIndex_RDR
                [noLoc (AsPat (noLoc c_RDR)
                           (nlTuplePat [a_Pat, nlWildPat] Boxed)),
                                d_Pat] (
           untag_Expr tycon [(a_RDR, ah_RDR)] (
           untag_Expr tycon [(d_RDR, dh_RDR)] (
           let
                rhs = nlHsVarApps intDataCon_RDR [c_RDR]
           in
           nlHsCase
             (genOpApp (nlHsVar dh_RDR) minusInt_RDR (nlHsVar ah_RDR))
             [mkHsCaseAlt (nlVarPat c_RDR) rhs]
           ))
        )

    -- This produces something like `(ch >= ah) && (ch <= bh)`
    enum_inRange
      = mk_easy_FunBind loc inRange_RDR [nlTuplePat [a_Pat, b_Pat] Boxed, c_Pat] $
          untag_Expr tycon [(a_RDR, ah_RDR)] (
          untag_Expr tycon [(b_RDR, bh_RDR)] (
          untag_Expr tycon [(c_RDR, ch_RDR)] (
          -- This used to use `if`, which interacts badly with RebindableSyntax.
          -- See #11396.
          nlHsApps and_RDR
              [ genPrimOpApp (nlHsVar ch_RDR) geInt_RDR (nlHsVar ah_RDR)
              , genPrimOpApp (nlHsVar ch_RDR) leInt_RDR (nlHsVar bh_RDR)
              ]
          )))

    --------------------------------------------------------------
    single_con_ixes
      = listToBag [single_con_range, single_con_index, single_con_inRange]

    data_con
      = case tyConSingleDataCon_maybe tycon of -- just checking...
          Nothing -> panic "get_Ix_binds"
          Just dc -> dc

    con_arity    = dataConSourceArity data_con
    data_con_RDR = getRdrName data_con

    as_needed = take con_arity as_RDRs
    bs_needed = take con_arity bs_RDRs
    cs_needed = take con_arity cs_RDRs

    con_pat  xs  = nlConVarPat data_con_RDR xs
    con_expr     = nlHsVarApps data_con_RDR cs_needed

    --------------------------------------------------------------
    single_con_range
      = mk_easy_FunBind loc range_RDR
          [nlTuplePat [con_pat as_needed, con_pat bs_needed] Boxed] $
        noLoc (mkHsComp ListComp stmts con_expr)
      where
        stmts = zipWith3Equal "single_con_range" mk_qual as_needed bs_needed cs_needed

        mk_qual a b c = noLoc $ mkBindStmt (nlVarPat c)
                                 (nlHsApp (nlHsVar range_RDR)
                                          (mkLHsVarTuple [a,b]))

    ----------------
    single_con_index
      = mk_easy_FunBind loc unsafeIndex_RDR
                [nlTuplePat [con_pat as_needed, con_pat bs_needed] Boxed,
                 con_pat cs_needed]
        -- We need to reverse the order we consider the components in
        -- so that
        --     range (l,u) !! index (l,u) i == i   -- when i is in range
        -- (from http://haskell.org/onlinereport/ix.html) holds.
                (mk_index (reverse $ zip3 as_needed bs_needed cs_needed))
      where
        -- index (l1,u1) i1 + rangeSize (l1,u1) * (index (l2,u2) i2 + ...)
        mk_index []        = nlHsIntLit 0
        mk_index [(l,u,i)] = mk_one l u i
        mk_index ((l,u,i) : rest)
          = genOpApp (
                mk_one l u i
            ) plus_RDR (
                genOpApp (
                    (nlHsApp (nlHsVar unsafeRangeSize_RDR)
                             (mkLHsVarTuple [l,u]))
                ) times_RDR (mk_index rest)
           )
        mk_one l u i
          = nlHsApps unsafeIndex_RDR [mkLHsVarTuple [l,u], nlHsVar i]

    ------------------
    single_con_inRange
      = mk_easy_FunBind loc inRange_RDR
                [nlTuplePat [con_pat as_needed, con_pat bs_needed] Boxed,
                 con_pat cs_needed] $
          foldl1 and_Expr (zipWith3Equal "single_con_inRange" in_range as_needed bs_needed cs_needed)
      where
        in_range a b c = nlHsApps inRange_RDR [mkLHsVarTuple [a,b], nlHsVar c]

{-
************************************************************************
*                                                                      *
        Read instances
*                                                                      *
************************************************************************

Example

  infix 4 %%
  data T = Int %% Int
         | T1 { f1 :: Int }
         | T2 T

instance Read T where
  readPrec =
    parens
    ( prec 4 (
        do x <- ReadP.step Read.readPrec
           expectP (Symbol "%%")
           y <- ReadP.step Read.readPrec
           return (x %% y))
      +++
      prec (appPrec+1) (
        -- Note the "+1" part; "T2 T1 {f1=3}" should parse ok
        -- Record construction binds even more tightly than application
        do expectP (Ident "T1")
           expectP (Punc '{')
           expectP (Ident "f1")
           expectP (Punc '=')
           x          <- ReadP.reset Read.readPrec
           expectP (Punc '}')
           return (T1 { f1 = x }))
      +++
      prec appPrec (
        do expectP (Ident "T2")
           x <- ReadP.step Read.readPrec
           return (T2 x))
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault


Note [Use expectP]
~~~~~~~~~~~~~~~~~~
Note that we use
   expectP (Ident "T1")
rather than
   Ident "T1" <- lexP
The latter desugares to inline code for matching the Ident and the
string, and this can be very voluminous. The former is much more
compact.  Cf Trac #7258, although that also concerned non-linearity in
the occurrence analyser, a separate issue.

Note [Read for empty data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should we get for this?  (Trac #7931)
   data Emp deriving( Read )   -- No data constructors

Here we want
  read "[]" :: [Emp]   to succeed, returning []
So we do NOT want
   instance Read Emp where
     readPrec = error "urk"
Rather we want
   instance Read Emp where
     readPred = pfail   -- Same as choose []

Because 'pfail' allows the parser to backtrack, but 'error' doesn't.
These instances are also useful for Read (Either Int Emp), where
we want to be able to parse (Left 3) just fine.
-}

gen_Read_binds :: (Name -> Fixity) -> SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)

gen_Read_binds get_fixity loc tycon
  = (listToBag [read_prec, default_readlist, default_readlistprec], emptyBag)
  where
    -----------------------------------------------------------------------
    default_readlist
        = mkHsVarBind loc readList_RDR     (nlHsVar readListDefault_RDR)

    default_readlistprec
        = mkHsVarBind loc readListPrec_RDR (nlHsVar readListPrecDefault_RDR)
    -----------------------------------------------------------------------

    data_cons = tyConDataCons tycon
    (nullary_cons, non_nullary_cons) = partition isNullarySrcDataCon data_cons

    read_prec = mkHsVarBind loc readPrec_RDR
                              (nlHsApp (nlHsVar parens_RDR) read_cons)

    read_cons | null data_cons = nlHsVar pfail_RDR  -- See Note [Read for empty data types]
              | otherwise      = foldr1 mk_alt (read_nullary_cons ++ read_non_nullary_cons)
    read_non_nullary_cons = map read_non_nullary_con non_nullary_cons

    read_nullary_cons
      = case nullary_cons of
            []    -> []
            [con] -> [nlHsDo DoExpr (match_con con ++ [noLoc $ mkLastStmt (result_expr con [])])]
            _     -> [nlHsApp (nlHsVar choose_RDR)
                              (nlList (map mk_pair nullary_cons))]
        -- NB For operators the parens around (:=:) are matched by the
        -- enclosing "parens" call, so here we must match the naked
        -- data_con_str con

    match_con con | isSym con_str = [symbol_pat con_str]
                  | otherwise     = ident_h_pat  con_str
                  where
                    con_str = data_con_str con
        -- For nullary constructors we must match Ident s for normal constrs
        -- and   Symbol s   for operators

    mk_pair con = mkLHsTupleExpr [nlHsLit (mkHsString (data_con_str con)),
                                  result_expr con []]

    read_non_nullary_con data_con
      | is_infix  = mk_parser infix_prec  infix_stmts  body
      | is_record = mk_parser record_prec record_stmts body
--              Using these two lines instead allows the derived
--              read for infix and record bindings to read the prefix form
--      | is_infix  = mk_alt prefix_parser (mk_parser infix_prec  infix_stmts  body)
--      | is_record = mk_alt prefix_parser (mk_parser record_prec record_stmts body)
      | otherwise = prefix_parser
      where
        body = result_expr data_con as_needed
        con_str = data_con_str data_con

        prefix_parser = mk_parser prefix_prec prefix_stmts body

        read_prefix_con
            | isSym con_str = [read_punc "(", symbol_pat con_str, read_punc ")"]
            | otherwise     = ident_h_pat con_str

        read_infix_con
            | isSym con_str = [symbol_pat con_str]
            | otherwise     = [read_punc "`"] ++ ident_h_pat con_str ++ [read_punc "`"]

        prefix_stmts            -- T a b c
          = read_prefix_con ++ read_args

        infix_stmts             -- a %% b, or  a `T` b
          = [read_a1]
            ++ read_infix_con
            ++ [read_a2]

        record_stmts            -- T { f1 = a, f2 = b }
          = read_prefix_con
            ++ [read_punc "{"]
            ++ concat (intersperse [read_punc ","] field_stmts)
            ++ [read_punc "}"]

        field_stmts  = zipWithEqual "lbl_stmts" read_field labels as_needed

        con_arity    = dataConSourceArity data_con
        labels       = map flLabel $ dataConFieldLabels data_con
        dc_nm        = getName data_con
        is_infix     = dataConIsInfix data_con
        is_record    = length labels > 0
        as_needed    = take con_arity as_RDRs
        read_args    = zipWithEqual "gen_Read_binds" read_arg as_needed (dataConOrigArgTys data_con)
        (read_a1:read_a2:_) = read_args

        prefix_prec = appPrecedence
        infix_prec  = getPrecedence get_fixity dc_nm
        record_prec = appPrecedence + 1 -- Record construction binds even more tightly
                                        -- than application; e.g. T2 T1 {x=2} means T2 (T1 {x=2})

    ------------------------------------------------------------------------
    --          Helpers
    ------------------------------------------------------------------------
    mk_alt e1 e2       = genOpApp e1 alt_RDR e2                         -- e1 +++ e2
    mk_parser p ss b   = nlHsApps prec_RDR [nlHsIntLit p                -- prec p (do { ss ; b })
                                           , nlHsDo DoExpr (ss ++ [noLoc $ mkLastStmt b])]
    con_app con as     = nlHsVarApps (getRdrName con) as                -- con as
    result_expr con as = nlHsApp (nlHsVar returnM_RDR) (con_app con as) -- return (con as)

    -- For constructors and field labels ending in '#', we hackily
    -- let the lexer generate two tokens, and look for both in sequence
    -- Thus [Ident "I"; Symbol "#"].  See Trac #5041
    ident_h_pat s | Just (ss, '#') <- snocView s = [ ident_pat ss, symbol_pat "#" ]
                  | otherwise                    = [ ident_pat s ]

    bindLex pat  = noLoc (mkBodyStmt (nlHsApp (nlHsVar expectP_RDR) pat))  -- expectP p
                   -- See Note [Use expectP]
    ident_pat  s = bindLex $ nlHsApps ident_RDR  [nlHsLit (mkHsString s)]  -- expectP (Ident "foo")
    symbol_pat s = bindLex $ nlHsApps symbol_RDR [nlHsLit (mkHsString s)]  -- expectP (Symbol ">>")
    read_punc c  = bindLex $ nlHsApps punc_RDR   [nlHsLit (mkHsString c)]  -- expectP (Punc "<")

    data_con_str con = occNameString (getOccName con)

    read_arg a ty = ASSERT( not (isUnliftedType ty) )
                    noLoc (mkBindStmt (nlVarPat a) (nlHsVarApps step_RDR [readPrec_RDR]))

    read_field lbl a = read_lbl lbl ++
                       [read_punc "=",
                        noLoc (mkBindStmt (nlVarPat a) (nlHsVarApps reset_RDR [readPrec_RDR]))]

        -- When reading field labels we might encounter
        --      a  = 3
        --      _a = 3
        -- or   (#) = 4
        -- Note the parens!
    read_lbl lbl | isSym lbl_str
                 = [read_punc "(", symbol_pat lbl_str, read_punc ")"]
                 | otherwise
                 = ident_h_pat lbl_str
                 where
                   lbl_str = unpackFS lbl

{-
************************************************************************
*                                                                      *
        Show instances
*                                                                      *
************************************************************************

Example

    infixr 5 :^:

    data Tree a =  Leaf a  |  Tree a :^: Tree a

    instance (Show a) => Show (Tree a) where

        showsPrec d (Leaf m) = showParen (d > app_prec) showStr
          where
             showStr = showString "Leaf " . showsPrec (app_prec+1) m

        showsPrec d (u :^: v) = showParen (d > up_prec) showStr
          where
             showStr = showsPrec (up_prec+1) u .
                       showString " :^: "      .
                       showsPrec (up_prec+1) v
                -- Note: right-associativity of :^: ignored

    up_prec  = 5    -- Precedence of :^:
    app_prec = 10   -- Application has precedence one more than
                    -- the most tightly-binding operator
-}

gen_Show_binds :: (Name -> Fixity) -> SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)

gen_Show_binds get_fixity loc tycon
  = (listToBag [shows_prec, show_list], emptyBag)
  where
    -----------------------------------------------------------------------
    show_list = mkHsVarBind loc showList_RDR
                  (nlHsApp (nlHsVar showList___RDR) (nlHsPar (nlHsApp (nlHsVar showsPrec_RDR) (nlHsIntLit 0))))
    -----------------------------------------------------------------------
    data_cons = tyConDataCons tycon
    shows_prec = mk_FunBind loc showsPrec_RDR (map pats_etc data_cons)

    pats_etc data_con
      | nullary_con =  -- skip the showParen junk...
         ASSERT(null bs_needed)
         ([nlWildPat, con_pat], mk_showString_app op_con_str)
      | otherwise   =
         ([a_Pat, con_pat],
          showParen_Expr (genOpApp a_Expr ge_RDR
                              (nlHsLit (HsInt "" con_prec_plus_one)))
                         (nlHsPar (nested_compose_Expr show_thingies)))
        where
             data_con_RDR  = getRdrName data_con
             con_arity     = dataConSourceArity data_con
             bs_needed     = take con_arity bs_RDRs
             arg_tys       = dataConOrigArgTys data_con         -- Correspond 1-1 with bs_needed
             con_pat       = nlConVarPat data_con_RDR bs_needed
             nullary_con   = con_arity == 0
             labels        = map flLabel $ dataConFieldLabels data_con
             lab_fields    = length labels
             record_syntax = lab_fields > 0

             dc_nm          = getName data_con
             dc_occ_nm      = getOccName data_con
             con_str        = occNameString dc_occ_nm
             op_con_str     = wrapOpParens con_str
             backquote_str  = wrapOpBackquotes con_str

             show_thingies
                | is_infix      = [show_arg1, mk_showString_app (" " ++ backquote_str ++ " "), show_arg2]
                | record_syntax = mk_showString_app (op_con_str ++ " {") :
                                  show_record_args ++ [mk_showString_app "}"]
                | otherwise     = mk_showString_app (op_con_str ++ " ") : show_prefix_args

             show_label l = mk_showString_app (nm ++ " = ")
                        -- Note the spaces around the "=" sign.  If we
                        -- don't have them then we get Foo { x=-1 } and
                        -- the "=-" parses as a single lexeme.  Only the
                        -- space after the '=' is necessary, but it
                        -- seems tidier to have them both sides.
                 where
                   nm       = wrapOpParens (unpackFS l)

             show_args               = zipWith show_arg bs_needed arg_tys
             (show_arg1:show_arg2:_) = show_args
             show_prefix_args        = intersperse (nlHsVar showSpace_RDR) show_args

                -- Assumption for record syntax: no of fields == no of
                -- labelled fields (and in same order)
             show_record_args = concat $
                                intersperse [mk_showString_app ", "] $
                                [ [show_label lbl, arg]
                                | (lbl,arg) <- zipEqual "gen_Show_binds"
                                                        labels show_args ]

             show_arg :: RdrName -> Type -> LHsExpr RdrName
             show_arg b arg_ty
               | isUnliftedType arg_ty
               -- See Note [Deriving and unboxed types] in TcDeriv
               = nlHsApps compose_RDR [mk_shows_app boxed_arg,
                                       mk_showString_app postfixMod]
               | otherwise
               = mk_showsPrec_app arg_prec arg
                 where
                   arg        = nlHsVar b
                   boxed_arg  = box "Show" tycon arg arg_ty
                   postfixMod = assoc_ty_id "Show" tycon postfixModTbl arg_ty

                -- Fixity stuff
             is_infix = dataConIsInfix data_con
             con_prec_plus_one = 1 + getPrec is_infix get_fixity dc_nm
             arg_prec | record_syntax = 0  -- Record fields don't need parens
                      | otherwise     = con_prec_plus_one

wrapOpParens :: String -> String
wrapOpParens s | isSym s   = '(' : s ++ ")"
               | otherwise = s

wrapOpBackquotes :: String -> String
wrapOpBackquotes s | isSym s   = s
                   | otherwise = '`' : s ++ "`"

isSym :: String -> Bool
isSym ""      = False
isSym (c : _) = startsVarSym c || startsConSym c

-- | showString :: String -> ShowS
mk_showString_app :: String -> LHsExpr RdrName
mk_showString_app str = nlHsApp (nlHsVar showString_RDR) (nlHsLit (mkHsString str))

-- | showsPrec :: Show a => Int -> a -> ShowS
mk_showsPrec_app :: Integer -> LHsExpr RdrName -> LHsExpr RdrName
mk_showsPrec_app p x = nlHsApps showsPrec_RDR [nlHsLit (HsInt "" p), x]

-- | shows :: Show a => a -> ShowS
mk_shows_app :: LHsExpr RdrName -> LHsExpr RdrName
mk_shows_app x = nlHsApp (nlHsVar shows_RDR) x

getPrec :: Bool -> (Name -> Fixity) -> Name -> Integer
getPrec is_infix get_fixity nm
  | not is_infix   = appPrecedence
  | otherwise      = getPrecedence get_fixity nm

appPrecedence :: Integer
appPrecedence = fromIntegral maxPrecedence + 1
  -- One more than the precedence of the most
  -- tightly-binding operator

getPrecedence :: (Name -> Fixity) -> Name -> Integer
getPrecedence get_fixity nm
   = case get_fixity nm of
        Fixity _ x _assoc -> fromIntegral x
          -- NB: the Report says that associativity is not taken
          --     into account for either Read or Show; hence we
          --     ignore associativity here

{-
************************************************************************
*                                                                      *
        Data instances
*                                                                      *
************************************************************************

From the data type

  data T a b = T1 a b | T2

we generate

  $cT1 = mkDataCon $dT "T1" Prefix
  $cT2 = mkDataCon $dT "T2" Prefix
  $dT  = mkDataType "Module.T" [] [$con_T1, $con_T2]
  -- the [] is for field labels.

  instance (Data a, Data b) => Data (T a b) where
    gfoldl k z (T1 a b) = z T `k` a `k` b
    gfoldl k z T2           = z T2
    -- ToDo: add gmapT,Q,M, gfoldr

    gunfold k z c = case conIndex c of
                        I# 1# -> k (k (z T1))
                        I# 2# -> z T2

    toConstr (T1 _ _) = $cT1
    toConstr T2       = $cT2

    dataTypeOf _ = $dT

    dataCast1 = gcast1   -- If T :: * -> *
    dataCast2 = gcast2   -- if T :: * -> * -> *
-}

gen_Data_binds :: SrcSpan
               -> TyCon                 -- For data families, this is the
                                        --  *representation* TyCon
               -> TcM (LHsBinds RdrName,    -- The method bindings
                       BagDerivStuff)       -- Auxiliary bindings
gen_Data_binds loc rep_tc
  = do { dflags  <- getDynFlags

       -- Make unique names for the data type and constructor
       -- auxiliary bindings.  Start with the name of the TyCon/DataCon
       -- but that might not be unique: see Trac #12245.
       ; dt_occ  <- chooseUniqueOccTc (mkDataTOcc (getOccName rep_tc))
       ; dc_occs <- mapM (chooseUniqueOccTc . mkDataCOcc . getOccName)
                         (tyConDataCons rep_tc)
       ; let dt_rdr  = mkRdrUnqual dt_occ
             dc_rdrs = map mkRdrUnqual dc_occs

       -- OK, now do the work
       ; return (gen_data dflags dt_rdr dc_rdrs loc rep_tc) }

gen_data :: DynFlags -> RdrName -> [RdrName]
         -> SrcSpan -> TyCon
         -> (LHsBinds RdrName,    -- The method bindings
             BagDerivStuff)       -- Auxiliary bindings
gen_data dflags data_type_name constr_names loc rep_tc
  = (listToBag [gfoldl_bind, gunfold_bind, toCon_bind, dataTypeOf_bind]
     `unionBags` gcast_binds,
                -- Auxiliary definitions: the data type and constructors
     listToBag ( genDataTyCon
               : zipWith genDataDataCon data_cons constr_names ) )
  where
    data_cons  = tyConDataCons rep_tc
    n_cons     = length data_cons
    one_constr = n_cons == 1
    genDataTyCon :: DerivStuff
    genDataTyCon        --  $dT
      = DerivHsBind (mkHsVarBind loc data_type_name rhs,
                     L loc (TypeSig [L loc data_type_name] sig_ty))

    sig_ty = mkLHsSigWcType (nlHsTyVar dataType_RDR)
    rhs    = nlHsVar mkDataType_RDR
             `nlHsApp` nlHsLit (mkHsString (showSDocOneLine dflags (ppr rep_tc)))
             `nlHsApp` nlList (map nlHsVar constr_names)

    genDataDataCon :: DataCon -> RdrName -> DerivStuff
    genDataDataCon dc constr_name       --  $cT1 etc
      = DerivHsBind (mkHsVarBind loc constr_name rhs,
                     L loc (TypeSig [L loc constr_name] sig_ty))
      where
        sig_ty   = mkLHsSigWcType (nlHsTyVar constr_RDR)
        rhs      = nlHsApps mkConstr_RDR constr_args

        constr_args
           = [ -- nlHsIntLit (toInteger (dataConTag dc)),   -- Tag
               nlHsVar (data_type_name)                     -- DataType
             , nlHsLit (mkHsString (occNameString dc_occ))  -- String name
             , nlList  labels                               -- Field labels
             , nlHsVar fixity ]                             -- Fixity

        labels   = map (nlHsLit . mkHsString . unpackFS . flLabel)
                       (dataConFieldLabels dc)
        dc_occ   = getOccName dc
        is_infix = isDataSymOcc dc_occ
        fixity | is_infix  = infix_RDR
               | otherwise = prefix_RDR

        ------------ gfoldl
    gfoldl_bind = mk_HRFunBind 2 loc gfoldl_RDR (map gfoldl_eqn data_cons)

    gfoldl_eqn con
      = ([nlVarPat k_RDR, nlVarPat z_RDR, nlConVarPat con_name as_needed],
                       foldl mk_k_app (nlHsVar z_RDR `nlHsApp` nlHsVar con_name) as_needed)
                   where
                     con_name ::  RdrName
                     con_name = getRdrName con
                     as_needed = take (dataConSourceArity con) as_RDRs
                     mk_k_app e v = nlHsPar (nlHsOpApp e k_RDR (nlHsVar v))

        ------------ gunfold
    gunfold_bind = mk_HRFunBind 2 loc
                     gunfold_RDR
                     [([k_Pat, z_Pat, if one_constr then nlWildPat else c_Pat],
                       gunfold_rhs)]

    gunfold_rhs
        | one_constr = mk_unfold_rhs (head data_cons)   -- No need for case
        | otherwise  = nlHsCase (nlHsVar conIndex_RDR `nlHsApp` c_Expr)
                                (map gunfold_alt data_cons)

    gunfold_alt dc = mkHsCaseAlt (mk_unfold_pat dc) (mk_unfold_rhs dc)
    mk_unfold_rhs dc = foldr nlHsApp
                           (nlHsVar z_RDR `nlHsApp` nlHsVar (getRdrName dc))
                           (replicate (dataConSourceArity dc) (nlHsVar k_RDR))

    mk_unfold_pat dc    -- Last one is a wild-pat, to avoid
                        -- redundant test, and annoying warning
      | tag-fIRST_TAG == n_cons-1 = nlWildPat   -- Last constructor
      | otherwise = nlConPat intDataCon_RDR
                             [nlLitPat (HsIntPrim "" (toInteger tag))]
      where
        tag = dataConTag dc

        ------------ toConstr
    toCon_bind = mk_FunBind loc toConstr_RDR (zipWith to_con_eqn data_cons constr_names)
    to_con_eqn dc con_name = ([nlWildConPat dc], nlHsVar con_name)

        ------------ dataTypeOf
    dataTypeOf_bind = mk_easy_FunBind
                        loc
                        dataTypeOf_RDR
                        [nlWildPat]
                        (nlHsVar data_type_name)

        ------------ gcast1/2
        -- Make the binding    dataCast1 x = gcast1 x  -- if T :: * -> *
        --               or    dataCast2 x = gcast2 s  -- if T :: * -> * -> *
        -- (or nothing if T has neither of these two types)

        -- But care is needed for data families:
        -- If we have   data family D a
        --              data instance D (a,b,c) = A | B deriving( Data )
        -- and we want  instance ... => Data (D [(a,b,c)]) where ...
        -- then we need     dataCast1 x = gcast1 x
        -- because D :: * -> *
        -- even though rep_tc has kind * -> * -> * -> *
        -- Hence looking for the kind of fam_tc not rep_tc
        -- See Trac #4896
    tycon_kind = case tyConFamInst_maybe rep_tc of
                    Just (fam_tc, _) -> tyConKind fam_tc
                    Nothing          -> tyConKind rep_tc
    gcast_binds | tycon_kind `tcEqKind` kind1 = mk_gcast dataCast1_RDR gcast1_RDR
                | tycon_kind `tcEqKind` kind2 = mk_gcast dataCast2_RDR gcast2_RDR
                | otherwise                 = emptyBag
    mk_gcast dataCast_RDR gcast_RDR
      = unitBag (mk_easy_FunBind loc dataCast_RDR [nlVarPat f_RDR]
                                 (nlHsVar gcast_RDR `nlHsApp` nlHsVar f_RDR))


kind1, kind2 :: Kind
kind1 = liftedTypeKind `mkFunTy` liftedTypeKind
kind2 = liftedTypeKind `mkFunTy` kind1

gfoldl_RDR, gunfold_RDR, toConstr_RDR, dataTypeOf_RDR, mkConstr_RDR,
    mkDataType_RDR, conIndex_RDR, prefix_RDR, infix_RDR,
    dataCast1_RDR, dataCast2_RDR, gcast1_RDR, gcast2_RDR,
    constr_RDR, dataType_RDR,
    eqChar_RDR  , ltChar_RDR  , geChar_RDR  , gtChar_RDR  , leChar_RDR  ,
    eqInt_RDR   , ltInt_RDR   , geInt_RDR   , gtInt_RDR   , leInt_RDR   ,
    eqWord_RDR  , ltWord_RDR  , geWord_RDR  , gtWord_RDR  , leWord_RDR  ,
    eqAddr_RDR  , ltAddr_RDR  , geAddr_RDR  , gtAddr_RDR  , leAddr_RDR  ,
    eqFloat_RDR , ltFloat_RDR , geFloat_RDR , gtFloat_RDR , leFloat_RDR ,
    eqDouble_RDR, ltDouble_RDR, geDouble_RDR, gtDouble_RDR, leDouble_RDR :: RdrName
gfoldl_RDR     = varQual_RDR  gENERICS (fsLit "gfoldl")
gunfold_RDR    = varQual_RDR  gENERICS (fsLit "gunfold")
toConstr_RDR   = varQual_RDR  gENERICS (fsLit "toConstr")
dataTypeOf_RDR = varQual_RDR  gENERICS (fsLit "dataTypeOf")
dataCast1_RDR  = varQual_RDR  gENERICS (fsLit "dataCast1")
dataCast2_RDR  = varQual_RDR  gENERICS (fsLit "dataCast2")
gcast1_RDR     = varQual_RDR  tYPEABLE (fsLit "gcast1")
gcast2_RDR     = varQual_RDR  tYPEABLE (fsLit "gcast2")
mkConstr_RDR   = varQual_RDR  gENERICS (fsLit "mkConstr")
constr_RDR     = tcQual_RDR   gENERICS (fsLit "Constr")
mkDataType_RDR = varQual_RDR  gENERICS (fsLit "mkDataType")
dataType_RDR   = tcQual_RDR   gENERICS (fsLit "DataType")
conIndex_RDR   = varQual_RDR  gENERICS (fsLit "constrIndex")
prefix_RDR     = dataQual_RDR gENERICS (fsLit "Prefix")
infix_RDR      = dataQual_RDR gENERICS (fsLit "Infix")

eqChar_RDR     = varQual_RDR  gHC_PRIM (fsLit "eqChar#")
ltChar_RDR     = varQual_RDR  gHC_PRIM (fsLit "ltChar#")
leChar_RDR     = varQual_RDR  gHC_PRIM (fsLit "leChar#")
gtChar_RDR     = varQual_RDR  gHC_PRIM (fsLit "gtChar#")
geChar_RDR     = varQual_RDR  gHC_PRIM (fsLit "geChar#")

eqInt_RDR      = varQual_RDR  gHC_PRIM (fsLit "==#")
ltInt_RDR      = varQual_RDR  gHC_PRIM (fsLit "<#" )
leInt_RDR      = varQual_RDR  gHC_PRIM (fsLit "<=#")
gtInt_RDR      = varQual_RDR  gHC_PRIM (fsLit ">#" )
geInt_RDR      = varQual_RDR  gHC_PRIM (fsLit ">=#")

eqWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "eqWord#")
ltWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "ltWord#")
leWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "leWord#")
gtWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "gtWord#")
geWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "geWord#")

eqAddr_RDR     = varQual_RDR  gHC_PRIM (fsLit "eqAddr#")
ltAddr_RDR     = varQual_RDR  gHC_PRIM (fsLit "ltAddr#")
leAddr_RDR     = varQual_RDR  gHC_PRIM (fsLit "leAddr#")
gtAddr_RDR     = varQual_RDR  gHC_PRIM (fsLit "gtAddr#")
geAddr_RDR     = varQual_RDR  gHC_PRIM (fsLit "geAddr#")

eqFloat_RDR    = varQual_RDR  gHC_PRIM (fsLit "eqFloat#")
ltFloat_RDR    = varQual_RDR  gHC_PRIM (fsLit "ltFloat#")
leFloat_RDR    = varQual_RDR  gHC_PRIM (fsLit "leFloat#")
gtFloat_RDR    = varQual_RDR  gHC_PRIM (fsLit "gtFloat#")
geFloat_RDR    = varQual_RDR  gHC_PRIM (fsLit "geFloat#")

eqDouble_RDR   = varQual_RDR  gHC_PRIM (fsLit "==##")
ltDouble_RDR   = varQual_RDR  gHC_PRIM (fsLit "<##" )
leDouble_RDR   = varQual_RDR  gHC_PRIM (fsLit "<=##")
gtDouble_RDR   = varQual_RDR  gHC_PRIM (fsLit ">##" )
geDouble_RDR   = varQual_RDR  gHC_PRIM (fsLit ">=##")

{-
************************************************************************
*                                                                      *
                        Functor instances

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html

*                                                                      *
************************************************************************

For the data type:

  data T a = T1 Int a | T2 (T a)

We generate the instance:

  instance Functor T where
      fmap f (T1 b1 a) = T1 b1 (f a)
      fmap f (T2 ta)   = T2 (fmap f ta)

Notice that we don't simply apply 'fmap' to the constructor arguments.
Rather
  - Do nothing to an argument whose type doesn't mention 'a'
  - Apply 'f' to an argument of type 'a'
  - Apply 'fmap f' to other arguments
That's why we have to recurse deeply into the constructor argument types,
rather than just one level, as we typically do.

What about types with more than one type parameter?  In general, we only
derive Functor for the last position:

  data S a b = S1 [b] | S2 (a, T a b)
  instance Functor (S a) where
    fmap f (S1 bs)    = S1 (fmap f bs)
    fmap f (S2 (p,q)) = S2 (a, fmap f q)

However, we have special cases for
         - tuples
         - functions

More formally, we write the derivation of fmap code over type variable
'a for type 'b as ($fmap 'a 'b).  In this general notation the derived
instance for T is:

  instance Functor T where
      fmap f (T1 x1 x2) = T1 ($(fmap 'a 'b1) x1) ($(fmap 'a 'a) x2)
      fmap f (T2 x1)    = T2 ($(fmap 'a '(T a)) x1)

  $(fmap 'a 'b)          =  \x -> x     -- when b does not contain a
  $(fmap 'a 'a)          =  f
  $(fmap 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> ($(fmap 'a 'b1) x1, $(fmap 'a 'b2) x2)
  $(fmap 'a '(T b1 b2))  =  fmap $(fmap 'a 'b2)   -- when a only occurs in the last parameter, b2
  $(fmap 'a '(b -> c))   =  \x b -> $(fmap 'a' 'c) (x ($(cofmap 'a 'b) b))

For functions, the type parameter 'a can occur in a contravariant position,
which means we need to derive a function like:

  cofmap :: (a -> b) -> (f b -> f a)

This is pretty much the same as $fmap, only without the $(cofmap 'a 'a) case:

  $(cofmap 'a 'b)          =  \x -> x     -- when b does not contain a
  $(cofmap 'a 'a)          =  error "type variable in contravariant position"
  $(cofmap 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> ($(cofmap 'a 'b1) x1, $(cofmap 'a 'b2) x2)
  $(cofmap 'a '[b])        =  map $(cofmap 'a 'b)
  $(cofmap 'a '(T b1 b2))  =  fmap $(cofmap 'a 'b2)   -- when a only occurs in the last parameter, b2
  $(cofmap 'a '(b -> c))   =  \x b -> $(cofmap 'a' 'c) (x ($(fmap 'a 'c) b))

Note that the code produced by $(fmap _ _) is always a higher order function,
with type `(a -> b) -> (g a -> g b)` for some g. When we need to do pattern
matching on the type, this means create a lambda function (see the (,) case above).
The resulting code for fmap can look a bit weird, for example:

  data X a = X (a,Int)
  -- generated instance
  instance Functor X where
      fmap f (X x) = (\y -> case y of (x1,x2) -> X (f x1, (\z -> z) x2)) x

The optimizer should be able to simplify this code by simple inlining.

An older version of the deriving code tried to avoid these applied
lambda functions by producing a meta level function. But the function to
be mapped, `f`, is a function on the code level, not on the meta level,
so it was eta expanded to `\x -> [| f $x |]`. This resulted in too much eta expansion.
It is better to produce too many lambdas than to eta expand, see ticket #7436.
-}

gen_Functor_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Functor_binds loc tycon
  = (unitBag fmap_bind, emptyBag)
  where
    data_cons = tyConDataCons tycon
    fun_name = L loc fmap_RDR
    fmap_bind = mkRdrFunBind fun_name eqns

    fmap_eqn con = evalState (match_for_con [f_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_fmap con

    eqns | null data_cons = [mkSimpleMatch (FunRhs fun_name Prefix)
                                           [nlWildPat, nlWildPat]
                                           (error_Expr "Void fmap")]
         | otherwise      = map fmap_eqn data_cons

    ft_fmap :: FFoldType (State [RdrName] (LHsExpr RdrName))
    ft_fmap = FT { ft_triv = mkSimpleLam $ \x -> return x
                   -- fmap f = \x -> x
                 , ft_var  = return f_Expr
                   -- fmap f = f
                 , ft_fun  = \g h -> do
                     gg <- g
                     hh <- h
                     mkSimpleLam2 $ \x b -> return $
                       nlHsApp hh (nlHsApp x (nlHsApp gg b))
                   -- fmap f = \x b -> h (x (g b))
                 , ft_tup = \t gs -> do
                     gg <- sequence gs
                     mkSimpleLam $ mkSimpleTupleCase match_for_con t gg
                   -- fmap f = \x -> case x of (a1,a2,..) -> (g1 a1,g2 a2,..)
                 , ft_ty_app = \_ g -> nlHsApp fmap_Expr <$> g
                   -- fmap f = fmap g
                 , ft_forall = \_ g -> g
                 , ft_bad_app = panic "in other argument"
                 , ft_co_var = panic "contravariant" }

    -- Con a1 a2 ... -> Con (f1 a1) (f2 a2) ...
    match_for_con :: [LPat RdrName] -> DataCon -> [LHsExpr RdrName]
                  -> State [RdrName] (LMatch RdrName (LHsExpr RdrName))
    match_for_con = mkSimpleConMatch CaseAlt $
        \con_name xs -> return $ nlHsApps con_name xs  -- Con x1 x2 ..

{-
Utility functions related to Functor deriving.

Since several things use the same pattern of traversal, this is abstracted into functorLikeTraverse.
This function works like a fold: it makes a value of type 'a' in a bottom up way.
-}

-- Generic traversal for Functor deriving
-- See Note [FFoldType and functorLikeTraverse]
data FFoldType a      -- Describes how to fold over a Type in a functor like way
   = FT { ft_triv    :: a
          -- ^ Does not contain variable
        , ft_var     :: a
          -- ^ The variable itself
        , ft_co_var  :: a
          -- ^ The variable itself, contravariantly
        , ft_fun     :: a -> a -> a
          -- ^ Function type
        , ft_tup     :: TyCon -> [a] -> a
          -- ^ Tuple type
        , ft_ty_app  :: Type -> a -> a
          -- ^ Type app, variable only in last argument
        , ft_bad_app :: a
          -- ^ Type app, variable other than in last argument
        , ft_forall  :: TcTyVar -> a -> a
          -- ^ Forall type
     }

functorLikeTraverse :: forall a.
                       TyVar         -- ^ Variable to look for
                    -> FFoldType a   -- ^ How to fold
                    -> Type          -- ^ Type to process
                    -> a
functorLikeTraverse var (FT { ft_triv = caseTrivial,     ft_var = caseVar
                            , ft_co_var = caseCoVar,     ft_fun = caseFun
                            , ft_tup = caseTuple,        ft_ty_app = caseTyApp
                            , ft_bad_app = caseWrongArg, ft_forall = caseForAll })
                    ty
  = fst (go False ty)
  where
    go :: Bool        -- Covariant or contravariant context
       -> Type
       -> (a, Bool)   -- (result of type a, does type contain var)

    go co ty | Just ty' <- coreView ty = go co ty'
    go co (TyVarTy    v) | v == var = (if co then caseCoVar else caseVar,True)
    go co (FunTy x y)  | isPredTy x = go co y
                       | xc || yc   = (caseFun xr yr,True)
        where (xr,xc) = go (not co) x
              (yr,yc) = go co       y
    go co (AppTy    x y) | xc = (caseWrongArg,   True)
                         | yc = (caseTyApp x yr, True)
        where (_, xc) = go co x
              (yr,yc) = go co y
    go co ty@(TyConApp con args)
       | not (or xcs)     = (caseTrivial, False)   -- Variable does not occur
       -- At this point we know that xrs, xcs is not empty,
       -- and at least one xr is True
       | isTupleTyCon con = (caseTuple con xrs, True)
       | or (init xcs)    = (caseWrongArg, True)         -- T (..var..)    ty
       | Just (fun_ty, _) <- splitAppTy_maybe ty         -- T (..no var..) ty
                          = (caseTyApp fun_ty (last xrs), True)
       | otherwise        = (caseWrongArg, True)   -- Non-decomposable (eg type function)
       where
         -- When folding over an unboxed tuple, we must explicitly drop the
         -- runtime rep arguments, or else GHC will generate twice as many
         -- variables in a unboxed tuple pattern match and expression as it
         -- actually needs. See Trac #12399
         (xrs,xcs) = unzip (map (go co) (dropRuntimeRepArgs args))
    go co (ForAllTy (TvBndr v vis) x)
       | isVisibleArgFlag vis = panic "unexpected visible binder"
       | v /= var && xc       = (caseForAll v xr,True)
       where (xr,xc) = go co x

    go _ _ = (caseTrivial,False)

-- Return all syntactic subterms of ty that contain var somewhere
-- These are the things that should appear in instance constraints
deepSubtypesContaining :: TyVar -> Type -> [TcType]
deepSubtypesContaining tv
  = functorLikeTraverse tv
        (FT { ft_triv = []
            , ft_var = []
            , ft_fun = (++)
            , ft_tup = \_ xs -> concat xs
            , ft_ty_app = (:)
            , ft_bad_app = panic "in other argument"
            , ft_co_var = panic "contravariant"
            , ft_forall = \v xs -> filterOut ((v `elemVarSet`) . tyCoVarsOfType) xs })


foldDataConArgs :: FFoldType a -> DataCon -> [a]
-- Fold over the arguments of the datacon
foldDataConArgs ft con
  = map foldArg (dataConOrigArgTys con)
  where
    foldArg
      = case getTyVar_maybe (last (tyConAppArgs (dataConOrigResTy con))) of
             Just tv -> functorLikeTraverse tv ft
             Nothing -> const (ft_triv ft)
    -- If we are deriving Foldable for a GADT, there is a chance that the last
    -- type variable in the data type isn't actually a type variable at all.
    -- (for example, this can happen if the last type variable is refined to
    -- be a concrete type such as Int). If the last type variable is refined
    -- to be a specific type, then getTyVar_maybe will return Nothing.
    -- See Note [DeriveFoldable with ExistentialQuantification]
    --
    -- The kind checks have ensured the last type parameter is of kind *.

-- Make a HsLam using a fresh variable from a State monad
mkSimpleLam :: (LHsExpr RdrName -> State [RdrName] (LHsExpr RdrName))
            -> State [RdrName] (LHsExpr RdrName)
-- (mkSimpleLam fn) returns (\x. fn(x))
mkSimpleLam lam = do
    (n:names) <- get
    put names
    body <- lam (nlHsVar n)
    return (mkHsLam [nlVarPat n] body)

mkSimpleLam2 :: (LHsExpr RdrName -> LHsExpr RdrName
             -> State [RdrName] (LHsExpr RdrName))
             -> State [RdrName] (LHsExpr RdrName)
mkSimpleLam2 lam = do
    (n1:n2:names) <- get
    put names
    body <- lam (nlHsVar n1) (nlHsVar n2)
    return (mkHsLam [nlVarPat n1,nlVarPat n2] body)

-- "Con a1 a2 a3 -> fold [x1 a1, x2 a2, x3 a3]"
--
-- @mkSimpleConMatch fold extra_pats con insides@ produces a match clause in
-- which the LHS pattern-matches on @extra_pats@, followed by a match on the
-- constructor @con@ and its arguments. The RHS folds (with @fold@) over @con@
-- and its arguments, applying an expression (from @insides@) to each of the
-- respective arguments of @con@.
mkSimpleConMatch :: Monad m => HsMatchContext RdrName
                 -> (RdrName -> [LHsExpr RdrName] -> m (LHsExpr RdrName))
                 -> [LPat RdrName]
                 -> DataCon
                 -> [LHsExpr RdrName]
                 -> m (LMatch RdrName (LHsExpr RdrName))
mkSimpleConMatch ctxt fold extra_pats con insides = do
    let con_name = getRdrName con
    let vars_needed = takeList insides as_RDRs
    let pat = nlConVarPat con_name vars_needed
    rhs <- fold con_name (zipWith nlHsApp insides (map nlHsVar vars_needed))
    return $ mkMatch ctxt (extra_pats ++ [pat]) rhs
                     (noLoc emptyLocalBinds)

-- "Con a1 a2 a3 -> fmap (\b2 -> Con a1 b2 a3) (traverse f a2)"
--
-- @mkSimpleConMatch2 fold extra_pats con insides@ behaves very similarly to
-- 'mkSimpleConMatch', with two key differences:
--
-- 1. @insides@ is a @[Maybe (LHsExpr RdrName)]@ instead of a
--    @[LHsExpr RdrName]@. This is because it filters out the expressions
--    corresponding to arguments whose types do not mention the last type
--    variable in a derived 'Foldable' or 'Traversable' instance (i.e., the
--    'Nothing' elements of @insides@).
--
-- 2. @fold@ takes an expression as its first argument instead of a
--    constructor name. This is because it uses a specialized
--    constructor function expression that only takes as many parameters as
--    there are argument types that mention the last type variable.
--
-- See Note [Generated code for DeriveFoldable and DeriveTraversable]
mkSimpleConMatch2 :: Monad m
                  => HsMatchContext RdrName
                  -> (LHsExpr RdrName -> [LHsExpr RdrName]
                                      -> m (LHsExpr RdrName))
                  -> [LPat RdrName]
                  -> DataCon
                  -> [Maybe (LHsExpr RdrName)]
                  -> m (LMatch RdrName (LHsExpr RdrName))
mkSimpleConMatch2 ctxt fold extra_pats con insides = do
    let con_name = getRdrName con
        vars_needed = takeList insides as_RDRs
        pat = nlConVarPat con_name vars_needed
        -- Make sure to zip BEFORE invoking catMaybes. We want the variable
        -- indicies in each expression to match up with the argument indices
        -- in con_expr (defined below).
        exps = catMaybes $ zipWith (\i v -> (`nlHsApp` v) <$> i)
                                   insides (map nlHsVar vars_needed)
        -- An element of argTysTyVarInfo is True if the constructor argument
        -- with the same index has a type which mentions the last type
        -- variable.
        argTysTyVarInfo = map isJust insides
        (asWithTyVar, asWithoutTyVar) = partitionByList argTysTyVarInfo as_RDRs

        con_expr
          | null asWithTyVar = nlHsApps con_name $ map nlHsVar asWithoutTyVar
          | otherwise =
              let bs   = filterByList  argTysTyVarInfo bs_RDRs
                  vars = filterByLists argTysTyVarInfo
                                       (map nlHsVar bs_RDRs)
                                       (map nlHsVar as_RDRs)
              in mkHsLam (map nlVarPat bs) (nlHsApps con_name vars)

    rhs <- fold con_expr exps
    return $ mkMatch ctxt (extra_pats ++ [pat]) rhs
                     (noLoc emptyLocalBinds)

-- "case x of (a1,a2,a3) -> fold [x1 a1, x2 a2, x3 a3]"
mkSimpleTupleCase :: Monad m => ([LPat RdrName] -> DataCon -> [a]
                                 -> m (LMatch RdrName (LHsExpr RdrName)))
                  -> TyCon -> [a] -> LHsExpr RdrName -> m (LHsExpr RdrName)
mkSimpleTupleCase match_for_con tc insides x
  = do { let data_con = tyConSingleDataCon tc
       ; match <- match_for_con [] data_con insides
       ; return $ nlHsCase x [match] }

{-
************************************************************************
*                                                                      *
                        Foldable instances

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html

*                                                                      *
************************************************************************

Deriving Foldable instances works the same way as Functor instances,
only Foldable instances are not possible for function types at all.
Given (data T a = T a a (T a) deriving Foldable), we get:

  instance Foldable T where
      foldr f z (T x1 x2 x3) =
        $(foldr 'a 'a) x1 ( $(foldr 'a 'a) x2 ( $(foldr 'a '(T a)) x3 z ) )

-XDeriveFoldable is different from -XDeriveFunctor in that it filters out
arguments to the constructor that would produce useless code in a Foldable
instance. For example, the following datatype:

  data Foo a = Foo Int a Int deriving Foldable

would have the following generated Foldable instance:

  instance Foldable Foo where
    foldr f z (Foo x1 x2 x3) = $(foldr 'a 'a) x2

since neither of the two Int arguments are folded over.

The cases are:

  $(foldr 'a 'a)         =  f
  $(foldr 'a '(b1,b2))   =  \x z -> case x of (x1,x2) -> $(foldr 'a 'b1) x1 ( $(foldr 'a 'b2) x2 z )
  $(foldr 'a '(T b1 b2)) =  \x z -> foldr $(foldr 'a 'b2) z x  -- when a only occurs in the last parameter, b2

Note that the arguments to the real foldr function are the wrong way around,
since (f :: a -> b -> b), while (foldr f :: b -> t a -> b).

One can envision a case for types that don't contain the last type variable:

  $(foldr 'a 'b)         =  \x z -> z     -- when b does not contain a

But this case will never materialize, since the aforementioned filtering
removes all such types from consideration.
See Note [Generated code for DeriveFoldable and DeriveTraversable].

Foldable instances differ from Functor and Traversable instances in that
Foldable instances can be derived for data types in which the last type
variable is existentially quantified. In particular, if the last type variable
is refined to a more specific type in a GADT:

  data GADT a where
      G :: a ~ Int => a -> G Int

then the deriving machinery does not attempt to check that the type a contains
Int, since it is not syntactically equal to a type variable. That is, the
derived Foldable instance for GADT is:

  instance Foldable GADT where
      foldr _ z (GADT _) = z

See Note [DeriveFoldable with ExistentialQuantification].

-}

gen_Foldable_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Foldable_binds loc tycon
  = (listToBag [foldr_bind, foldMap_bind], emptyBag)
  where
    data_cons = tyConDataCons tycon

    foldr_bind = mkRdrFunBind (L loc foldable_foldr_RDR) eqns
    eqns = map foldr_eqn data_cons
    foldr_eqn con
      = evalState (match_foldr z_Expr [f_Pat,z_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_foldr con

    foldMap_bind = mkRdrFunBind (L loc foldMap_RDR) (map foldMap_eqn data_cons)
    foldMap_eqn con
      = evalState (match_foldMap [f_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_foldMap con

    -- Yields 'Just' an expression if we're folding over a type that mentions
    -- the last type parameter of the datatype. Otherwise, yields 'Nothing'.
    -- See Note [FFoldType and functorLikeTraverse]
    ft_foldr :: FFoldType (State [RdrName] (Maybe (LHsExpr RdrName)))
    ft_foldr
      = FT { ft_triv    = return Nothing
             -- foldr f = \x z -> z
           , ft_var     = return $ Just f_Expr
             -- foldr f = f
           , ft_tup     = \t g -> do
               gg  <- sequence g
               lam <- mkSimpleLam2 $ \x z ->
                 mkSimpleTupleCase (match_foldr z) t gg x
               return (Just lam)
             -- foldr f = (\x z -> case x of ...)
           , ft_ty_app  = \_ g -> do
               gg <- g
               mapM (\gg' -> mkSimpleLam2 $ \x z -> return $
                 nlHsApps foldable_foldr_RDR [gg',z,x]) gg
             -- foldr f = (\x z -> foldr g z x)
           , ft_forall  = \_ g -> g
           , ft_co_var  = panic "contravariant"
           , ft_fun     = panic "function"
           , ft_bad_app = panic "in other argument" }

    match_foldr :: LHsExpr RdrName
                -> [LPat RdrName]
                -> DataCon
                -> [Maybe (LHsExpr RdrName)]
                -> State [RdrName] (LMatch RdrName (LHsExpr RdrName))
    match_foldr z = mkSimpleConMatch2 LambdaExpr $ \_ xs -> return (mkFoldr xs)
      where
        -- g1 v1 (g2 v2 (.. z))
        mkFoldr :: [LHsExpr RdrName] -> LHsExpr RdrName
        mkFoldr = foldr nlHsApp z

    -- See Note [FFoldType and functorLikeTraverse]
    ft_foldMap :: FFoldType (State [RdrName] (Maybe (LHsExpr RdrName)))
    ft_foldMap
      = FT { ft_triv = return Nothing
             -- foldMap f = \x -> mempty
           , ft_var  = return (Just f_Expr)
             -- foldMap f = f
           , ft_tup  = \t g -> do
               gg  <- sequence g
               lam <- mkSimpleLam $ mkSimpleTupleCase match_foldMap t gg
               return (Just lam)
             -- foldMap f = \x -> case x of (..,)
           , ft_ty_app = \_ g -> fmap (nlHsApp foldMap_Expr) <$> g
             -- foldMap f = foldMap g
           , ft_forall = \_ g -> g
           , ft_co_var = panic "contravariant"
           , ft_fun = panic "function"
           , ft_bad_app = panic "in other argument" }

    match_foldMap :: [LPat RdrName]
                  -> DataCon
                  -> [Maybe (LHsExpr RdrName)]
                  -> State [RdrName] (LMatch RdrName (LHsExpr RdrName))
    match_foldMap = mkSimpleConMatch2 CaseAlt $ \_ xs -> return (mkFoldMap xs)
      where
        -- mappend v1 (mappend v2 ..)
        mkFoldMap :: [LHsExpr RdrName] -> LHsExpr RdrName
        mkFoldMap [] = mempty_Expr
        mkFoldMap xs = foldr1 (\x y -> nlHsApps mappend_RDR [x,y]) xs

{-
************************************************************************
*                                                                      *
                        Traversable instances

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html
*                                                                      *
************************************************************************

Again, Traversable is much like Functor and Foldable.

The cases are:

  $(traverse 'a 'a)          =  f
  $(traverse 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> (,) <$> $(traverse 'a 'b1) x1 <*> $(traverse 'a 'b2) x2
  $(traverse 'a '(T b1 b2))  =  traverse $(traverse 'a 'b2)  -- when a only occurs in the last parameter, b2

Like -XDeriveFoldable, -XDeriveTraversable filters out arguments whose types
do not mention the last type parameter. Therefore, the following datatype:

  data Foo a = Foo Int a Int

would have the following derived Traversable instance:

  instance Traversable Foo where
    traverse f (Foo x1 x2 x3) =
      fmap (\b2 -> Foo x1 b2 x3) ( $(traverse 'a 'a) x2 )

since the two Int arguments do not produce any effects in a traversal.

One can envision a case for types that do not mention the last type parameter:

  $(traverse 'a 'b)          =  pure     -- when b does not contain a

But this case will never materialize, since the aforementioned filtering
removes all such types from consideration.
See Note [Generated code for DeriveFoldable and DeriveTraversable].
-}

gen_Traversable_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Traversable_binds loc tycon
  = (unitBag traverse_bind, emptyBag)
  where
    data_cons = tyConDataCons tycon

    traverse_bind = mkRdrFunBind (L loc traverse_RDR) eqns
    eqns = map traverse_eqn data_cons
    traverse_eqn con
      = evalState (match_for_con [f_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_trav con

    -- Yields 'Just' an expression if we're folding over a type that mentions
    -- the last type parameter of the datatype. Otherwise, yields 'Nothing'.
    -- See Note [FFoldType and functorLikeTraverse]
    ft_trav :: FFoldType (State [RdrName] (Maybe (LHsExpr RdrName)))
    ft_trav
      = FT { ft_triv    = return Nothing
             -- traverse f = pure x
           , ft_var     = return (Just f_Expr)
             -- traverse f = f x
           , ft_tup     = \t gs -> do
               gg  <- sequence gs
               lam <- mkSimpleLam $ mkSimpleTupleCase match_for_con t gg
               return (Just lam)
             -- traverse f = \x -> case x of (a1,a2,..) ->
             --                           (,,) <$> g1 a1 <*> g2 a2 <*> ..
           , ft_ty_app  = \_ g -> fmap (nlHsApp traverse_Expr) <$> g
             -- traverse f = traverse g
           , ft_forall  = \_ g -> g
           , ft_co_var  = panic "contravariant"
           , ft_fun     = panic "function"
           , ft_bad_app = panic "in other argument" }

    -- Con a1 a2 ... -> fmap (\b1 b2 ... -> Con b1 b2 ...) (g1 a1)
    --                    <*> g2 a2 <*> ...
    match_for_con :: [LPat RdrName]
                  -> DataCon
                  -> [Maybe (LHsExpr RdrName)]
                  -> State [RdrName] (LMatch RdrName (LHsExpr RdrName))
    match_for_con = mkSimpleConMatch2 CaseAlt $
                                             \con xs -> return (mkApCon con xs)
      where
        -- fmap (\b1 b2 ... -> Con b1 b2 ...) x1 <*> x2 <*> ..
        mkApCon :: LHsExpr RdrName -> [LHsExpr RdrName] -> LHsExpr RdrName
        mkApCon con [] = nlHsApps pure_RDR [con]
        mkApCon con (x:xs) = foldl appAp (nlHsApps fmap_RDR [con,x]) xs
          where appAp x y = nlHsApps ap_RDR [x,y]

{-
************************************************************************
*                                                                      *
                        Lift instances
*                                                                      *
************************************************************************

Example:

    data Foo a = Foo a | a :^: a deriving Lift

    ==>

    instance (Lift a) => Lift (Foo a) where
        lift (Foo a)
          = appE
              (conE
                (mkNameG_d "package-name" "ModuleName" "Foo"))
              (lift a)
        lift (u :^: v)
          = infixApp
              (lift u)
              (conE
                (mkNameG_d "package-name" "ModuleName" ":^:"))
              (lift v)

Note that (mkNameG_d "package-name" "ModuleName" "Foo") is equivalent to what
'Foo would be when using the -XTemplateHaskell extension. To make sure that
-XDeriveLift can be used on stage-1 compilers, however, we expliticly invoke
makeG_d.
-}

gen_Lift_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Lift_binds loc tycon
  | null data_cons = (unitBag (L loc $ mkFunBind (L loc lift_RDR)
                       [mkMatch (FunRhs (L loc lift_RDR) Prefix)
                                        [nlWildPat] errorMsg_Expr
                                        (noLoc emptyLocalBinds)])
                     , emptyBag)
  | otherwise = (unitBag lift_bind, emptyBag)
  where
    errorMsg_Expr = nlHsVar error_RDR `nlHsApp` nlHsLit
        (mkHsString $ "Can't lift value of empty datatype " ++ tycon_str)

    lift_bind = mk_FunBind loc lift_RDR (map pats_etc data_cons)
    data_cons = tyConDataCons tycon
    tycon_str = occNameString . nameOccName . tyConName $ tycon

    pats_etc data_con
      = ([con_pat], lift_Expr)
       where
            con_pat      = nlConVarPat data_con_RDR as_needed
            data_con_RDR = getRdrName data_con
            con_arity    = dataConSourceArity data_con
            as_needed    = take con_arity as_RDRs
            lifted_as    = zipWithEqual "mk_lift_app" mk_lift_app
                             tys_needed as_needed
            tycon_name   = tyConName tycon
            is_infix     = dataConIsInfix data_con
            tys_needed   = dataConOrigArgTys data_con

            mk_lift_app ty a
              | not (isUnliftedType ty) = nlHsApp (nlHsVar lift_RDR)
                                                  (nlHsVar a)
              | otherwise = nlHsApp (nlHsVar litE_RDR)
                              (primLitOp (mkBoxExp (nlHsVar a)))
              where (primLitOp, mkBoxExp) = primLitOps "Lift" tycon ty

            pkg_name = unitIdString . moduleUnitId
                     . nameModule $ tycon_name
            mod_name = moduleNameString . moduleName . nameModule $ tycon_name
            con_name = occNameString . nameOccName . dataConName $ data_con

            conE_Expr = nlHsApp (nlHsVar conE_RDR)
                                (nlHsApps mkNameG_dRDR
                                  (map (nlHsLit . mkHsString)
                                    [pkg_name, mod_name, con_name]))

            lift_Expr
              | is_infix  = nlHsApps infixApp_RDR [a1, conE_Expr, a2]
              | otherwise = foldl mk_appE_app conE_Expr lifted_as
            (a1:a2:_) = lifted_as

mk_appE_app :: LHsExpr RdrName -> LHsExpr RdrName -> LHsExpr RdrName
mk_appE_app a b = nlHsApps appE_RDR [a, b]

{-
************************************************************************
*                                                                      *
                     Newtype-deriving instances
*                                                                      *
************************************************************************

Note [Newtype-deriving instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We take every method in the original instance and `coerce` it to fit
into the derived instance. We need a type annotation on the argument
to `coerce` to make it obvious what instantiation of the method we're
coercing from.  So from, say,
  class C a b where
    op :: a -> [b] -> Int

  newtype T x = MkT <rep-ty>

  instance C a <rep-ty> => C a (T x) where
    op = (coerce
             (op :: a -> [<rep-ty>] -> Int)
         ) :: a -> [T x] -> Int

Notice that we give the 'coerce' call two type signatures: one to
fix the type of the inner call, and one for the expected type.  The outer
type signature ought to be redundant, but may improve error messages.
The inner one is essential to fix the type at which 'op' is called.

See #8503 for more discussion.

Here's a wrinkle. Supppose 'op' is locally overloaded:

  class C2 b where
    op2 :: forall a. Eq a => a -> [b] -> Int

Then we could do exactly as above, but it's a bit redundant to
instantiate op, then re-generalise with the inner signature.
(The inner sig is only there to fix the type at which 'op' is
called.)  So we just instantiate the signature, and add

  instance C2 <rep-ty> => C2 (T x) where
    op2 = (coerce
             (op2 :: a -> [<rep-ty>] -> Int)
          ) :: forall a. Eq a => a -> [T x] -> Int
-}

gen_Newtype_binds :: SrcSpan
                  -> Class   -- the class being derived
                  -> [TyVar] -- the tvs in the instance head
                  -> [Type]  -- instance head parameters (incl. newtype)
                  -> Type    -- the representation type (already eta-reduced)
                  -> LHsBinds RdrName
-- See Note [Newtype-deriving instances]
gen_Newtype_binds loc cls inst_tvs cls_tys rhs_ty
  = listToBag $ map mk_bind (classMethods cls)
  where
    coerce_RDR = getRdrName coerceId

    mk_bind :: Id -> LHsBind RdrName
    mk_bind meth_id
      = mkRdrFunBind (L loc meth_RDR) [mkSimpleMatch
                                         (FunRhs (L loc meth_RDR) Prefix)
                                         [] rhs_expr]
      where
        Pair from_ty to_ty = mkCoerceClassMethEqn cls inst_tvs cls_tys rhs_ty meth_id

        -- See "wrinkle" in Note [Newtype-deriving instances]
        (_, _, from_ty') = tcSplitSigmaTy from_ty

        meth_RDR = getRdrName meth_id

        rhs_expr = ( nlHsVar coerce_RDR
                      `nlHsApp`
                    (nlHsVar meth_RDR `nlExprWithTySig` toLHsSigWcType from_ty'))
                  `nlExprWithTySig` toLHsSigWcType to_ty


nlExprWithTySig :: LHsExpr RdrName -> LHsSigWcType RdrName -> LHsExpr RdrName
nlExprWithTySig e s = noLoc (ExprWithTySig e s)

mkCoerceClassMethEqn :: Class   -- the class being derived
                     -> [TyVar] -- the tvs in the instance head
                     -> [Type]  -- instance head parameters (incl. newtype)
                     -> Type    -- the representation type (already eta-reduced)
                     -> Id      -- the method to look at
                     -> Pair Type
-- See Note [Newtype-deriving instances]
-- The pair is the (from_type, to_type), where to_type is
-- the type of the method we are tyrying to get
mkCoerceClassMethEqn cls inst_tvs cls_tys rhs_ty id
  = Pair (substTy rhs_subst user_meth_ty)
         (substTy lhs_subst user_meth_ty)
  where
    cls_tvs = classTyVars cls
    in_scope = mkInScopeSet $ mkVarSet inst_tvs
    lhs_subst = mkTvSubst in_scope (zipTyEnv cls_tvs cls_tys)
    rhs_subst = mkTvSubst in_scope (zipTyEnv cls_tvs (changeLast cls_tys rhs_ty))
    (_class_tvs, _class_constraint, user_meth_ty)
      = tcSplitMethodTy (varType id)

    changeLast :: [a] -> a -> [a]
    changeLast []     _  = panic "changeLast"
    changeLast [_]    x  = [x]
    changeLast (x:xs) x' = x : changeLast xs x'

{-
************************************************************************
*                                                                      *
\subsection{Generating extra binds (@con2tag@ and @tag2con@)}
*                                                                      *
************************************************************************

\begin{verbatim}
data Foo ... = ...

con2tag_Foo :: Foo ... -> Int#
tag2con_Foo :: Int -> Foo ...   -- easier if Int, not Int#
maxtag_Foo  :: Int              -- ditto (NB: not unlifted)
\end{verbatim}

The `tags' here start at zero, hence the @fIRST_TAG@ (currently one)
fiddling around.
-}

genAuxBindSpec :: SrcSpan -> AuxBindSpec -> (LHsBind RdrName, LSig RdrName)
genAuxBindSpec loc (DerivCon2Tag tycon)
  = (mk_FunBind loc rdr_name eqns,
     L loc (TypeSig [L loc rdr_name] sig_ty))
  where
    rdr_name = con2tag_RDR tycon

    sig_ty = mkLHsSigWcType $ L loc $ HsCoreTy $
             mkSpecSigmaTy (tyConTyVars tycon) (tyConStupidTheta tycon) $
             mkParentType tycon `mkFunTy` intPrimTy

    lots_of_constructors = tyConFamilySize tycon > 8
                        -- was: mAX_FAMILY_SIZE_FOR_VEC_RETURNS
                        -- but we don't do vectored returns any more.

    eqns | lots_of_constructors = [get_tag_eqn]
         | otherwise = map mk_eqn (tyConDataCons tycon)

    get_tag_eqn = ([nlVarPat a_RDR], nlHsApp (nlHsVar getTag_RDR) a_Expr)

    mk_eqn :: DataCon -> ([LPat RdrName], LHsExpr RdrName)
    mk_eqn con = ([nlWildConPat con],
                  nlHsLit (HsIntPrim ""
                                    (toInteger ((dataConTag con) - fIRST_TAG))))

genAuxBindSpec loc (DerivTag2Con tycon)
  = (mk_FunBind loc rdr_name
        [([nlConVarPat intDataCon_RDR [a_RDR]],
           nlHsApp (nlHsVar tagToEnum_RDR) a_Expr)],
     L loc (TypeSig [L loc rdr_name] sig_ty))
  where
    sig_ty = mkLHsSigWcType $ L loc $
             HsCoreTy $ mkSpecForAllTys (tyConTyVars tycon) $
             intTy `mkFunTy` mkParentType tycon

    rdr_name = tag2con_RDR tycon

genAuxBindSpec loc (DerivMaxTag tycon)
  = (mkHsVarBind loc rdr_name rhs,
     L loc (TypeSig [L loc rdr_name] sig_ty))
  where
    rdr_name = maxtag_RDR tycon
    sig_ty = mkLHsSigWcType (L loc (HsCoreTy intTy))
    rhs = nlHsApp (nlHsVar intDataCon_RDR) (nlHsLit (HsIntPrim "" max_tag))
    max_tag =  case (tyConDataCons tycon) of
                 data_cons -> toInteger ((length data_cons) - fIRST_TAG)

type SeparateBagsDerivStuff =
  -- AuxBinds and SYB bindings
  ( Bag (LHsBind RdrName, LSig RdrName)
  -- Extra family instances (used by Generic and DeriveAnyClass)
  , Bag (FamInst) )

genAuxBinds :: SrcSpan -> BagDerivStuff -> SeparateBagsDerivStuff
genAuxBinds loc b = genAuxBinds' b2 where
  (b1,b2) = partitionBagWith splitDerivAuxBind b
  splitDerivAuxBind (DerivAuxBind x) = Left x
  splitDerivAuxBind  x               = Right x

  rm_dups = foldrBag dup_check emptyBag
  dup_check a b = if anyBag (== a) b then b else consBag a b

  genAuxBinds' :: BagDerivStuff -> SeparateBagsDerivStuff
  genAuxBinds' = foldrBag f ( mapBag (genAuxBindSpec loc) (rm_dups b1)
                            , emptyBag )
  f :: DerivStuff -> SeparateBagsDerivStuff -> SeparateBagsDerivStuff
  f (DerivAuxBind _) = panic "genAuxBinds'" -- We have removed these before
  f (DerivHsBind  b) = add1 b
  f (DerivFamInst t) = add2 t

  add1 x (a,b) = (x `consBag` a,b)
  add2 x (a,b) = (a,x `consBag` b)

mkParentType :: TyCon -> Type
-- Turn the representation tycon of a family into
-- a use of its family constructor
mkParentType tc
  = case tyConFamInst_maybe tc of
       Nothing  -> mkTyConApp tc (mkTyVarTys (tyConTyVars tc))
       Just (fam_tc,tys) -> mkTyConApp fam_tc tys

{-
************************************************************************
*                                                                      *
\subsection{Utility bits for generating bindings}
*                                                                      *
************************************************************************
-}

mk_FunBind :: SrcSpan -> RdrName
           -> [([LPat RdrName], LHsExpr RdrName)]
           -> LHsBind RdrName
mk_FunBind = mk_HRFunBind 0   -- by using mk_FunBind and not mk_HRFunBind,
                              -- the caller says that the Void case needs no
                              -- patterns

-- | This variant of 'mk_FunBind' puts an 'Arity' number of wildcards before
-- the "=" in the empty-data-decl case. This is necessary if the function
-- has a higher-rank type, like foldl. (See deriving/should_compile/T4302)
mk_HRFunBind :: Arity -> SrcSpan -> RdrName
             -> [([LPat RdrName], LHsExpr RdrName)]
             -> LHsBind RdrName
mk_HRFunBind arity loc fun pats_and_exprs
  = mkHRRdrFunBind arity (L loc fun) matches
  where
    matches = [mkMatch (FunRhs (L loc fun) Prefix) p e
                               (noLoc emptyLocalBinds)
              | (p,e) <-pats_and_exprs]

mkRdrFunBind :: Located RdrName -> [LMatch RdrName (LHsExpr RdrName)] -> LHsBind RdrName
mkRdrFunBind = mkHRRdrFunBind 0

mkHRRdrFunBind :: Arity -> Located RdrName -> [LMatch RdrName (LHsExpr RdrName)] -> LHsBind RdrName
mkHRRdrFunBind arity fun@(L loc fun_rdr) matches = L loc (mkFunBind fun matches')
 where
   -- Catch-all eqn looks like
   --     fmap = error "Void fmap"
   -- It's needed if there no data cons at all,
   -- which can happen with -XEmptyDataDecls
   -- See Trac #4302
   matches' = if null matches
              then [mkMatch (FunRhs fun Prefix)
                            (replicate arity nlWildPat)
                            (error_Expr str) (noLoc emptyLocalBinds)]
              else matches
   str = "Void " ++ occNameString (rdrNameOcc fun_rdr)

box ::         String           -- The class involved
            -> TyCon            -- The tycon involved
            -> LHsExpr RdrName  -- The argument
            -> Type             -- The argument type
            -> LHsExpr RdrName  -- Boxed version of the arg
-- See Note [Deriving and unboxed types] in TcDeriv
box cls_str tycon arg arg_ty = nlHsApp (nlHsVar box_con) arg
  where
    box_con = assoc_ty_id cls_str tycon boxConTbl arg_ty

---------------------
primOrdOps :: String    -- The class involved
           -> TyCon     -- The tycon involved
           -> Type      -- The type
           -> (RdrName, RdrName, RdrName, RdrName, RdrName)  -- (lt,le,eq,ge,gt)
-- See Note [Deriving and unboxed types] in TcDeriv
primOrdOps str tycon ty = assoc_ty_id str tycon ordOpTbl ty

primLitOps :: String -- The class involved
           -> TyCon  -- The tycon involved
           -> Type   -- The type
           -> ( LHsExpr RdrName -> LHsExpr RdrName -- Constructs a Q Exp value
              , LHsExpr RdrName -> LHsExpr RdrName -- Constructs a boxed value
              )
primLitOps str tycon ty = ( assoc_ty_id str tycon litConTbl ty
                          , \v -> nlHsVar boxRDR `nlHsApp` v
                          )
  where
    boxRDR
      | ty `eqType` addrPrimTy = unpackCString_RDR
      | otherwise = assoc_ty_id str tycon boxConTbl ty

ordOpTbl :: [(Type, (RdrName, RdrName, RdrName, RdrName, RdrName))]
ordOpTbl
 =  [(charPrimTy  , (ltChar_RDR  , leChar_RDR  , eqChar_RDR  , geChar_RDR  , gtChar_RDR  ))
    ,(intPrimTy   , (ltInt_RDR   , leInt_RDR   , eqInt_RDR   , geInt_RDR   , gtInt_RDR   ))
    ,(wordPrimTy  , (ltWord_RDR  , leWord_RDR  , eqWord_RDR  , geWord_RDR  , gtWord_RDR  ))
    ,(addrPrimTy  , (ltAddr_RDR  , leAddr_RDR  , eqAddr_RDR  , geAddr_RDR  , gtAddr_RDR  ))
    ,(floatPrimTy , (ltFloat_RDR , leFloat_RDR , eqFloat_RDR , geFloat_RDR , gtFloat_RDR ))
    ,(doublePrimTy, (ltDouble_RDR, leDouble_RDR, eqDouble_RDR, geDouble_RDR, gtDouble_RDR)) ]

boxConTbl :: [(Type, RdrName)]
boxConTbl
  = [(charPrimTy  , getRdrName charDataCon  )
    ,(intPrimTy   , getRdrName intDataCon   )
    ,(wordPrimTy  , getRdrName wordDataCon  )
    ,(floatPrimTy , getRdrName floatDataCon )
    ,(doublePrimTy, getRdrName doubleDataCon)
    ]

-- | A table of postfix modifiers for unboxed values.
postfixModTbl :: [(Type, String)]
postfixModTbl
  = [(charPrimTy  , "#" )
    ,(intPrimTy   , "#" )
    ,(wordPrimTy  , "##")
    ,(floatPrimTy , "#" )
    ,(doublePrimTy, "##")
    ]

litConTbl :: [(Type, LHsExpr RdrName -> LHsExpr RdrName)]
litConTbl
  = [(charPrimTy  , nlHsApp (nlHsVar charPrimL_RDR))
    ,(intPrimTy   , nlHsApp (nlHsVar intPrimL_RDR)
                      . nlHsApp (nlHsVar toInteger_RDR))
    ,(wordPrimTy  , nlHsApp (nlHsVar wordPrimL_RDR)
                      . nlHsApp (nlHsVar toInteger_RDR))
    ,(addrPrimTy  , nlHsApp (nlHsVar stringPrimL_RDR)
                      . nlHsApp (nlHsApp
                          (nlHsVar map_RDR)
                          (compose_RDR `nlHsApps`
                            [ nlHsVar fromIntegral_RDR
                            , nlHsVar fromEnum_RDR
                            ])))
    ,(floatPrimTy , nlHsApp (nlHsVar floatPrimL_RDR)
                      . nlHsApp (nlHsVar toRational_RDR))
    ,(doublePrimTy, nlHsApp (nlHsVar doublePrimL_RDR)
                      . nlHsApp (nlHsVar toRational_RDR))
    ]

-- | Lookup `Type` in an association list.
assoc_ty_id :: String           -- The class involved
            -> TyCon            -- The tycon involved
            -> [(Type,a)]       -- The table
            -> Type             -- The type
            -> a                -- The result of the lookup
assoc_ty_id cls_str _ tbl ty
  | null res = pprPanic "Error in deriving:" (text "Can't derive" <+> text cls_str <+>
                                              text "for primitive type" <+> ppr ty)
  | otherwise = head res
  where
    res = [id | (ty',id) <- tbl, ty `eqType` ty']

-----------------------------------------------------------------------

and_Expr :: LHsExpr RdrName -> LHsExpr RdrName -> LHsExpr RdrName
and_Expr a b = genOpApp a and_RDR    b

-----------------------------------------------------------------------

eq_Expr :: TyCon -> Type -> LHsExpr RdrName -> LHsExpr RdrName -> LHsExpr RdrName
eq_Expr tycon ty a b
    | not (isUnliftedType ty) = genOpApp a eq_RDR b
    | otherwise               = genPrimOpApp a prim_eq b
 where
   (_, _, prim_eq, _, _) = primOrdOps "Eq" tycon ty

untag_Expr :: TyCon -> [( RdrName,  RdrName)] -> LHsExpr RdrName -> LHsExpr RdrName
untag_Expr _ [] expr = expr
untag_Expr tycon ((untag_this, put_tag_here) : more) expr
  = nlHsCase (nlHsPar (nlHsVarApps (con2tag_RDR tycon) [untag_this])) {-of-}
      [mkHsCaseAlt (nlVarPat put_tag_here) (untag_Expr tycon more expr)]

enum_from_to_Expr
        :: LHsExpr RdrName -> LHsExpr RdrName
        -> LHsExpr RdrName
enum_from_then_to_Expr
        :: LHsExpr RdrName -> LHsExpr RdrName -> LHsExpr RdrName
        -> LHsExpr RdrName

enum_from_to_Expr      f   t2 = nlHsApp (nlHsApp (nlHsVar enumFromTo_RDR) f) t2
enum_from_then_to_Expr f t t2 = nlHsApp (nlHsApp (nlHsApp (nlHsVar enumFromThenTo_RDR) f) t) t2

showParen_Expr
        :: LHsExpr RdrName -> LHsExpr RdrName
        -> LHsExpr RdrName

showParen_Expr e1 e2 = nlHsApp (nlHsApp (nlHsVar showParen_RDR) e1) e2

nested_compose_Expr :: [LHsExpr RdrName] -> LHsExpr RdrName

nested_compose_Expr []  = panic "nested_compose_expr"   -- Arg is always non-empty
nested_compose_Expr [e] = parenify e
nested_compose_Expr (e:es)
  = nlHsApp (nlHsApp (nlHsVar compose_RDR) (parenify e)) (nested_compose_Expr es)

-- impossible_Expr is used in case RHSs that should never happen.
-- We generate these to keep the desugarer from complaining that they *might* happen!
error_Expr :: String -> LHsExpr RdrName
error_Expr string = nlHsApp (nlHsVar error_RDR) (nlHsLit (mkHsString string))

-- illegal_Expr is used when signalling error conditions in the RHS of a derived
-- method. It is currently only used by Enum.{succ,pred}
illegal_Expr :: String -> String -> String -> LHsExpr RdrName
illegal_Expr meth tp msg =
   nlHsApp (nlHsVar error_RDR) (nlHsLit (mkHsString (meth ++ '{':tp ++ "}: " ++ msg)))

-- illegal_toEnum_tag is an extended version of illegal_Expr, which also allows you
-- to include the value of a_RDR in the error string.
illegal_toEnum_tag :: String -> RdrName -> LHsExpr RdrName
illegal_toEnum_tag tp maxtag =
   nlHsApp (nlHsVar error_RDR)
           (nlHsApp (nlHsApp (nlHsVar append_RDR)
                       (nlHsLit (mkHsString ("toEnum{" ++ tp ++ "}: tag ("))))
                    (nlHsApp (nlHsApp (nlHsApp
                           (nlHsVar showsPrec_RDR)
                           (nlHsIntLit 0))
                           (nlHsVar a_RDR))
                           (nlHsApp (nlHsApp
                               (nlHsVar append_RDR)
                               (nlHsLit (mkHsString ") is outside of enumeration's range (0,")))
                               (nlHsApp (nlHsApp (nlHsApp
                                        (nlHsVar showsPrec_RDR)
                                        (nlHsIntLit 0))
                                        (nlHsVar maxtag))
                                        (nlHsLit (mkHsString ")"))))))

parenify :: LHsExpr RdrName -> LHsExpr RdrName
parenify e@(L _ (HsVar _)) = e
parenify e                 = mkHsPar e

-- genOpApp wraps brackets round the operator application, so that the
-- renamer won't subsequently try to re-associate it.
genOpApp :: LHsExpr RdrName -> RdrName -> LHsExpr RdrName -> LHsExpr RdrName
genOpApp e1 op e2 = nlHsPar (nlHsOpApp e1 op e2)

genPrimOpApp :: LHsExpr RdrName -> RdrName -> LHsExpr RdrName -> LHsExpr RdrName
genPrimOpApp e1 op e2 = nlHsPar (nlHsApp (nlHsVar tagToEnum_RDR) (nlHsOpApp e1 op e2))

a_RDR, b_RDR, c_RDR, d_RDR, f_RDR, k_RDR, z_RDR, ah_RDR, bh_RDR, ch_RDR, dh_RDR
    :: RdrName
a_RDR           = mkVarUnqual (fsLit "a")
b_RDR           = mkVarUnqual (fsLit "b")
c_RDR           = mkVarUnqual (fsLit "c")
d_RDR           = mkVarUnqual (fsLit "d")
f_RDR           = mkVarUnqual (fsLit "f")
k_RDR           = mkVarUnqual (fsLit "k")
z_RDR           = mkVarUnqual (fsLit "z")
ah_RDR          = mkVarUnqual (fsLit "a#")
bh_RDR          = mkVarUnqual (fsLit "b#")
ch_RDR          = mkVarUnqual (fsLit "c#")
dh_RDR          = mkVarUnqual (fsLit "d#")

as_RDRs, bs_RDRs, cs_RDRs :: [RdrName]
as_RDRs         = [ mkVarUnqual (mkFastString ("a"++show i)) | i <- [(1::Int) .. ] ]
bs_RDRs         = [ mkVarUnqual (mkFastString ("b"++show i)) | i <- [(1::Int) .. ] ]
cs_RDRs         = [ mkVarUnqual (mkFastString ("c"++show i)) | i <- [(1::Int) .. ] ]

a_Expr, b_Expr, c_Expr, f_Expr, z_Expr, ltTag_Expr, eqTag_Expr, gtTag_Expr,
    false_Expr, true_Expr, fmap_Expr,
    mempty_Expr, foldMap_Expr, traverse_Expr :: LHsExpr RdrName
a_Expr          = nlHsVar a_RDR
b_Expr          = nlHsVar b_RDR
c_Expr          = nlHsVar c_RDR
f_Expr          = nlHsVar f_RDR
z_Expr          = nlHsVar z_RDR
ltTag_Expr      = nlHsVar ltTag_RDR
eqTag_Expr      = nlHsVar eqTag_RDR
gtTag_Expr      = nlHsVar gtTag_RDR
false_Expr      = nlHsVar false_RDR
true_Expr       = nlHsVar true_RDR
fmap_Expr       = nlHsVar fmap_RDR
-- pure_Expr       = nlHsVar pure_RDR
mempty_Expr     = nlHsVar mempty_RDR
foldMap_Expr    = nlHsVar foldMap_RDR
traverse_Expr   = nlHsVar traverse_RDR

a_Pat, b_Pat, c_Pat, d_Pat, f_Pat, k_Pat, z_Pat :: LPat RdrName
a_Pat           = nlVarPat a_RDR
b_Pat           = nlVarPat b_RDR
c_Pat           = nlVarPat c_RDR
d_Pat           = nlVarPat d_RDR
f_Pat           = nlVarPat f_RDR
k_Pat           = nlVarPat k_RDR
z_Pat           = nlVarPat z_RDR

minusInt_RDR, tagToEnum_RDR :: RdrName
minusInt_RDR  = getRdrName (primOpId IntSubOp   )
tagToEnum_RDR = getRdrName (primOpId TagToEnumOp)

con2tag_RDR, tag2con_RDR, maxtag_RDR :: TyCon -> RdrName
-- Generates Orig s RdrName, for the binding positions
con2tag_RDR tycon = mk_tc_deriv_name tycon mkCon2TagOcc
tag2con_RDR tycon = mk_tc_deriv_name tycon mkTag2ConOcc
maxtag_RDR  tycon = mk_tc_deriv_name tycon mkMaxTagOcc

mk_tc_deriv_name :: TyCon -> (OccName -> OccName) -> RdrName
mk_tc_deriv_name tycon occ_fun = mkAuxBinderName (tyConName tycon) occ_fun

mkAuxBinderName :: Name -> (OccName -> OccName) -> RdrName
-- ^ Make a top-level binder name for an auxiliary binding for a parent name
-- See Note [Auxiliary binders]
mkAuxBinderName parent occ_fun
  = mkRdrUnqual (occ_fun stable_parent_occ)
  where
    stable_parent_occ = mkOccName (occNameSpace parent_occ) stable_string
    stable_string
      | opt_PprStyle_Debug = parent_stable
      | otherwise = parent_stable_hash
    parent_stable = nameStableString parent
    parent_stable_hash =
      let Fingerprint high low = fingerprintString parent_stable
      in toBase62 high ++ toBase62Padded low
      -- See Note [Base 62 encoding 128-bit integers]
    parent_occ  = nameOccName parent


{-
Note [Auxiliary binders]
~~~~~~~~~~~~~~~~~~~~~~~~
We often want to make a top-level auxiliary binding.  E.g. for comparison we haev

  instance Ord T where
    compare a b = $con2tag a `compare` $con2tag b

  $con2tag :: T -> Int
  $con2tag = ...code....

Of course these top-level bindings should all have distinct name, and we are
generating RdrNames here.  We can't just use the TyCon or DataCon to distinguish
because with standalone deriving two imported TyCons might both be called T!
(See Trac #7947.)

So we use package name, module name and the name of the parent
(T in this example) as part of the OccName we generate for the new binding.
To make the symbol names short we take a base62 hash of the full name.

In the past we used the *unique* from the parent, but that's not stable across
recompilations as uniques are nondeterministic.

Note [DeriveFoldable with ExistentialQuantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Functor and Traversable instances can only be derived for data types whose
last type parameter is truly universally polymorphic. For example:

  data T a b where
    T1 ::                 b   -> T a b   -- YES, b is unconstrained
    T2 :: Ord b   =>      b   -> T a b   -- NO, b is constrained by (Ord b)
    T3 :: b ~ Int =>      b   -> T a b   -- NO, b is constrained by (b ~ Int)
    T4 ::                 Int -> T a Int -- NO, this is just like T3
    T5 :: Ord a   => a -> b   -> T a b   -- YES, b is unconstrained, even
                                         -- though a is existential
    T6 ::                 Int -> T Int b -- YES, b is unconstrained

For Foldable instances, however, we can completely lift the constraint that
the last type parameter be truly universally polymorphic. This means that T
(as defined above) can have a derived Foldable instance:

  instance Foldable (T a) where
    foldr f z (T1 b)   = f b z
    foldr f z (T2 b)   = f b z
    foldr f z (T3 b)   = f b z
    foldr f z (T4 b)   = z
    foldr f z (T5 a b) = f b z
    foldr f z (T6 a)   = z

    foldMap f (T1 b)   = f b
    foldMap f (T2 b)   = f b
    foldMap f (T3 b)   = f b
    foldMap f (T4 b)   = mempty
    foldMap f (T5 a b) = f b
    foldMap f (T6 a)   = mempty

In a Foldable instance, it is safe to fold over an occurrence of the last type
parameter that is not truly universally polymorphic. However, there is a bit
of subtlety in determining what is actually an occurrence of a type parameter.
T3 and T4, as defined above, provide one example:

  data T a b where
    ...
    T3 :: b ~ Int => b   -> T a b
    T4 ::            Int -> T a Int
    ...

  instance Foldable (T a) where
    ...
    foldr f z (T3 b) = f b z
    foldr f z (T4 b) = z
    ...
    foldMap f (T3 b) = f b
    foldMap f (T4 b) = mempty
    ...

Notice that the argument of T3 is folded over, whereas the argument of T4 is
not. This is because we only fold over constructor arguments that
syntactically mention the universally quantified type parameter of that
particular data constructor. See foldDataConArgs for how this is implemented.

As another example, consider the following data type. The argument of each
constructor has the same type as the last type parameter:

  data E a where
    E1 :: (a ~ Int) => a   -> E a
    E2 ::              Int -> E Int
    E3 :: (a ~ Int) => a   -> E Int
    E4 :: (a ~ Int) => Int -> E a

Only E1's argument is an occurrence of a universally quantified type variable
that is syntactically equivalent to the last type parameter, so only E1's
argument will be be folded over in a derived Foldable instance.

See Trac #10447 for the original discussion on this feature. Also see
https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor
for a more in-depth explanation.

Note [FFoldType and functorLikeTraverse]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Deriving Functor, Foldable, and Traversable all require generating expressions
which perform an operation on each argument of a data constructor depending
on the argument's type. In particular, a generated operation can be different
depending on whether the type mentions the last type variable of the datatype
(e.g., if you have data T a = MkT a Int, then a generated foldr expresion would
fold over the first argument of MkT, but not the second).

This pattern is abstracted with the FFoldType datatype, which provides hooks
for the user to specify how a constructor argument should be folded when it
has a type with a particular "shape". The shapes are as follows (assume that
a is the last type variable in a given datatype):

* ft_triv:    The type does not mention the last type variable at all.
              Examples: Int, b

* ft_var:     The type is syntactically equal to the last type variable.
              Moreover, the type appears in a covariant position (see
              the Deriving Functor instances section of the users' guide
              for an in-depth explanation of covariance vs. contravariance).
              Example: a (covariantly)

* ft_co_var:  The type is syntactically equal to the last type variable.
              Moreover, the type appears in a contravariant position.
              Example: a (contravariantly)

* ft_fun:     A function type which mentions the last type variable in
              the argument position, result position or both.
              Examples: a -> Int, Int -> a, Maybe a -> [a]

* ft_tup:     A tuple type which mentions the last type variable in at least
              one of its fields. The TyCon argument of ft_tup represents the
              particular tuple's type constructor.
              Examples: (a, Int), (Maybe a, [a], Either a Int), (# Int, a #)

* ft_ty_app:  A type is being applied to the last type parameter, where the
              applied type does not mention the last type parameter (if it
              did, it would fall under ft_bad_app). The Type argument to
              ft_ty_app represents the applied type.

              Note that functions, tuples, and foralls are distinct cases
              and take precedence of ft_ty_app. (For example, (Int -> a) would
              fall under (ft_fun Int a), not (ft_ty_app ((->) Int) a).
              Examples: Maybe a, Either b a

* ft_bad_app: A type application uses the last type parameter in a position
              other than the last argument. This case is singled out because
              Functor, Foldable, and Traversable instances cannot be derived
              for datatypes containing arguments with such types.
              Examples: Either a Int, Const a b

* ft_forall:  A forall'd type mentions the last type parameter on its right-
              hand side (and is not quantified on the left-hand side). This
              case is present mostly for plumbing purposes.
              Example: forall b. Either b a

If FFoldType describes a strategy for folding subcomponents of a Type, then
functorLikeTraverse is the function that applies that strategy to the entirety
of a Type, returning the final folded-up result.

foldDataConArgs applies functorLikeTraverse to every argument type of a
constructor, returning a list of the fold results. This makes foldDataConArgs
a natural way to generate the subexpressions in a generated fmap, foldr,
foldMap, or traverse definition (the subexpressions must then be combined in
a method-specific fashion to form the final generated expression).

Deriving Generic1 also does validity checking by looking for the last type
variable in certain positions of a constructor's argument types, so it also
uses foldDataConArgs. See Note [degenerate use of FFoldType] in TcGenGenerics.

Note [Generated code for DeriveFoldable and DeriveTraversable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We adapt the algorithms for -XDeriveFoldable and -XDeriveTraversable based on
that of -XDeriveFunctor. However, there an important difference between deriving
the former two typeclasses and the latter one, which is best illustrated by the
following scenario:

  data WithInt a = WithInt a Int# deriving (Functor, Foldable, Traversable)

The generated code for the Functor instance is straightforward:

  instance Functor WithInt where
    fmap f (WithInt a i) = WithInt (f a) i

But if we use too similar of a strategy for deriving the Foldable and
Traversable instances, we end up with this code:

  instance Foldable WithInt where
    foldMap f (WithInt a i) = f a <> mempty

  instance Traversable WithInt where
    traverse f (WithInt a i) = fmap WithInt (f a) <*> pure i

This is unsatisfying for two reasons:

1. The Traversable instance doesn't typecheck! Int# is of kind #, but pure
   expects an argument whose type is of kind *. This effectively prevents
   Traversable from being derived for any datatype with an unlifted argument
   type (Trac #11174).

2. The generated code contains superfluous expressions. By the Monoid laws,
   we can reduce (f a <> mempty) to (f a), and by the Applicative laws, we can
   reduce (fmap WithInt (f a) <*> pure i) to (fmap (\b -> WithInt b i) (f a)).

We can fix both of these issues by incorporating a slight twist to the usual
algorithm that we use for -XDeriveFunctor. The differences can be summarized
as follows:

1. In the generated expression, we only fold over arguments whose types
   mention the last type parameter. Any other argument types will simply
   produce useless 'mempty's or 'pure's, so they can be safely ignored.

2. In the case of -XDeriveTraversable, instead of applying ConName,
   we apply (\b_i ... b_k -> ConName a_1 ... a_n), where

   * ConName has n arguments
   * {b_i, ..., b_k} is a subset of {a_1, ..., a_n} whose indices correspond
     to the arguments whose types mention the last type parameter. As a
     consequence, taking the difference of {a_1, ..., a_n} and
     {b_i, ..., b_k} yields the all the argument values of ConName whose types
     do not mention the last type parameter. Note that [i, ..., k] is a
     strictly increasing—but not necessarily consecutive—integer sequence.

     For example, the datatype

       data Foo a = Foo Int a Int a

     would generate the following Traversable instance:

       instance Traversable Foo where
         traverse f (Foo a1 a2 a3 a4) =
           fmap (\b2 b4 -> Foo a1 b2 a3 b4) (f a2) <*> f a4

Technically, this approach would also work for -XDeriveFunctor as well, but we
decide not to do so because:

1. There's not much benefit to generating, e.g., ((\b -> WithInt b i) (f a))
   instead of (WithInt (f a) i).

2. There would be certain datatypes for which the above strategy would
   generate Functor code that would fail to typecheck. For example:

     data Bar f a = Bar (forall f. Functor f => f a) deriving Functor

   With the conventional algorithm, it would generate something like:

     fmap f (Bar a) = Bar (fmap f a)

   which typechecks. But with the strategy mentioned above, it would generate:

     fmap f (Bar a) = (\b -> Bar b) (fmap f a)

   which does not typecheck, since GHC cannot unify the rank-2 type variables
   in the types of b and (fmap f a).
-}
