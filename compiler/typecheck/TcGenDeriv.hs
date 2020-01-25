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
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TcGenDeriv (
        BagDerivStuff, DerivStuff(..),

        gen_Eq_binds,
        gen_Ord_binds,
        gen_Enum_binds,
        gen_Bounded_binds,
        gen_Ix_binds,
        gen_Show_binds,
        gen_Read_binds,
        gen_Data_binds,
        gen_Lift_binds,
        gen_Newtype_binds,
        mkCoerceClassMethEqn,
        genAuxBinds,
        ordOpTbl, boxConTbl, litConTbl,
        mkRdrFunBind, mkRdrFunBindEC, mkRdrFunBindSE, error_Expr
    ) where

#include "HsVersions.h"

import GhcPrelude

import TcRnMonad
import GHC.Hs
import RdrName
import BasicTypes
import DataCon
import Name
import Fingerprint
import Encoding

import DynFlags
import PrelInfo
import FamInst
import FamInstEnv
import PrelNames
import THNames
import MkId ( coerceId )
import PrimOp
import SrcLoc
import TyCon
import TcEnv
import TcType
import TcValidity ( checkValidCoAxBranch )
import CoAxiom    ( coAxiomSingleBranch )
import TysPrim
import TysWiredIn
import Type
import Class
import VarSet
import VarEnv
import Util
import Var
import Outputable
import Lexeme
import FastString
import Pair
import Bag

import Data.List  ( find, partition, intersperse )

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
  | DerivHsBind (LHsBind GhcPs, LSig GhcPs) -- Also used for SYB


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

* If there are a lot of (more than ten) nullary constructors, we emit a
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

gen_Eq_binds :: SrcSpan -> TyCon -> TcM (LHsBinds GhcPs, BagDerivStuff)
gen_Eq_binds loc tycon = do
    dflags <- getDynFlags
    return (method_binds dflags, aux_binds)
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

    fall_through_eqn dflags
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
         untag_Expr dflags tycon [(a_RDR,ah_RDR), (b_RDR,bh_RDR)]
                    (genPrimOpApp (nlHsVar ah_RDR) eqInt_RDR (nlHsVar bh_RDR)))]

    aux_binds | no_tag_match_cons = emptyBag
              | otherwise         = unitBag $ DerivAuxBind $ DerivCon2Tag tycon

    method_binds dflags = unitBag (eq_bind dflags)
    eq_bind dflags = mkFunBindEC 2 loc eq_RDR (const true_Expr)
                                 (map pats_etc pat_match_cons
                                   ++ fall_through_eqn dflags)

    ------------------------------------------------------------------
    pats_etc data_con
      = let
            con1_pat = nlParPat $ nlConVarPat data_con_RDR as_needed
            con2_pat = nlParPat $ nlConVarPat data_con_RDR bs_needed

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
          = foldr1 and_Expr (zipWith3Equal "nested_eq" nested_eq tys as bs)
          -- Using 'foldr1' here ensures that the derived code is correctly
          -- associated. See #10859.
          where
            nested_eq ty a b = nlHsPar (eq_Expr ty (nlHsVar a) (nlHsVar b))

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
comparisons on top of it; see #2130, #4019.  Reason: we don't
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
ltResult :: OrdOp -> LHsExpr GhcPs
-- Knowing a<b, what is the result for a `op` b?
ltResult OrdCompare = ltTag_Expr
ltResult OrdLT      = true_Expr
ltResult OrdLE      = true_Expr
ltResult OrdGE      = false_Expr
ltResult OrdGT      = false_Expr

------------
eqResult :: OrdOp -> LHsExpr GhcPs
-- Knowing a=b, what is the result for a `op` b?
eqResult OrdCompare = eqTag_Expr
eqResult OrdLT      = false_Expr
eqResult OrdLE      = true_Expr
eqResult OrdGE      = true_Expr
eqResult OrdGT      = false_Expr

------------
gtResult :: OrdOp -> LHsExpr GhcPs
-- Knowing a>b, what is the result for a `op` b?
gtResult OrdCompare = gtTag_Expr
gtResult OrdLT      = false_Expr
gtResult OrdLE      = false_Expr
gtResult OrdGE      = true_Expr
gtResult OrdGT      = true_Expr

------------
gen_Ord_binds :: SrcSpan -> TyCon -> TcM (LHsBinds GhcPs, BagDerivStuff)
gen_Ord_binds loc tycon = do
    dflags <- getDynFlags
    return $ if null tycon_data_cons -- No data-cons => invoke bale-out case
      then ( unitBag $ mkFunBindEC 2 loc compare_RDR (const eqTag_Expr) []
           , emptyBag)
      else ( unitBag (mkOrdOp dflags OrdCompare) `unionBags` other_ops dflags
           , aux_binds)
  where
    aux_binds | single_con_type = emptyBag
              | otherwise       = unitBag $ DerivAuxBind $ DerivCon2Tag tycon

        -- Note [Game plan for deriving Ord]
    other_ops dflags
      | (last_tag - first_tag) <= 2     -- 1-3 constructors
        || null non_nullary_cons        -- Or it's an enumeration
      = listToBag [mkOrdOp dflags OrdLT, lE, gT, gE]
      | otherwise
      = emptyBag

    negate_expr = nlHsApp (nlHsVar not_RDR)
    lE = mkSimpleGeneratedFunBind loc le_RDR [a_Pat, b_Pat] $
        negate_expr (nlHsApp (nlHsApp (nlHsVar lt_RDR) b_Expr) a_Expr)
    gT = mkSimpleGeneratedFunBind loc gt_RDR [a_Pat, b_Pat] $
        nlHsApp (nlHsApp (nlHsVar lt_RDR) b_Expr) a_Expr
    gE = mkSimpleGeneratedFunBind loc ge_RDR [a_Pat, b_Pat] $
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


    mkOrdOp :: DynFlags -> OrdOp -> LHsBind GhcPs
    -- Returns a binding   op a b = ... compares a and b according to op ....
    mkOrdOp dflags op = mkSimpleGeneratedFunBind loc (ordMethRdr op) [a_Pat, b_Pat]
                                        (mkOrdOpRhs dflags op)

    mkOrdOpRhs :: DynFlags -> OrdOp -> LHsExpr GhcPs
    mkOrdOpRhs dflags op       -- RHS for comparing 'a' and 'b' according to op
      | nullary_cons `lengthAtMost` 2 -- Two nullary or fewer, so use cases
      = nlHsCase (nlHsVar a_RDR) $
        map (mkOrdOpAlt dflags op) tycon_data_cons
        -- i.e.  case a of { C1 x y -> case b of C1 x y -> ....compare x,y...
        --                   C2 x   -> case b of C2 x -> ....comopare x.... }

      | null non_nullary_cons    -- All nullary, so go straight to comparing tags
      = mkTagCmp dflags op

      | otherwise                -- Mixed nullary and non-nullary
      = nlHsCase (nlHsVar a_RDR) $
        (map (mkOrdOpAlt dflags op) non_nullary_cons
         ++ [mkHsCaseAlt nlWildPat (mkTagCmp dflags op)])


    mkOrdOpAlt :: DynFlags -> OrdOp -> DataCon
                  -> LMatch GhcPs (LHsExpr GhcPs)
    -- Make the alternative  (Ki a1 a2 .. av ->
    mkOrdOpAlt dflags op data_con
      = mkHsCaseAlt (nlConVarPat data_con_RDR as_needed)
                    (mkInnerRhs dflags op data_con)
      where
        as_needed    = take (dataConSourceArity data_con) as_RDRs
        data_con_RDR = getRdrName data_con

    mkInnerRhs dflags op data_con
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
      = untag_Expr dflags tycon [(b_RDR, bh_RDR)] $
        nlHsIf (genPrimOpApp (nlHsVar bh_RDR) ltInt_RDR tag_lit)
               (gtResult op) $  -- Definitely GT
        nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con
                                 , mkHsCaseAlt nlWildPat (ltResult op) ]

      | otherwise               -- upper range is larger
      = untag_Expr dflags tycon [(b_RDR, bh_RDR)] $
        nlHsIf (genPrimOpApp (nlHsVar bh_RDR) gtInt_RDR tag_lit)
               (ltResult op) $  -- Definitely LT
        nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con
                                 , mkHsCaseAlt nlWildPat (gtResult op) ]
      where
        tag     = get_tag data_con
        tag_lit = noLoc (HsLit noExtField (HsIntPrim NoSourceText (toInteger tag)))

    mkInnerEqAlt :: OrdOp -> DataCon -> LMatch GhcPs (LHsExpr GhcPs)
    -- First argument 'a' known to be built with K
    -- Returns a case alternative  Ki b1 b2 ... bv -> compare (a1,a2,...) with (b1,b2,...)
    mkInnerEqAlt op data_con
      = mkHsCaseAlt (nlConVarPat data_con_RDR bs_needed) $
        mkCompareFields op (dataConOrigArgTys data_con)
      where
        data_con_RDR = getRdrName data_con
        bs_needed    = take (dataConSourceArity data_con) bs_RDRs

    mkTagCmp :: DynFlags -> OrdOp -> LHsExpr GhcPs
    -- Both constructors known to be nullary
    -- generates (case data2Tag a of a# -> case data2Tag b of b# -> a# `op` b#
    mkTagCmp dflags op =
      untag_Expr dflags tycon[(a_RDR, ah_RDR),(b_RDR, bh_RDR)] $
        unliftedOrdOp intPrimTy op ah_RDR bh_RDR

mkCompareFields :: OrdOp -> [Type] -> LHsExpr GhcPs
-- Generates nested comparisons for (a1,a2...) against (b1,b2,...)
-- where the ai,bi have the given types
mkCompareFields op tys
  = go tys as_RDRs bs_RDRs
  where
    go []   _      _          = eqResult op
    go [ty] (a:_)  (b:_)
      | isUnliftedType ty     = unliftedOrdOp ty op a b
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
        (lt_op, _, eq_op, _, _) = primOrdOps "Ord" ty

unliftedOrdOp :: Type -> OrdOp -> RdrName -> RdrName -> LHsExpr GhcPs
unliftedOrdOp ty op a b
  = case op of
       OrdCompare -> unliftedCompare lt_op eq_op a_expr b_expr
                                     ltTag_Expr eqTag_Expr gtTag_Expr
       OrdLT      -> wrap lt_op
       OrdLE      -> wrap le_op
       OrdGE      -> wrap ge_op
       OrdGT      -> wrap gt_op
  where
   (lt_op, le_op, eq_op, ge_op, gt_op) = primOrdOps "Ord" ty
   wrap prim_op = genPrimOpApp a_expr prim_op b_expr
   a_expr = nlHsVar a
   b_expr = nlHsVar b

unliftedCompare :: RdrName -> RdrName
                -> LHsExpr GhcPs -> LHsExpr GhcPs   -- What to compare
                -> LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
                                                    -- Three results
                -> LHsExpr GhcPs
-- Return (if a < b then lt else if a == b then eq else gt)
unliftedCompare lt_op eq_op a_expr b_expr lt eq gt
  = nlHsIf (ascribeBool $ genPrimOpApp a_expr lt_op b_expr) lt $
                        -- Test (<) first, not (==), because the latter
                        -- is true less often, so putting it first would
                        -- mean more tests (dynamically)
        nlHsIf (ascribeBool $ genPrimOpApp a_expr eq_op b_expr) eq gt
  where
    ascribeBool e = nlExprWithTySig e boolTy

nlConWildPat :: DataCon -> LPat GhcPs
-- The pattern (K {})
nlConWildPat con = noLoc $ ConPat
  (noLoc $ getRdrName con)
  (RecCon $ HsRecFields { rec_flds = []
                        , rec_dotdot = Nothing })
  NoExtField

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

gen_Enum_binds :: SrcSpan -> TyCon -> TcM (LHsBinds GhcPs, BagDerivStuff)
gen_Enum_binds loc tycon = do
    dflags <- getDynFlags
    return (method_binds dflags, aux_binds)
  where
    method_binds dflags = listToBag
      [ succ_enum      dflags
      , pred_enum      dflags
      , to_enum        dflags
      , enum_from      dflags
      , enum_from_then dflags
      , from_enum      dflags
      ]
    aux_binds = listToBag $ map DerivAuxBind
                  [DerivCon2Tag tycon, DerivTag2Con tycon, DerivMaxTag tycon]

    occ_nm = getOccString tycon

    succ_enum dflags
      = mkSimpleGeneratedFunBind loc succ_RDR [a_Pat] $
        untag_Expr dflags tycon [(a_RDR, ah_RDR)] $
        nlHsIf (nlHsApps eq_RDR [nlHsVar (maxtag_RDR dflags tycon),
                               nlHsVarApps intDataCon_RDR [ah_RDR]])
             (illegal_Expr "succ" occ_nm "tried to take `succ' of last tag in enumeration")
             (nlHsApp (nlHsVar (tag2con_RDR dflags tycon))
                    (nlHsApps plus_RDR [nlHsVarApps intDataCon_RDR [ah_RDR],
                                        nlHsIntLit 1]))

    pred_enum dflags
      = mkSimpleGeneratedFunBind loc pred_RDR [a_Pat] $
        untag_Expr dflags tycon [(a_RDR, ah_RDR)] $
        nlHsIf (nlHsApps eq_RDR [nlHsIntLit 0,
                               nlHsVarApps intDataCon_RDR [ah_RDR]])
             (illegal_Expr "pred" occ_nm "tried to take `pred' of first tag in enumeration")
             (nlHsApp (nlHsVar (tag2con_RDR dflags tycon))
                      (nlHsApps plus_RDR
                            [ nlHsVarApps intDataCon_RDR [ah_RDR]
                            , nlHsLit (HsInt noExtField
                                                (mkIntegralLit (-1 :: Int)))]))

    to_enum dflags
      = mkSimpleGeneratedFunBind loc toEnum_RDR [a_Pat] $
        nlHsIf (nlHsApps and_RDR
                [nlHsApps ge_RDR [nlHsVar a_RDR, nlHsIntLit 0],
                 nlHsApps le_RDR [ nlHsVar a_RDR
                                 , nlHsVar (maxtag_RDR dflags tycon)]])
             (nlHsVarApps (tag2con_RDR dflags tycon) [a_RDR])
             (illegal_toEnum_tag occ_nm (maxtag_RDR dflags tycon))

    enum_from dflags
      = mkSimpleGeneratedFunBind loc enumFrom_RDR [a_Pat] $
          untag_Expr dflags tycon [(a_RDR, ah_RDR)] $
          nlHsApps map_RDR
                [nlHsVar (tag2con_RDR dflags tycon),
                 nlHsPar (enum_from_to_Expr
                            (nlHsVarApps intDataCon_RDR [ah_RDR])
                            (nlHsVar (maxtag_RDR dflags tycon)))]

    enum_from_then dflags
      = mkSimpleGeneratedFunBind loc enumFromThen_RDR [a_Pat, b_Pat] $
          untag_Expr dflags tycon [(a_RDR, ah_RDR), (b_RDR, bh_RDR)] $
          nlHsApp (nlHsVarApps map_RDR [tag2con_RDR dflags tycon]) $
            nlHsPar (enum_from_then_to_Expr
                    (nlHsVarApps intDataCon_RDR [ah_RDR])
                    (nlHsVarApps intDataCon_RDR [bh_RDR])
                    (nlHsIf  (nlHsApps gt_RDR [nlHsVarApps intDataCon_RDR [ah_RDR],
                                               nlHsVarApps intDataCon_RDR [bh_RDR]])
                           (nlHsIntLit 0)
                           (nlHsVar (maxtag_RDR dflags tycon))
                           ))

    from_enum dflags
      = mkSimpleGeneratedFunBind loc fromEnum_RDR [a_Pat] $
          untag_Expr dflags tycon [(a_RDR, ah_RDR)] $
          (nlHsVarApps intDataCon_RDR [ah_RDR])

{-
************************************************************************
*                                                                      *
        Bounded instances
*                                                                      *
************************************************************************
-}

gen_Bounded_binds :: SrcSpan -> TyCon -> (LHsBinds GhcPs, BagDerivStuff)
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
                     nlHsVarApps data_con_1_RDR (replicate arity minBound_RDR)
    max_bound_1con = mkHsVarBind loc maxBound_RDR $
                     nlHsVarApps data_con_1_RDR (replicate arity maxBound_RDR)

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

gen_Ix_binds :: SrcSpan -> TyCon -> TcM (LHsBinds GhcPs, BagDerivStuff)

gen_Ix_binds loc tycon = do
    dflags <- getDynFlags
    return $ if isEnumerationTyCon tycon
      then (enum_ixes dflags, listToBag $ map DerivAuxBind
                   [DerivCon2Tag tycon, DerivTag2Con tycon, DerivMaxTag tycon])
      else (single_con_ixes, unitBag (DerivAuxBind (DerivCon2Tag tycon)))
  where
    --------------------------------------------------------------
    enum_ixes dflags = listToBag
      [ enum_range   dflags
      , enum_index   dflags
      , enum_inRange dflags
      ]

    enum_range dflags
      = mkSimpleGeneratedFunBind loc range_RDR [nlTuplePat [a_Pat, b_Pat] Boxed] $
          untag_Expr dflags tycon [(a_RDR, ah_RDR)] $
          untag_Expr dflags tycon [(b_RDR, bh_RDR)] $
          nlHsApp (nlHsVarApps map_RDR [tag2con_RDR dflags tycon]) $
              nlHsPar (enum_from_to_Expr
                        (nlHsVarApps intDataCon_RDR [ah_RDR])
                        (nlHsVarApps intDataCon_RDR [bh_RDR]))

    enum_index dflags
      = mkSimpleGeneratedFunBind loc unsafeIndex_RDR
                [noLoc (AsPat noExtField (noLoc c_RDR)
                           (nlTuplePat [a_Pat, nlWildPat] Boxed)),
                                d_Pat] (
           untag_Expr dflags tycon [(a_RDR, ah_RDR)] (
           untag_Expr dflags tycon [(d_RDR, dh_RDR)] (
           let
                rhs = nlHsVarApps intDataCon_RDR [c_RDR]
           in
           nlHsCase
             (genOpApp (nlHsVar dh_RDR) minusInt_RDR (nlHsVar ah_RDR))
             [mkHsCaseAlt (nlVarPat c_RDR) rhs]
           ))
        )

    -- This produces something like `(ch >= ah) && (ch <= bh)`
    enum_inRange dflags
      = mkSimpleGeneratedFunBind loc inRange_RDR [nlTuplePat [a_Pat, b_Pat] Boxed, c_Pat] $
          untag_Expr dflags tycon [(a_RDR, ah_RDR)] (
          untag_Expr dflags tycon [(b_RDR, bh_RDR)] (
          untag_Expr dflags tycon [(c_RDR, ch_RDR)] (
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
      = mkSimpleGeneratedFunBind loc range_RDR
          [nlTuplePat [con_pat as_needed, con_pat bs_needed] Boxed] $
        noLoc (mkHsComp ListComp stmts con_expr)
      where
        stmts = zipWith3Equal "single_con_range" mk_qual as_needed bs_needed cs_needed

        mk_qual a b c = noLoc $ mkBindStmt (nlVarPat c)
                                 (nlHsApp (nlHsVar range_RDR)
                                          (mkLHsVarTuple [a,b]))

    ----------------
    single_con_index
      = mkSimpleGeneratedFunBind loc unsafeIndex_RDR
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
      = mkSimpleGeneratedFunBind loc inRange_RDR
                [nlTuplePat [con_pat as_needed, con_pat bs_needed] Boxed,
                 con_pat cs_needed] $
          if con_arity == 0
             -- If the product type has no fields, inRange is trivially true
             -- (see #12853).
             then true_Expr
             else foldl1 and_Expr (zipWith3Equal "single_con_inRange" in_range
                    as_needed bs_needed cs_needed)
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
           x          <- Read.readField "f1" (ReadP.reset readPrec)
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
compact.  Cf #7258, although that also concerned non-linearity in
the occurrence analyser, a separate issue.

Note [Read for empty data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should we get for this?  (#7931)
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

gen_Read_binds :: (Name -> Fixity) -> SrcSpan -> TyCon
               -> (LHsBinds GhcPs, BagDerivStuff)

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

    read_prec = mkHsVarBind loc readPrec_RDR rhs
      where
        rhs | null data_cons -- See Note [Read for empty data types]
            = nlHsVar pfail_RDR
            | otherwise
            = nlHsApp (nlHsVar parens_RDR)
                      (foldr1 mk_alt (read_nullary_cons ++
                                      read_non_nullary_cons))

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
        is_record    = labels `lengthExceeds` 0
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
    -- Thus [Ident "I"; Symbol "#"].  See #5041
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

    -- When reading field labels we might encounter
    --      a  = 3
    --      _a = 3
    -- or   (#) = 4
    -- Note the parens!
    read_field lbl a =
        [noLoc
          (mkBindStmt
            (nlVarPat a)
            (nlHsApp
              read_field
              (nlHsVarApps reset_RDR [readPrec_RDR])
            )
          )
        ]
        where
          lbl_str = unpackFS lbl
          mk_read_field read_field_rdr lbl
              = nlHsApps read_field_rdr [nlHsLit (mkHsString lbl)]
          read_field
              | isSym lbl_str
              = mk_read_field readSymField_RDR lbl_str
              | Just (ss, '#') <- snocView lbl_str -- #14918
              = mk_read_field readFieldHash_RDR ss
              | otherwise
              = mk_read_field readField_RDR lbl_str

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

gen_Show_binds :: (Name -> Fixity) -> SrcSpan -> TyCon
               -> (LHsBinds GhcPs, BagDerivStuff)

gen_Show_binds get_fixity loc tycon
  = (unitBag shows_prec, emptyBag)
  where
    data_cons = tyConDataCons tycon
    shows_prec = mkFunBindEC 2 loc showsPrec_RDR id (map pats_etc data_cons)
    comma_space = nlHsVar showCommaSpace_RDR

    pats_etc data_con
      | nullary_con =  -- skip the showParen junk...
         ASSERT(null bs_needed)
         ([nlWildPat, con_pat], mk_showString_app op_con_str)
      | otherwise   =
         ([a_Pat, con_pat],
          showParen_Expr (genOpApp a_Expr ge_RDR (nlHsLit
                         (HsInt noExtField (mkIntegralLit con_prec_plus_one))))
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
                                intersperse [comma_space] $
                                [ [show_label lbl, arg]
                                | (lbl,arg) <- zipEqual "gen_Show_binds"
                                                        labels show_args ]

             show_arg :: RdrName -> Type -> LHsExpr GhcPs
             show_arg b arg_ty
                 | isUnliftedType arg_ty
                 -- See Note [Deriving and unboxed types] in TcDerivInfer
                 = with_conv $
                    nlHsApps compose_RDR
                        [mk_shows_app boxed_arg, mk_showString_app postfixMod]
                 | otherwise
                 = mk_showsPrec_app arg_prec arg
               where
                 arg        = nlHsVar b
                 boxed_arg  = box "Show" arg arg_ty
                 postfixMod = assoc_ty_id "Show" postfixModTbl arg_ty
                 with_conv expr
                    | (Just conv) <- assoc_ty_id_maybe primConvTbl arg_ty =
                        nested_compose_Expr
                            [ mk_showString_app ("(" ++ conv ++ " ")
                            , expr
                            , mk_showString_app ")"
                            ]
                    | otherwise = expr

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
mk_showString_app :: String -> LHsExpr GhcPs
mk_showString_app str = nlHsApp (nlHsVar showString_RDR) (nlHsLit (mkHsString str))

-- | showsPrec :: Show a => Int -> a -> ShowS
mk_showsPrec_app :: Integer -> LHsExpr GhcPs -> LHsExpr GhcPs
mk_showsPrec_app p x
  = nlHsApps showsPrec_RDR [nlHsLit (HsInt noExtField (mkIntegralLit p)), x]

-- | shows :: Show a => a -> ShowS
mk_shows_app :: LHsExpr GhcPs -> LHsExpr GhcPs
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
               -> TcM (LHsBinds GhcPs,  -- The method bindings
                       BagDerivStuff)   -- Auxiliary bindings
gen_Data_binds loc rep_tc
  = do { dflags  <- getDynFlags

       -- Make unique names for the data type and constructor
       -- auxiliary bindings.  Start with the name of the TyCon/DataCon
       -- but that might not be unique: see #12245.
       ; dt_occ  <- chooseUniqueOccTc (mkDataTOcc (getOccName rep_tc))
       ; dc_occs <- mapM (chooseUniqueOccTc . mkDataCOcc . getOccName)
                         (tyConDataCons rep_tc)
       ; let dt_rdr  = mkRdrUnqual dt_occ
             dc_rdrs = map mkRdrUnqual dc_occs

       -- OK, now do the work
       ; return (gen_data dflags dt_rdr dc_rdrs loc rep_tc) }

gen_data :: DynFlags -> RdrName -> [RdrName]
         -> SrcSpan -> TyCon
         -> (LHsBinds GhcPs,      -- The method bindings
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
                     L loc (TypeSig noExtField [L loc data_type_name] sig_ty))

    sig_ty = mkLHsSigWcType (nlHsTyVar dataType_RDR)
    rhs    = nlHsVar mkDataType_RDR
             `nlHsApp` nlHsLit (mkHsString (showSDocOneLine dflags (ppr rep_tc)))
             `nlHsApp` nlList (map nlHsVar constr_names)

    genDataDataCon :: DataCon -> RdrName -> DerivStuff
    genDataDataCon dc constr_name       --  $cT1 etc
      = DerivHsBind (mkHsVarBind loc constr_name rhs,
                     L loc (TypeSig noExtField [L loc constr_name] sig_ty))
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
    gfoldl_bind = mkFunBindEC 3 loc gfoldl_RDR id (map gfoldl_eqn data_cons)

    gfoldl_eqn con
      = ([nlVarPat k_RDR, z_Pat, nlConVarPat con_name as_needed],
                   foldl' mk_k_app (z_Expr `nlHsApp` nlHsVar con_name) as_needed)
                   where
                     con_name ::  RdrName
                     con_name = getRdrName con
                     as_needed = take (dataConSourceArity con) as_RDRs
                     mk_k_app e v = nlHsPar (nlHsOpApp e k_RDR (nlHsVar v))

        ------------ gunfold
    gunfold_bind = mkSimpleGeneratedFunBind loc
                     gunfold_RDR
                     [k_Pat, z_Pat, if one_constr then nlWildPat else c_Pat]
                     gunfold_rhs

    gunfold_rhs
        | one_constr = mk_unfold_rhs (head data_cons)   -- No need for case
        | otherwise  = nlHsCase (nlHsVar conIndex_RDR `nlHsApp` c_Expr)
                                (map gunfold_alt data_cons)

    gunfold_alt dc = mkHsCaseAlt (mk_unfold_pat dc) (mk_unfold_rhs dc)
    mk_unfold_rhs dc = foldr nlHsApp
                           (z_Expr `nlHsApp` nlHsVar (getRdrName dc))
                           (replicate (dataConSourceArity dc) (nlHsVar k_RDR))

    mk_unfold_pat dc    -- Last one is a wild-pat, to avoid
                        -- redundant test, and annoying warning
      | tag-fIRST_TAG == n_cons-1 = nlWildPat   -- Last constructor
      | otherwise = nlConPat intDataCon_RDR
                             [nlLitPat (HsIntPrim NoSourceText (toInteger tag))]
      where
        tag = dataConTag dc

        ------------ toConstr
    toCon_bind = mkFunBindEC 1 loc toConstr_RDR id
                     (zipWith to_con_eqn data_cons constr_names)
    to_con_eqn dc con_name = ([nlWildConPat dc], nlHsVar con_name)

        ------------ dataTypeOf
    dataTypeOf_bind = mkSimpleGeneratedFunBind
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
        -- See #4896
    tycon_kind = case tyConFamInst_maybe rep_tc of
                    Just (fam_tc, _) -> tyConKind fam_tc
                    Nothing          -> tyConKind rep_tc
    gcast_binds | tycon_kind `tcEqKind` kind1 = mk_gcast dataCast1_RDR gcast1_RDR
                | tycon_kind `tcEqKind` kind2 = mk_gcast dataCast2_RDR gcast2_RDR
                | otherwise                 = emptyBag
    mk_gcast dataCast_RDR gcast_RDR
      = unitBag (mkSimpleGeneratedFunBind loc dataCast_RDR [nlVarPat f_RDR]
                                 (nlHsVar gcast_RDR `nlHsApp` nlHsVar f_RDR))


kind1, kind2 :: Kind
kind1 = typeToTypeKind
kind2 = liftedTypeKind `mkVisFunTy` kind1

gfoldl_RDR, gunfold_RDR, toConstr_RDR, dataTypeOf_RDR, mkConstr_RDR,
    mkDataType_RDR, conIndex_RDR, prefix_RDR, infix_RDR,
    dataCast1_RDR, dataCast2_RDR, gcast1_RDR, gcast2_RDR,
    constr_RDR, dataType_RDR,
    eqChar_RDR  , ltChar_RDR  , geChar_RDR  , gtChar_RDR  , leChar_RDR  ,
    eqInt_RDR   , ltInt_RDR   , geInt_RDR   , gtInt_RDR   , leInt_RDR   ,
    eqInt8_RDR  , ltInt8_RDR  , geInt8_RDR  , gtInt8_RDR  , leInt8_RDR  ,
    eqInt16_RDR , ltInt16_RDR , geInt16_RDR , gtInt16_RDR , leInt16_RDR ,
    eqWord_RDR  , ltWord_RDR  , geWord_RDR  , gtWord_RDR  , leWord_RDR  ,
    eqWord8_RDR , ltWord8_RDR , geWord8_RDR , gtWord8_RDR , leWord8_RDR ,
    eqWord16_RDR, ltWord16_RDR, geWord16_RDR, gtWord16_RDR, leWord16_RDR,
    eqAddr_RDR  , ltAddr_RDR  , geAddr_RDR  , gtAddr_RDR  , leAddr_RDR  ,
    eqFloat_RDR , ltFloat_RDR , geFloat_RDR , gtFloat_RDR , leFloat_RDR ,
    eqDouble_RDR, ltDouble_RDR, geDouble_RDR, gtDouble_RDR, leDouble_RDR,
    extendWord8_RDR, extendInt8_RDR,
    extendWord16_RDR, extendInt16_RDR :: RdrName
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

eqInt8_RDR     = varQual_RDR  gHC_PRIM (fsLit "eqInt8#")
ltInt8_RDR     = varQual_RDR  gHC_PRIM (fsLit "ltInt8#" )
leInt8_RDR     = varQual_RDR  gHC_PRIM (fsLit "leInt8#")
gtInt8_RDR     = varQual_RDR  gHC_PRIM (fsLit "gtInt8#" )
geInt8_RDR     = varQual_RDR  gHC_PRIM (fsLit "geInt8#")

eqInt16_RDR    = varQual_RDR  gHC_PRIM (fsLit "eqInt16#")
ltInt16_RDR    = varQual_RDR  gHC_PRIM (fsLit "ltInt16#" )
leInt16_RDR    = varQual_RDR  gHC_PRIM (fsLit "leInt16#")
gtInt16_RDR    = varQual_RDR  gHC_PRIM (fsLit "gtInt16#" )
geInt16_RDR    = varQual_RDR  gHC_PRIM (fsLit "geInt16#")

eqWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "eqWord#")
ltWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "ltWord#")
leWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "leWord#")
gtWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "gtWord#")
geWord_RDR     = varQual_RDR  gHC_PRIM (fsLit "geWord#")

eqWord8_RDR    = varQual_RDR  gHC_PRIM (fsLit "eqWord8#")
ltWord8_RDR    = varQual_RDR  gHC_PRIM (fsLit "ltWord8#" )
leWord8_RDR    = varQual_RDR  gHC_PRIM (fsLit "leWord8#")
gtWord8_RDR    = varQual_RDR  gHC_PRIM (fsLit "gtWord8#" )
geWord8_RDR    = varQual_RDR  gHC_PRIM (fsLit "geWord8#")

eqWord16_RDR   = varQual_RDR  gHC_PRIM (fsLit "eqWord16#")
ltWord16_RDR   = varQual_RDR  gHC_PRIM (fsLit "ltWord16#" )
leWord16_RDR   = varQual_RDR  gHC_PRIM (fsLit "leWord16#")
gtWord16_RDR   = varQual_RDR  gHC_PRIM (fsLit "gtWord16#" )
geWord16_RDR   = varQual_RDR  gHC_PRIM (fsLit "geWord16#")

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

extendWord8_RDR = varQual_RDR  gHC_PRIM (fsLit "extendWord8#")
extendInt8_RDR  = varQual_RDR  gHC_PRIM (fsLit "extendInt8#")

extendWord16_RDR = varQual_RDR  gHC_PRIM (fsLit "extendWord16#")
extendInt16_RDR  = varQual_RDR  gHC_PRIM (fsLit "extendInt16#")


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
        lift (Foo a) = [| Foo a |]
        lift ((:^:) u v) = [| (:^:) u v |]

        liftTyped (Foo a) = [|| Foo a ||]
        liftTyped ((:^:) u v) = [|| (:^:) u v ||]
-}


gen_Lift_binds :: SrcSpan -> TyCon -> (LHsBinds GhcPs, BagDerivStuff)
gen_Lift_binds loc tycon = (listToBag [lift_bind, liftTyped_bind], emptyBag)
  where
    lift_bind      = mkFunBindEC 1 loc lift_RDR (nlHsApp pure_Expr)
                                 (map (pats_etc mk_exp) data_cons)
    liftTyped_bind = mkFunBindEC 1 loc liftTyped_RDR (nlHsApp pure_Expr)
                                 (map (pats_etc mk_texp) data_cons)

    mk_exp = ExpBr noExtField
    mk_texp = TExpBr noExtField
    data_cons = tyConDataCons tycon

    pats_etc mk_bracket data_con
      = ([con_pat], lift_Expr)
       where
            con_pat      = nlConVarPat data_con_RDR as_needed
            data_con_RDR = getRdrName data_con
            con_arity    = dataConSourceArity data_con
            as_needed    = take con_arity as_RDRs
            lift_Expr    = noLoc (HsBracket noExtField (mk_bracket br_body))
            br_body      = nlHsApps (Exact (dataConName data_con))
                                    (map nlHsVar as_needed)

{-
************************************************************************
*                                                                      *
                     Newtype-deriving instances
*                                                                      *
************************************************************************

Note [Newtype-deriving instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We take every method in the original instance and `coerce` it to fit
into the derived instance. We need type applications on the argument
to `coerce` to make it obvious what instantiation of the method we're
coercing from.  So from, say,

  class C a b where
    op :: forall c. a -> [b] -> c -> Int

  newtype T x = MkT <rep-ty>

  instance C a <rep-ty> => C a (T x) where
    op = coerce @ (a -> [<rep-ty>] -> c -> Int)
                @ (a -> [T x]      -> c -> Int)
                op :: forall c. a -> [T x] -> c -> Int

In addition to the type applications, we also have an explicit
type signature on the entire RHS. This brings the method-bound variable
`c` into scope over the two type applications.
See Note [GND and QuantifiedConstraints] for more information on why this
is important.

Giving 'coerce' two explicitly-visible type arguments grants us finer control
over how it should be instantiated. Recall

  coerce :: Coercible a b => a -> b

By giving it explicit type arguments we deal with the case where
'op' has a higher rank type, and so we must instantiate 'coerce' with
a polytype.  E.g.

   class C a where op :: a -> forall b. b -> b
   newtype T x = MkT <rep-ty>
   instance C <rep-ty> => C (T x) where
     op = coerce @ (<rep-ty> -> forall b. b -> b)
                 @ (T x      -> forall b. b -> b)
                op :: T x -> forall b. b -> b

The use of type applications is crucial here. If we had tried using only
explicit type signatures, like so:

   instance C <rep-ty> => C (T x) where
     op = coerce (op :: <rep-ty> -> forall b. b -> b)
                     :: T x      -> forall b. b -> b

Then GHC will attempt to deeply skolemize the two type signatures, which will
wreak havoc with the Coercible solver. Therefore, we instead use type
applications, which do not deeply skolemize and thus avoid this issue.
The downside is that we currently require -XImpredicativeTypes to permit this
polymorphic type instantiation, so we have to switch that flag on locally in
TcDeriv.genInst. See #8503 for more discussion.

Note [Newtype-deriving trickiness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12768):
  class C a where { op :: D a => a -> a }

  instance C a  => C [a] where { op = opList }

  opList :: (C a, D [a]) => [a] -> [a]
  opList = ...

Now suppose we try GND on this:
  newtype N a = MkN [a] deriving( C )

The GND is expecting to get an implementation of op for N by
coercing opList, thus:

  instance C a => C (N a) where { op = opN }

  opN :: (C a, D (N a)) => N a -> N a
  opN = coerce @([a]   -> [a])
               @([N a] -> [N a]
               opList :: D (N a) => [N a] -> [N a]

But there is no reason to suppose that (D [a]) and (D (N a))
are inter-coercible; these instances might completely different.
So GHC rightly rejects this code.

Note [GND and QuantifiedConstraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following example from #15290:

  class C m where
    join :: m (m a) -> m a

  newtype T m a = MkT (m a)

  deriving instance
    (C m, forall p q. Coercible p q => Coercible (m p) (m q)) =>
    C (T m)

The code that GHC used to generate for this was:

  instance (C m, forall p q. Coercible p q => Coercible (m p) (m q)) =>
      C (T m) where
    join = coerce @(forall a.   m   (m a) ->   m a)
                  @(forall a. T m (T m a) -> T m a)
                  join

This instantiates `coerce` at a polymorphic type, a form of impredicative
polymorphism, so we're already on thin ice. And in fact the ice breaks,
as we'll explain:

The call to `coerce` gives rise to:

  Coercible (forall a.   m   (m a) ->   m a)
            (forall a. T m (T m a) -> T m a)

And that simplified to the following implication constraint:

  forall a <no-ev>. m (T m a) ~R# m (m a)

But because this constraint is under a `forall`, inside a type, we have to
prove it *without computing any term evidence* (hence the <no-ev>). Alas, we
*must* generate a term-level evidence binding in order to instantiate the
quantified constraint! In response, GHC currently chooses not to use such
a quantified constraint.
See Note [Instances in no-evidence implications] in TcInteract.

But this isn't the death knell for combining QuantifiedConstraints with GND.
On the contrary, if we generate GND bindings in a slightly different way, then
we can avoid this situation altogether. Instead of applying `coerce` to two
polymorphic types, we instead let an explicit type signature do the polymorphic
instantiation, and omit the `forall`s in the type applications.
More concretely, we generate the following code instead:

  instance (C m, forall p q. Coercible p q => Coercible (m p) (m q)) =>
      C (T m) where
    join = coerce @(  m   (m a) ->   m a)
                  @(T m (T m a) -> T m a)
                  join :: forall a. T m (T m a) -> T m a

Now the visible type arguments are both monotypes, so we need do any of this
funny quantified constraint instantiation business.

You might think that that second @(T m (T m a) -> T m a) argument is redundant
in the presence of the explicit `:: forall a. T m (T m a) -> T m a` type
signature, but in fact leaving it off will break this example (from the
T15290d test case):

  class C a where
    c :: Int -> forall b. b -> a

  instance C Int

  instance C Age where
    c = coerce @(Int -> forall b. b -> Int)
               c :: Int -> forall b. b -> Age

That is because the explicit type signature deeply skolemizes the forall-bound
`b`, which wreaks havoc with the `Coercible` solver. An additional visible type
argument of @(Int -> forall b. b -> Age) is enough to prevent this.

Be aware that the use of an explicit type signature doesn't /solve/ this
problem; it just makes it less likely to occur. For example, if a class has
a truly higher-rank type like so:

  class CProblem m where
    op :: (forall b. ... (m b) ...) -> Int

Then the same situation will arise again. But at least it won't arise for the
common case of methods with ordinary, prenex-quantified types.

Note [GND and ambiguity]
~~~~~~~~~~~~~~~~~~~~~~~~
We make an effort to make the code generated through GND be robust w.r.t.
ambiguous type variables. As one example, consider the following example
(from #15637):

  class C a where f :: String
  instance C () where f = "foo"
  newtype T = T () deriving C

A naïve attempt and generating a C T instance would be:

  instance C T where
    f = coerce @String @String f
          :: String

This isn't going to typecheck, however, since GHC doesn't know what to
instantiate the type variable `a` with in the call to `f` in the method body.
(Note that `f :: forall a. String`!) To compensate for the possibility of
ambiguity here, we explicitly instantiate `a` like so:

  instance C T where
    f = coerce @String @String (f @())
          :: String

All better now.
-}

gen_Newtype_binds :: SrcSpan
                  -> Class   -- the class being derived
                  -> [TyVar] -- the tvs in the instance head (this includes
                             -- the tvs from both the class types and the
                             -- newtype itself)
                  -> [Type]  -- instance head parameters (incl. newtype)
                  -> Type    -- the representation type
                  -> TcM (LHsBinds GhcPs, BagDerivStuff)
-- See Note [Newtype-deriving instances]
gen_Newtype_binds loc cls inst_tvs inst_tys rhs_ty
  = do let ats = classATs cls
       atf_insts <- ASSERT( all (not . isDataFamilyTyCon) ats )
                    mapM mk_atf_inst ats
       return ( listToBag $ map mk_bind (classMethods cls)
              , listToBag $ map DerivFamInst atf_insts )
  where
    mk_bind :: Id -> LHsBind GhcPs
    mk_bind meth_id
      = mkRdrFunBind (L loc meth_RDR) [mkSimpleMatch
                                          (mkPrefixFunRhs (L loc meth_RDR))
                                          [] rhs_expr]
      where
        Pair from_ty to_ty = mkCoerceClassMethEqn cls inst_tvs inst_tys rhs_ty meth_id
        (_, _, from_tau) = tcSplitSigmaTy from_ty
        (_, _, to_tau)   = tcSplitSigmaTy to_ty

        meth_RDR = getRdrName meth_id

        rhs_expr = nlHsVar (getRdrName coerceId)
                                      `nlHsAppType`     from_tau
                                      `nlHsAppType`     to_tau
                                      `nlHsApp`         meth_app
                                      `nlExprWithTySig` to_ty

        -- The class method, applied to all of the class instance types
        -- (including the representation type) to avoid potential ambiguity.
        -- See Note [GND and ambiguity]
        meth_app = foldl' nlHsAppType (nlHsVar meth_RDR) $
                   filterOutInferredTypes (classTyCon cls) underlying_inst_tys
                     -- Filter out any inferred arguments, since they can't be
                     -- applied with visible type application.

    mk_atf_inst :: TyCon -> TcM FamInst
    mk_atf_inst fam_tc = do
        rep_tc_name <- newFamInstTyConName (L loc (tyConName fam_tc))
                                           rep_lhs_tys
        let axiom = mkSingleCoAxiom Nominal rep_tc_name rep_tvs' [] rep_cvs'
                                    fam_tc rep_lhs_tys rep_rhs_ty
        -- Check (c) from Note [GND and associated type families] in TcDeriv
        checkValidCoAxBranch fam_tc (coAxiomSingleBranch axiom)
        newFamInst SynFamilyInst axiom
      where
        cls_tvs     = classTyVars cls
        in_scope    = mkInScopeSet $ mkVarSet inst_tvs
        lhs_env     = zipTyEnv cls_tvs inst_tys
        lhs_subst   = mkTvSubst in_scope lhs_env
        rhs_env     = zipTyEnv cls_tvs underlying_inst_tys
        rhs_subst   = mkTvSubst in_scope rhs_env
        fam_tvs     = tyConTyVars fam_tc
        rep_lhs_tys = substTyVars lhs_subst fam_tvs
        rep_rhs_tys = substTyVars rhs_subst fam_tvs
        rep_rhs_ty  = mkTyConApp fam_tc rep_rhs_tys
        rep_tcvs    = tyCoVarsOfTypesList rep_lhs_tys
        (rep_tvs, rep_cvs) = partition isTyVar rep_tcvs
        rep_tvs'    = scopedSort rep_tvs
        rep_cvs'    = scopedSort rep_cvs

    -- Same as inst_tys, but with the last argument type replaced by the
    -- representation type.
    underlying_inst_tys :: [Type]
    underlying_inst_tys = changeLast inst_tys rhs_ty

nlHsAppType :: LHsExpr GhcPs -> Type -> LHsExpr GhcPs
nlHsAppType e s = noLoc (HsAppType noExtField e hs_ty)
  where
    hs_ty = mkHsWildCardBndrs $ parenthesizeHsType appPrec (typeToLHsType s)

nlExprWithTySig :: LHsExpr GhcPs -> Type -> LHsExpr GhcPs
nlExprWithTySig e s = noLoc $ ExprWithTySig noExtField (parenthesizeHsExpr sigPrec e) hs_ty
  where
    hs_ty = mkLHsSigWcType (typeToLHsType s)

mkCoerceClassMethEqn :: Class   -- the class being derived
                     -> [TyVar] -- the tvs in the instance head (this includes
                                -- the tvs from both the class types and the
                                -- newtype itself)
                     -> [Type]  -- instance head parameters (incl. newtype)
                     -> Type    -- the representation type
                     -> Id      -- the method to look at
                     -> Pair Type
-- See Note [Newtype-deriving instances]
-- See also Note [Newtype-deriving trickiness]
-- The pair is the (from_type, to_type), where to_type is
-- the type of the method we are trying to get
mkCoerceClassMethEqn cls inst_tvs inst_tys rhs_ty id
  = Pair (substTy rhs_subst user_meth_ty)
         (substTy lhs_subst user_meth_ty)
  where
    cls_tvs = classTyVars cls
    in_scope = mkInScopeSet $ mkVarSet inst_tvs
    lhs_subst = mkTvSubst in_scope (zipTyEnv cls_tvs inst_tys)
    rhs_subst = mkTvSubst in_scope (zipTyEnv cls_tvs (changeLast inst_tys rhs_ty))
    (_class_tvs, _class_constraint, user_meth_ty)
      = tcSplitMethodTy (varType id)

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

genAuxBindSpec :: DynFlags -> SrcSpan -> AuxBindSpec
                  -> (LHsBind GhcPs, LSig GhcPs)
genAuxBindSpec dflags loc (DerivCon2Tag tycon)
  = (mkFunBindSE 0 loc rdr_name eqns,
     L loc (TypeSig noExtField [L loc rdr_name] sig_ty))
  where
    rdr_name = con2tag_RDR dflags tycon

    sig_ty = mkLHsSigWcType $ L loc $ XHsType $ NHsCoreTy $
             mkSpecSigmaTy (tyConTyVars tycon) (tyConStupidTheta tycon) $
             mkParentType tycon `mkVisFunTy` intPrimTy

    lots_of_constructors = tyConFamilySize tycon > 8
                        -- was: mAX_FAMILY_SIZE_FOR_VEC_RETURNS
                        -- but we don't do vectored returns any more.

    eqns | lots_of_constructors = [get_tag_eqn]
         | otherwise = map mk_eqn (tyConDataCons tycon)

    get_tag_eqn = ([nlVarPat a_RDR], nlHsApp (nlHsVar getTag_RDR) a_Expr)

    mk_eqn :: DataCon -> ([LPat GhcPs], LHsExpr GhcPs)
    mk_eqn con = ([nlWildConPat con],
                  nlHsLit (HsIntPrim NoSourceText
                                    (toInteger ((dataConTag con) - fIRST_TAG))))

genAuxBindSpec dflags loc (DerivTag2Con tycon)
  = (mkFunBindSE 0 loc rdr_name
        [([nlConVarPat intDataCon_RDR [a_RDR]],
           nlHsApp (nlHsVar tagToEnum_RDR) a_Expr)],
     L loc (TypeSig noExtField [L loc rdr_name] sig_ty))
  where
    sig_ty = mkLHsSigWcType $ L loc $
             XHsType $ NHsCoreTy $ mkSpecForAllTys (tyConTyVars tycon) $
             intTy `mkVisFunTy` mkParentType tycon

    rdr_name = tag2con_RDR dflags tycon

genAuxBindSpec dflags loc (DerivMaxTag tycon)
  = (mkHsVarBind loc rdr_name rhs,
     L loc (TypeSig noExtField [L loc rdr_name] sig_ty))
  where
    rdr_name = maxtag_RDR dflags tycon
    sig_ty = mkLHsSigWcType (L loc (XHsType (NHsCoreTy intTy)))
    rhs = nlHsApp (nlHsVar intDataCon_RDR)
                  (nlHsLit (HsIntPrim NoSourceText max_tag))
    max_tag =  case (tyConDataCons tycon) of
                 data_cons -> toInteger ((length data_cons) - fIRST_TAG)

type SeparateBagsDerivStuff =
  -- AuxBinds and SYB bindings
  ( Bag (LHsBind GhcPs, LSig GhcPs)
  -- Extra family instances (used by Generic and DeriveAnyClass)
  , Bag (FamInst) )

genAuxBinds :: DynFlags -> SrcSpan -> BagDerivStuff -> SeparateBagsDerivStuff
genAuxBinds dflags loc b = genAuxBinds' b2 where
  (b1,b2) = partitionBagWith splitDerivAuxBind b
  splitDerivAuxBind (DerivAuxBind x) = Left x
  splitDerivAuxBind  x               = Right x

  rm_dups = foldr dup_check emptyBag
  dup_check a b = if anyBag (== a) b then b else consBag a b

  genAuxBinds' :: BagDerivStuff -> SeparateBagsDerivStuff
  genAuxBinds' = foldr f ( mapBag (genAuxBindSpec dflags loc) (rm_dups b1)
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

-- | Make a function binding. If no equations are given, produce a function
-- with the given arity that produces a stock error.
mkFunBindSE :: Arity -> SrcSpan -> RdrName
             -> [([LPat GhcPs], LHsExpr GhcPs)]
             -> LHsBind GhcPs
mkFunBindSE arity loc fun pats_and_exprs
  = mkRdrFunBindSE arity (L loc fun) matches
  where
    matches = [mkMatch (mkPrefixFunRhs (L loc fun))
                               (map (parenthesizePat appPrec) p) e
                               (noLoc emptyLocalBinds)
              | (p,e) <-pats_and_exprs]

mkRdrFunBind :: Located RdrName -> [LMatch GhcPs (LHsExpr GhcPs)]
             -> LHsBind GhcPs
mkRdrFunBind fun@(L loc _fun_rdr) matches
  = L loc (mkFunBind Generated fun matches)

-- | Make a function binding. If no equations are given, produce a function
-- with the given arity that uses an empty case expression for the last
-- argument that is passes to the given function to produce the right-hand
-- side.
mkFunBindEC :: Arity -> SrcSpan -> RdrName
            -> (LHsExpr GhcPs -> LHsExpr GhcPs)
            -> [([LPat GhcPs], LHsExpr GhcPs)]
            -> LHsBind GhcPs
mkFunBindEC arity loc fun catch_all pats_and_exprs
  = mkRdrFunBindEC arity catch_all (L loc fun) matches
  where
    matches = [ mkMatch (mkPrefixFunRhs (L loc fun))
                                (map (parenthesizePat appPrec) p) e
                                (noLoc emptyLocalBinds)
              | (p,e) <- pats_and_exprs ]

-- | Produces a function binding. When no equations are given, it generates
-- a binding of the given arity and an empty case expression
-- for the last argument that it passes to the given function to produce
-- the right-hand side.
mkRdrFunBindEC :: Arity
               -> (LHsExpr GhcPs -> LHsExpr GhcPs)
               -> Located RdrName
               -> [LMatch GhcPs (LHsExpr GhcPs)]
               -> LHsBind GhcPs
mkRdrFunBindEC arity catch_all
                 fun@(L loc _fun_rdr) matches = L loc (mkFunBind Generated fun matches')
 where
   -- Catch-all eqn looks like
   --     fmap _ z = case z of {}
   -- or
   --     traverse _ z = pure (case z of)
   -- or
   --     foldMap _ z = mempty
   -- It's needed if there no data cons at all,
   -- which can happen with -XEmptyDataDecls
   -- See #4302
   matches' = if null matches
              then [mkMatch (mkPrefixFunRhs fun)
                            (replicate (arity - 1) nlWildPat ++ [z_Pat])
                            (catch_all $ nlHsCase z_Expr [])
                            (noLoc emptyLocalBinds)]
              else matches

-- | Produces a function binding. When there are no equations, it generates
-- a binding with the given arity that produces an error based on the name of
-- the type of the last argument.
mkRdrFunBindSE :: Arity -> Located RdrName ->
                    [LMatch GhcPs (LHsExpr GhcPs)] -> LHsBind GhcPs
mkRdrFunBindSE arity
                 fun@(L loc fun_rdr) matches = L loc (mkFunBind Generated fun matches')
 where
   -- Catch-all eqn looks like
   --     compare _ _ = error "Void compare"
   -- It's needed if there no data cons at all,
   -- which can happen with -XEmptyDataDecls
   -- See #4302
   matches' = if null matches
              then [mkMatch (mkPrefixFunRhs fun)
                            (replicate arity nlWildPat)
                            (error_Expr str) (noLoc emptyLocalBinds)]
              else matches
   str = "Void " ++ occNameString (rdrNameOcc fun_rdr)


box ::         String           -- The class involved
            -> LHsExpr GhcPs    -- The argument
            -> Type             -- The argument type
            -> LHsExpr GhcPs    -- Boxed version of the arg
-- See Note [Deriving and unboxed types] in TcDerivInfer
box cls_str arg arg_ty = assoc_ty_id cls_str boxConTbl arg_ty arg

---------------------
primOrdOps :: String    -- The class involved
           -> Type      -- The type
           -> (RdrName, RdrName, RdrName, RdrName, RdrName)  -- (lt,le,eq,ge,gt)
-- See Note [Deriving and unboxed types] in TcDerivInfer
primOrdOps str ty = assoc_ty_id str ordOpTbl ty

ordOpTbl :: [(Type, (RdrName, RdrName, RdrName, RdrName, RdrName))]
ordOpTbl
 =  [(charPrimTy  , (ltChar_RDR  , leChar_RDR
     , eqChar_RDR  , geChar_RDR  , gtChar_RDR  ))
    ,(intPrimTy   , (ltInt_RDR   , leInt_RDR
     , eqInt_RDR   , geInt_RDR   , gtInt_RDR   ))
    ,(int8PrimTy  , (ltInt8_RDR  , leInt8_RDR
     , eqInt8_RDR  , geInt8_RDR  , gtInt8_RDR   ))
    ,(int16PrimTy , (ltInt16_RDR , leInt16_RDR
     , eqInt16_RDR , geInt16_RDR , gtInt16_RDR   ))
    ,(wordPrimTy  , (ltWord_RDR  , leWord_RDR
     , eqWord_RDR  , geWord_RDR  , gtWord_RDR  ))
    ,(word8PrimTy , (ltWord8_RDR , leWord8_RDR
     , eqWord8_RDR , geWord8_RDR , gtWord8_RDR   ))
    ,(word16PrimTy, (ltWord16_RDR, leWord16_RDR
     , eqWord16_RDR, geWord16_RDR, gtWord16_RDR  ))
    ,(addrPrimTy  , (ltAddr_RDR  , leAddr_RDR
     , eqAddr_RDR  , geAddr_RDR  , gtAddr_RDR  ))
    ,(floatPrimTy , (ltFloat_RDR , leFloat_RDR
     , eqFloat_RDR , geFloat_RDR , gtFloat_RDR ))
    ,(doublePrimTy, (ltDouble_RDR, leDouble_RDR
     , eqDouble_RDR, geDouble_RDR, gtDouble_RDR)) ]

-- A mapping from a primitive type to a function that constructs its boxed
-- version.
-- NOTE: Int8#/Word8# will become Int/Word.
boxConTbl :: [(Type, LHsExpr GhcPs -> LHsExpr GhcPs)]
boxConTbl =
    [ (charPrimTy  , nlHsApp (nlHsVar $ getRdrName charDataCon))
    , (intPrimTy   , nlHsApp (nlHsVar $ getRdrName intDataCon))
    , (wordPrimTy  , nlHsApp (nlHsVar $ getRdrName wordDataCon ))
    , (floatPrimTy , nlHsApp (nlHsVar $ getRdrName floatDataCon ))
    , (doublePrimTy, nlHsApp (nlHsVar $ getRdrName doubleDataCon))
    , (int8PrimTy,
        nlHsApp (nlHsVar $ getRdrName intDataCon)
        . nlHsApp (nlHsVar extendInt8_RDR))
    , (word8PrimTy,
        nlHsApp (nlHsVar $ getRdrName wordDataCon)
        .  nlHsApp (nlHsVar extendWord8_RDR))
    , (int16PrimTy,
        nlHsApp (nlHsVar $ getRdrName intDataCon)
        . nlHsApp (nlHsVar extendInt16_RDR))
    , (word16PrimTy,
        nlHsApp (nlHsVar $ getRdrName wordDataCon)
        .  nlHsApp (nlHsVar extendWord16_RDR))
    ]


-- | A table of postfix modifiers for unboxed values.
postfixModTbl :: [(Type, String)]
postfixModTbl
  = [(charPrimTy  , "#" )
    ,(intPrimTy   , "#" )
    ,(wordPrimTy  , "##")
    ,(floatPrimTy , "#" )
    ,(doublePrimTy, "##")
    ,(int8PrimTy, "#")
    ,(word8PrimTy, "##")
    ,(int16PrimTy, "#")
    ,(word16PrimTy, "##")
    ]

primConvTbl :: [(Type, String)]
primConvTbl =
    [ (int8PrimTy, "narrowInt8#")
    , (word8PrimTy, "narrowWord8#")
    , (int16PrimTy, "narrowInt16#")
    , (word16PrimTy, "narrowWord16#")
    ]

litConTbl :: [(Type, LHsExpr GhcPs -> LHsExpr GhcPs)]
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
assoc_ty_id :: HasCallStack => String           -- The class involved
            -> [(Type,a)]       -- The table
            -> Type             -- The type
            -> a                -- The result of the lookup
assoc_ty_id cls_str tbl ty
  | Just a <- assoc_ty_id_maybe tbl ty = a
  | otherwise =
      pprPanic "Error in deriving:"
          (text "Can't derive" <+> text cls_str <+>
           text "for primitive type" <+> ppr ty)

-- | Lookup `Type` in an association list.
assoc_ty_id_maybe :: [(Type, a)] -> Type -> Maybe a
assoc_ty_id_maybe tbl ty = snd <$> find (\(t, _) -> t `eqType` ty) tbl

-----------------------------------------------------------------------

and_Expr :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
and_Expr a b = genOpApp a and_RDR    b

-----------------------------------------------------------------------

eq_Expr :: Type -> LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
eq_Expr ty a b
    | not (isUnliftedType ty) = genOpApp a eq_RDR b
    | otherwise               = genPrimOpApp a prim_eq b
 where
   (_, _, prim_eq, _, _) = primOrdOps "Eq" ty

untag_Expr :: DynFlags -> TyCon -> [( RdrName,  RdrName)]
              -> LHsExpr GhcPs -> LHsExpr GhcPs
untag_Expr _ _ [] expr = expr
untag_Expr dflags tycon ((untag_this, put_tag_here) : more) expr
  = nlHsCase (nlHsPar (nlHsVarApps (con2tag_RDR dflags tycon)
                                   [untag_this])) {-of-}
      [mkHsCaseAlt (nlVarPat put_tag_here) (untag_Expr dflags tycon more expr)]

enum_from_to_Expr
        :: LHsExpr GhcPs -> LHsExpr GhcPs
        -> LHsExpr GhcPs
enum_from_then_to_Expr
        :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
        -> LHsExpr GhcPs

enum_from_to_Expr      f   t2 = nlHsApp (nlHsApp (nlHsVar enumFromTo_RDR) f) t2
enum_from_then_to_Expr f t t2 = nlHsApp (nlHsApp (nlHsApp (nlHsVar enumFromThenTo_RDR) f) t) t2

showParen_Expr
        :: LHsExpr GhcPs -> LHsExpr GhcPs
        -> LHsExpr GhcPs

showParen_Expr e1 e2 = nlHsApp (nlHsApp (nlHsVar showParen_RDR) e1) e2

nested_compose_Expr :: [LHsExpr GhcPs] -> LHsExpr GhcPs

nested_compose_Expr []  = panic "nested_compose_expr"   -- Arg is always non-empty
nested_compose_Expr [e] = parenify e
nested_compose_Expr (e:es)
  = nlHsApp (nlHsApp (nlHsVar compose_RDR) (parenify e)) (nested_compose_Expr es)

-- impossible_Expr is used in case RHSs that should never happen.
-- We generate these to keep the desugarer from complaining that they *might* happen!
error_Expr :: String -> LHsExpr GhcPs
error_Expr string = nlHsApp (nlHsVar error_RDR) (nlHsLit (mkHsString string))

-- illegal_Expr is used when signalling error conditions in the RHS of a derived
-- method. It is currently only used by Enum.{succ,pred}
illegal_Expr :: String -> String -> String -> LHsExpr GhcPs
illegal_Expr meth tp msg =
   nlHsApp (nlHsVar error_RDR) (nlHsLit (mkHsString (meth ++ '{':tp ++ "}: " ++ msg)))

-- illegal_toEnum_tag is an extended version of illegal_Expr, which also allows you
-- to include the value of a_RDR in the error string.
illegal_toEnum_tag :: String -> RdrName -> LHsExpr GhcPs
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

parenify :: LHsExpr GhcPs -> LHsExpr GhcPs
parenify e@(L _ (HsVar _ _)) = e
parenify e                   = mkHsPar e

-- genOpApp wraps brackets round the operator application, so that the
-- renamer won't subsequently try to re-associate it.
genOpApp :: LHsExpr GhcPs -> RdrName -> LHsExpr GhcPs -> LHsExpr GhcPs
genOpApp e1 op e2 = nlHsPar (nlHsOpApp e1 op e2)

genPrimOpApp :: LHsExpr GhcPs -> RdrName -> LHsExpr GhcPs -> LHsExpr GhcPs
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

a_Expr, b_Expr, c_Expr, z_Expr, ltTag_Expr, eqTag_Expr, gtTag_Expr, false_Expr,
    true_Expr, pure_Expr :: LHsExpr GhcPs
a_Expr          = nlHsVar a_RDR
b_Expr          = nlHsVar b_RDR
c_Expr          = nlHsVar c_RDR
z_Expr          = nlHsVar z_RDR
ltTag_Expr      = nlHsVar ltTag_RDR
eqTag_Expr      = nlHsVar eqTag_RDR
gtTag_Expr      = nlHsVar gtTag_RDR
false_Expr      = nlHsVar false_RDR
true_Expr       = nlHsVar true_RDR
pure_Expr       = nlHsVar pure_RDR

a_Pat, b_Pat, c_Pat, d_Pat, k_Pat, z_Pat :: LPat GhcPs
a_Pat           = nlVarPat a_RDR
b_Pat           = nlVarPat b_RDR
c_Pat           = nlVarPat c_RDR
d_Pat           = nlVarPat d_RDR
k_Pat           = nlVarPat k_RDR
z_Pat           = nlVarPat z_RDR

minusInt_RDR, tagToEnum_RDR :: RdrName
minusInt_RDR  = getRdrName (primOpId IntSubOp   )
tagToEnum_RDR = getRdrName (primOpId TagToEnumOp)

con2tag_RDR, tag2con_RDR, maxtag_RDR :: DynFlags -> TyCon -> RdrName
-- Generates Orig s RdrName, for the binding positions
con2tag_RDR dflags tycon = mk_tc_deriv_name dflags tycon mkCon2TagOcc
tag2con_RDR dflags tycon = mk_tc_deriv_name dflags tycon mkTag2ConOcc
maxtag_RDR  dflags tycon = mk_tc_deriv_name dflags tycon mkMaxTagOcc

mk_tc_deriv_name :: DynFlags -> TyCon -> (OccName -> OccName) -> RdrName
mk_tc_deriv_name dflags tycon occ_fun =
   mkAuxBinderName dflags (tyConName tycon) occ_fun

mkAuxBinderName :: DynFlags -> Name -> (OccName -> OccName) -> RdrName
-- ^ Make a top-level binder name for an auxiliary binding for a parent name
-- See Note [Auxiliary binders]
mkAuxBinderName dflags parent occ_fun
  = mkRdrUnqual (occ_fun stable_parent_occ)
  where
    stable_parent_occ = mkOccName (occNameSpace parent_occ) stable_string
    stable_string
      | hasPprDebug dflags = parent_stable
      | otherwise          = parent_stable_hash
    parent_stable = nameStableString parent
    parent_stable_hash =
      let Fingerprint high low = fingerprintString parent_stable
      in toBase62 high ++ toBase62Padded low
      -- See Note [Base 62 encoding 128-bit integers] in Encoding
    parent_occ  = nameOccName parent


{-
Note [Auxiliary binders]
~~~~~~~~~~~~~~~~~~~~~~~~
We often want to make a top-level auxiliary binding.  E.g. for comparison we have

  instance Ord T where
    compare a b = $con2tag a `compare` $con2tag b

  $con2tag :: T -> Int
  $con2tag = ...code....

Of course these top-level bindings should all have distinct name, and we are
generating RdrNames here.  We can't just use the TyCon or DataCon to distinguish
because with standalone deriving two imported TyCons might both be called T!
(See #7947.)

So we use package name, module name and the name of the parent
(T in this example) as part of the OccName we generate for the new binding.
To make the symbol names short we take a base62 hash of the full name.

In the past we used the *unique* from the parent, but that's not stable across
recompilations as uniques are nondeterministic.
-}
