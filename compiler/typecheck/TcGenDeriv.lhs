    %
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

TcGenDeriv: Generating derived instance declarations

This module is nominally ``subordinate'' to @TcDeriv@, which is the
``official'' interface to deriving-related things.

This is where we do all the grimy bindings' generation.

\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module TcGenDeriv (
        BagDerivStuff, DerivStuff(..),

        gen_Bounded_binds,
        gen_Enum_binds,
        gen_Eq_binds,
        gen_Ix_binds,
        gen_Ord_binds,
        gen_Read_binds,
        gen_Show_binds,
        gen_Data_binds,
        gen_old_Typeable_binds, gen_Typeable_binds,
        gen_Functor_binds,
        FFoldType(..), functorLikeTraverse,
        deepSubtypesContaining, foldDataConArgs,
        gen_Foldable_binds,
        gen_Traversable_binds,
        mkCoerceClassMethEqn,
        gen_Newtype_binds,
        genAuxBinds,
        ordOpTbl, boxConTbl,
        mkRdrFunBind
    ) where

#include "HsVersions.h"

import HsSyn
import RdrName
import BasicTypes
import DataCon
import Name

import DynFlags
import HscTypes
import PrelInfo
import FamInstEnv( FamInst )
import MkCore ( eRROR_ID )
import PrelNames hiding (error_RDR)
import MkId ( coerceId )
import PrimOp
import SrcLoc
import TyCon
import TcType
import TysPrim
import TysWiredIn
import Type
import Class
import TypeRep
import VarSet
import VarEnv
import Module
import State
import Util
import Var
import MonadUtils
import Outputable
import FastString
import Pair
import Bag
import Fingerprint
import TcEnv (InstInfo)

import Data.List ( partition, intersperse )
\end{code}

\begin{code}
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

  -- Generics
  | DerivTyCon TyCon                   -- New data types
  | DerivFamInst (FamInst)             -- New type family instances

  -- New top-level auxiliary bindings
  | DerivHsBind ((Origin, LHsBind RdrName), LSig RdrName) -- Also used for SYB
  | DerivInst (InstInfo RdrName)                -- New, auxiliary instances
\end{code}


%************************************************************************
%*                                                                      *
                Eq instances
%*                                                                      *
%************************************************************************

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

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
        Ord instances
%*                                                                      *
%************************************************************************

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

Note [Do not rely on compare]
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

So for sufficiently small types (few constructors, or all nullary)
we generate all methods; for large ones we just use 'compare'.

\begin{code}
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

        -- Note [Do not rely on compare]
    other_ops | (last_tag - first_tag) <= 2     -- 1-3 constructors
                || null non_nullary_cons        -- Or it's an enumeration
              = listToBag (map mkOrdOp [OrdLT,OrdLE,OrdGE,OrdGT])
              | otherwise
              = emptyBag

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


    mkOrdOp :: OrdOp -> (Origin, LHsBind RdrName)
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
         ++ [mkSimpleHsAlt nlWildPat (mkTagCmp op)])


    mkOrdOpAlt :: OrdOp -> DataCon -> LMatch RdrName (LHsExpr RdrName)
    -- Make the alternative  (Ki a1 a2 .. av ->
    mkOrdOpAlt op data_con
      = mkSimpleHsAlt (nlConVarPat data_con_RDR as_needed) (mkInnerRhs op data_con)
      where
        as_needed    = take (dataConSourceArity data_con) as_RDRs
        data_con_RDR = getRdrName data_con

    mkInnerRhs op data_con
      | single_con_type
      = nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con ]

      | tag == first_tag
      = nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con
                                 , mkSimpleHsAlt nlWildPat (ltResult op) ]
      | tag == last_tag
      = nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con
                                 , mkSimpleHsAlt nlWildPat (gtResult op) ]

      | tag == first_tag + 1
      = nlHsCase (nlHsVar b_RDR) [ mkSimpleHsAlt (nlConWildPat first_con) (gtResult op)
                                 , mkInnerEqAlt op data_con
                                 , mkSimpleHsAlt nlWildPat (ltResult op) ]
      | tag == last_tag - 1
      = nlHsCase (nlHsVar b_RDR) [ mkSimpleHsAlt (nlConWildPat last_con) (ltResult op)
                                 , mkInnerEqAlt op data_con
                                 , mkSimpleHsAlt nlWildPat (gtResult op) ]

      | tag > last_tag `div` 2  -- lower range is larger
      = untag_Expr tycon [(b_RDR, bh_RDR)] $
        nlHsIf (genPrimOpApp (nlHsVar bh_RDR) ltInt_RDR tag_lit)
               (gtResult op) $  -- Definitely GT
        nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con
                                 , mkSimpleHsAlt nlWildPat (ltResult op) ]

      | otherwise               -- upper range is larger
      = untag_Expr tycon [(b_RDR, bh_RDR)] $
        nlHsIf (genPrimOpApp (nlHsVar bh_RDR) gtInt_RDR tag_lit)
               (ltResult op) $  -- Definitely LT
        nlHsCase (nlHsVar b_RDR) [ mkInnerEqAlt op data_con
                                 , mkSimpleHsAlt nlWildPat (gtResult op) ]
      where
        tag     = get_tag data_con
        tag_lit = noLoc (HsLit (HsIntPrim (toInteger tag)))

    mkInnerEqAlt :: OrdOp -> DataCon -> LMatch RdrName (LHsExpr RdrName)
    -- First argument 'a' known to be built with K
    -- Returns a case alternative  Ki b1 b2 ... bv -> compare (a1,a2,...) with (b1,b2,...)
    mkInnerEqAlt op data_con
      = mkSimpleHsAlt (nlConVarPat data_con_RDR bs_needed) $
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
      | isUnLiftedType ty     = unliftedOrdOp tycon ty op a b
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
      | isUnLiftedType ty
      = unliftedCompare lt_op eq_op a_expr b_expr lt eq gt
      | otherwise
      = nlHsCase (nlHsPar (nlHsApp (nlHsApp (nlHsVar compare_RDR) a_expr) b_expr))
          [mkSimpleHsAlt (nlNullaryConPat ltTag_RDR) lt,
           mkSimpleHsAlt (nlNullaryConPat eqTag_RDR) eq,
           mkSimpleHsAlt (nlNullaryConPat gtTag_RDR) gt]
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
  = nlHsIf (genPrimOpApp a_expr lt_op b_expr) lt $
                        -- Test (<) first, not (==), because the latter
                        -- is true less often, so putting it first would
                        -- mean more tests (dynamically)
        nlHsIf (genPrimOpApp a_expr eq_op b_expr) eq gt

nlConWildPat :: DataCon -> LPat RdrName
-- The pattern (K {})
nlConWildPat con = noLoc (ConPatIn (noLoc (getRdrName con))
                                   (RecCon (HsRecFields { rec_flds = []
                                                        , rec_dotdot = Nothing })))
\end{code}



%************************************************************************
%*                                                                      *
        Enum instances
%*                                                                      *
%************************************************************************

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

\begin{code}
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
                                               nlHsLit (HsInt (-1))]))

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
\end{code}

%************************************************************************
%*                                                                      *
        Bounded instances
%*                                                                      *
%************************************************************************

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
        Ix instances
%*                                                                      *
%************************************************************************

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

\begin{code}
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
             [mkSimpleHsAlt (nlVarPat c_RDR) rhs]
           ))
        )

    enum_inRange
      = mk_easy_FunBind loc inRange_RDR [nlTuplePat [a_Pat, b_Pat] Boxed, c_Pat] $
          untag_Expr tycon [(a_RDR, ah_RDR)] (
          untag_Expr tycon [(b_RDR, bh_RDR)] (
          untag_Expr tycon [(c_RDR, ch_RDR)] (
          nlHsIf (genPrimOpApp (nlHsVar ch_RDR) geInt_RDR (nlHsVar ah_RDR)) (
             (genPrimOpApp (nlHsVar ch_RDR) leInt_RDR (nlHsVar bh_RDR))
          ) {-else-} (
             false_Expr
          ))))

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
\end{code}

%************************************************************************
%*                                                                      *
        Read instances
%*                                                                      *
%************************************************************************

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

\begin{code}
gen_Read_binds :: FixityEnv -> SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)

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
        labels       = dataConFieldLabels data_con
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

    read_arg a ty = ASSERT( not (isUnLiftedType ty) )
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
                   lbl_str = occNameString (getOccName lbl)
\end{code}


%************************************************************************
%*                                                                      *
        Show instances
%*                                                                      *
%************************************************************************

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

\begin{code}
gen_Show_binds :: FixityEnv -> SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)

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
          showParen_Expr (nlHsPar (genOpApp a_Expr ge_RDR (nlHsLit (HsInt con_prec_plus_one))))
                         (nlHsPar (nested_compose_Expr show_thingies)))
        where
             data_con_RDR  = getRdrName data_con
             con_arity     = dataConSourceArity data_con
             bs_needed     = take con_arity bs_RDRs
             arg_tys       = dataConOrigArgTys data_con         -- Correspond 1-1 with bs_needed
             con_pat       = nlConVarPat data_con_RDR bs_needed
             nullary_con   = con_arity == 0
             labels        = dataConFieldLabels data_con
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
                   occ_nm   = getOccName l
                   nm       = wrapOpParens (occNameString occ_nm)

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

                -- Generates (showsPrec p x) for argument x, but it also boxes
                -- the argument first if necessary.  Note that this prints unboxed
                -- things without any '#' decorations; could change that if need be
             show_arg b arg_ty = nlHsApps showsPrec_RDR [nlHsLit (HsInt arg_prec),
                                                         box_if_necy "Show" tycon (nlHsVar b) arg_ty]

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

mk_showString_app :: String -> LHsExpr RdrName
mk_showString_app str = nlHsApp (nlHsVar showString_RDR) (nlHsLit (mkHsString str))
\end{code}

\begin{code}
getPrec :: Bool -> FixityEnv -> Name -> Integer
getPrec is_infix get_fixity nm
  | not is_infix   = appPrecedence
  | otherwise      = getPrecedence get_fixity nm

appPrecedence :: Integer
appPrecedence = fromIntegral maxPrecedence + 1
  -- One more than the precedence of the most
  -- tightly-binding operator

getPrecedence :: FixityEnv -> Name -> Integer
getPrecedence get_fixity nm
   = case lookupFixity get_fixity nm of
        Fixity x _assoc -> fromIntegral x
          -- NB: the Report says that associativity is not taken
          --     into account for either Read or Show; hence we
          --     ignore associativity here
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Typeable (old)}
%*                                                                      *
%************************************************************************

From the data type

        data T a b = ....

we generate

        instance Typeable2 T where
                typeOf2 _ = mkTyConApp (mkTyCon <hash-high> <hash-low>
                                                <pkg> <module> "T") []

We are passed the Typeable2 class as well as T

\begin{code}
gen_old_Typeable_binds :: DynFlags -> SrcSpan -> TyCon -> LHsBinds RdrName
gen_old_Typeable_binds dflags loc tycon
  = unitBag $
        mk_easy_FunBind loc
                (old_mk_typeOf_RDR tycon)   -- Name of appropriate type0f function
                [nlWildPat]
                (nlHsApps oldMkTyConApp_RDR [tycon_rep, nlList []])
  where
    tycon_name = tyConName tycon
    modl       = nameModule tycon_name
    pkg        = modulePackageId modl

    modl_fs    = moduleNameFS (moduleName modl)
    pkg_fs     = packageIdFS pkg
    name_fs    = occNameFS (nameOccName tycon_name)

    tycon_rep = nlHsApps oldMkTyCon_RDR
                    (map nlHsLit [int64 high,
                                  int64 low,
                                  HsString pkg_fs,
                                  HsString modl_fs,
                                  HsString name_fs])

    hashThis = unwords $ map unpackFS [pkg_fs, modl_fs, name_fs]
    Fingerprint high low = fingerprintString hashThis

    int64
      | wORD_SIZE dflags == 4 = HsWord64Prim . fromIntegral
      | otherwise             = HsWordPrim . fromIntegral


old_mk_typeOf_RDR :: TyCon -> RdrName
-- Use the arity of the TyCon to make the right typeOfn function
old_mk_typeOf_RDR tycon = varQual_RDR oLDTYPEABLE_INTERNAL (mkFastString ("typeOf" ++ suffix))
                where
                  arity = tyConArity tycon
                  suffix | arity == 0 = ""
                         | otherwise  = show arity
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Typeable (new)}
%*                                                                      *
%************************************************************************

From the data type

        data T a b = ....

we generate

        instance Typeable2 T where
                typeOf2 _ = mkTyConApp (mkTyCon <hash-high> <hash-low>
                                                <pkg> <module> "T") []

We are passed the Typeable2 class as well as T

\begin{code}
gen_Typeable_binds :: DynFlags -> SrcSpan -> TyCon -> LHsBinds RdrName
gen_Typeable_binds dflags loc tycon
  = unitBag $ mk_easy_FunBind loc typeRep_RDR [nlWildPat]
              (nlHsApps mkTyConApp_RDR [tycon_rep, nlList []])
  where
    tycon_name = tyConName tycon
    modl       = nameModule tycon_name
    pkg        = modulePackageId modl

    modl_fs    = moduleNameFS (moduleName modl)
    pkg_fs     = packageIdFS pkg
    name_fs    = occNameFS (nameOccName tycon_name)

    tycon_rep = nlHsApps mkTyCon_RDR
                    (map nlHsLit [int64 high,
                                  int64 low,
                                  HsString pkg_fs,
                                  HsString modl_fs,
                                  HsString name_fs])

    hashThis = unwords $ map unpackFS [pkg_fs, modl_fs, name_fs]
    Fingerprint high low = fingerprintString hashThis

    int64
      | wORD_SIZE dflags == 4 = HsWord64Prim . fromIntegral
      | otherwise             = HsWordPrim . fromIntegral
\end{code}



%************************************************************************
%*                                                                      *
        Data instances
%*                                                                      *
%************************************************************************

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


\begin{code}
gen_Data_binds :: DynFlags
                -> SrcSpan
               -> TyCon
               -> (LHsBinds RdrName,    -- The method bindings
                   BagDerivStuff)       -- Auxiliary bindings
gen_Data_binds dflags loc tycon
  = (listToBag [gfoldl_bind, gunfold_bind, toCon_bind, dataTypeOf_bind]
     `unionBags` gcast_binds,
                -- Auxiliary definitions: the data type and constructors
     listToBag ( DerivHsBind (genDataTyCon)
               : map (DerivHsBind . genDataDataCon) data_cons))
  where
    data_cons  = tyConDataCons tycon
    n_cons     = length data_cons
    one_constr = n_cons == 1

    genDataTyCon :: ((Origin, LHsBind RdrName), LSig RdrName)
    genDataTyCon        --  $dT
      = (mkHsVarBind loc rdr_name rhs,
         L loc (TypeSig [L loc rdr_name] sig_ty))
      where
        rdr_name = mk_data_type_name tycon
        sig_ty   = nlHsTyVar dataType_RDR
        constrs  = [nlHsVar (mk_constr_name con) | con <- tyConDataCons tycon]
        rhs = nlHsVar mkDataType_RDR
              `nlHsApp` nlHsLit (mkHsString (showSDocOneLine dflags (ppr tycon)))
              `nlHsApp` nlList constrs

    genDataDataCon :: DataCon -> ((Origin, LHsBind RdrName), LSig RdrName)
    genDataDataCon dc       --  $cT1 etc
      = (mkHsVarBind loc rdr_name rhs,
         L loc (TypeSig [L loc rdr_name] sig_ty))
      where
        rdr_name = mk_constr_name dc
        sig_ty   = nlHsTyVar constr_RDR
        rhs      = nlHsApps mkConstr_RDR constr_args

        constr_args
           = [ -- nlHsIntLit (toInteger (dataConTag dc)), -- Tag
           nlHsVar (mk_data_type_name (dataConTyCon dc)), -- DataType
           nlHsLit (mkHsString (occNameString dc_occ)),   -- String name
               nlList  labels,                            -- Field labels
           nlHsVar fixity]                                -- Fixity

        labels   = map (nlHsLit . mkHsString . getOccString)
                       (dataConFieldLabels dc)
        dc_occ   = getOccName dc
        is_infix = isDataSymOcc dc_occ
        fixity | is_infix  = infix_RDR
           | otherwise = prefix_RDR

        ------------ gfoldl
    gfoldl_bind = mk_FunBind loc gfoldl_RDR (map gfoldl_eqn data_cons)

    gfoldl_eqn con
      = ([nlVarPat k_RDR, nlVarPat z_RDR, nlConVarPat con_name as_needed],
                       foldl mk_k_app (nlHsVar z_RDR `nlHsApp` nlHsVar con_name) as_needed)
                   where
                     con_name ::  RdrName
                     con_name = getRdrName con
                     as_needed = take (dataConSourceArity con) as_RDRs
                     mk_k_app e v = nlHsPar (nlHsOpApp e k_RDR (nlHsVar v))

        ------------ gunfold
    gunfold_bind = mk_FunBind loc
                              gunfold_RDR
                              [([k_Pat, z_Pat, if one_constr then nlWildPat else c_Pat],
                                gunfold_rhs)]

    gunfold_rhs
        | one_constr = mk_unfold_rhs (head data_cons)   -- No need for case
        | otherwise  = nlHsCase (nlHsVar conIndex_RDR `nlHsApp` c_Expr)
                                (map gunfold_alt data_cons)

    gunfold_alt dc = mkSimpleHsAlt (mk_unfold_pat dc) (mk_unfold_rhs dc)
    mk_unfold_rhs dc = foldr nlHsApp
                           (nlHsVar z_RDR `nlHsApp` nlHsVar (getRdrName dc))
                           (replicate (dataConSourceArity dc) (nlHsVar k_RDR))

    mk_unfold_pat dc    -- Last one is a wild-pat, to avoid
                        -- redundant test, and annoying warning
      | tag-fIRST_TAG == n_cons-1 = nlWildPat   -- Last constructor
      | otherwise = nlConPat intDataCon_RDR [nlLitPat (HsIntPrim (toInteger tag))]
      where
        tag = dataConTag dc

        ------------ toConstr
    toCon_bind = mk_FunBind loc toConstr_RDR (map to_con_eqn data_cons)
    to_con_eqn dc = ([nlWildConPat dc], nlHsVar (mk_constr_name dc))

        ------------ dataTypeOf
    dataTypeOf_bind = mk_easy_FunBind
                        loc
                        dataTypeOf_RDR
                        [nlWildPat]
                        (nlHsVar (mk_data_type_name tycon))

        ------------ gcast1/2
    tycon_kind = tyConKind tycon
    gcast_binds | tycon_kind `tcEqKind` kind1 = mk_gcast dataCast1_RDR gcast1_RDR
                | tycon_kind `tcEqKind` kind2 = mk_gcast dataCast2_RDR gcast2_RDR
                | otherwise                 = emptyBag
    mk_gcast dataCast_RDR gcast_RDR
      = unitBag (mk_easy_FunBind loc dataCast_RDR [nlVarPat f_RDR]
                                 (nlHsVar gcast_RDR `nlHsApp` nlHsVar f_RDR))


kind1, kind2 :: Kind
kind1 = liftedTypeKind `mkArrowKind` liftedTypeKind
kind2 = liftedTypeKind `mkArrowKind` kind1

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
\end{code}



%************************************************************************
%*                                                                      *
                        Functor instances

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html

%*                                                                      *
%************************************************************************

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

\begin{code}
gen_Functor_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Functor_binds loc tycon
  = (unitBag fmap_bind, emptyBag)
  where
    data_cons = tyConDataCons tycon
    fmap_bind = mkRdrFunBind (L loc fmap_RDR) eqns

    fmap_eqn con = evalState (match_for_con [f_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_fmap con

    eqns | null data_cons = [mkSimpleMatch [nlWildPat, nlWildPat]
                                           (error_Expr "Void fmap")]
         | otherwise      = map fmap_eqn data_cons

    ft_fmap :: FFoldType (State [RdrName] (LHsExpr RdrName))
    ft_fmap = FT { ft_triv = mkSimpleLam $ \x -> return x    -- fmap f = \x -> x
                 , ft_var  = return f_Expr                   -- fmap f = f
                 , ft_fun  = \g h -> do                      -- fmap f = \x b -> h (x (g b))
                                 gg <- g
                                 hh <- h
                                 mkSimpleLam2 $ \x b -> return $ nlHsApp hh (nlHsApp x (nlHsApp gg b))
                 , ft_tup = \t gs -> do                      -- fmap f = \x -> case x of (a1,a2,..) -> (g1 a1,g2 a2,..)
                                 gg <- sequence gs
                                 mkSimpleLam $ mkSimpleTupleCase match_for_con t gg
                 , ft_ty_app = \_ g -> nlHsApp fmap_Expr <$> g  -- fmap f = fmap g
                 , ft_forall = \_ g -> g
                 , ft_bad_app = panic "in other argument"
                 , ft_co_var = panic "contravariant" }

    -- Con a1 a2 ... -> Con (f1 a1) (f2 a2) ...
    match_for_con :: [LPat RdrName] -> DataCon -> [LHsExpr RdrName]
                  -> State [RdrName] (LMatch RdrName (LHsExpr RdrName))
    match_for_con = mkSimpleConMatch $
        \con_name xs -> return $ nlHsApps con_name xs  -- Con x1 x2 ..
\end{code}

Utility functions related to Functor deriving.

Since several things use the same pattern of traversal, this is abstracted into functorLikeTraverse.
This function works like a fold: it makes a value of type 'a' in a bottom up way.

\begin{code}
-- Generic traversal for Functor deriving
data FFoldType a      -- Describes how to fold over a Type in a functor like way
   = FT { ft_triv    :: a                   -- Does not contain variable
        , ft_var     :: a                   -- The variable itself
        , ft_co_var  :: a                   -- The variable itself, contravariantly
        , ft_fun     :: a -> a -> a         -- Function type
        , ft_tup     :: TupleSort -> [a] -> a  -- Tuple type
        , ft_ty_app  :: Type -> a -> a      -- Type app, variable only in last argument
        , ft_bad_app :: a                   -- Type app, variable other than in last argument
        , ft_forall  :: TcTyVar -> a -> a   -- Forall type
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
       | isTupleTyCon con = (caseTuple (tupleTyConSort con) xrs, True)
       | or (init xcs)    = (caseWrongArg, True)         -- T (..var..)    ty
       | otherwise        = case splitAppTy_maybe ty of  -- T (..no var..) ty
                              Nothing -> (caseWrongArg, True)   -- Non-decomposable (eg type function)
                              Just (fun_ty, _) -> (caseTyApp fun_ty (last xrs), True)
       where
         (xrs,xcs) = unzip (map (go co) args)
    go co (ForAllTy v x) | v /= var && xc = (caseForAll v xr,True)
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
            , ft_forall = \v xs -> filterOut ((v `elemVarSet`) . tyVarsOfType) xs })


foldDataConArgs :: FFoldType a -> DataCon -> [a]
-- Fold over the arguments of the datacon
foldDataConArgs ft con
  = map (functorLikeTraverse tv ft) (dataConOrigArgTys con)
  where
    Just tv = getTyVar_maybe (last (tyConAppArgs (dataConOrigResTy con)))
        -- Argument to derive for, 'a in the above description
        -- The validity and kind checks have ensured that
        -- the Just will match and a::*

-- Make a HsLam using a fresh variable from a State monad
mkSimpleLam :: (LHsExpr id -> State [id] (LHsExpr id)) -> State [id] (LHsExpr id)
-- (mkSimpleLam fn) returns (\x. fn(x))
mkSimpleLam lam = do
    (n:names) <- get
    put names
    body <- lam (nlHsVar n)
    return (mkHsLam [nlVarPat n] body)

mkSimpleLam2 :: (LHsExpr id -> LHsExpr id -> State [id] (LHsExpr id)) -> State [id] (LHsExpr id)
mkSimpleLam2 lam = do
    (n1:n2:names) <- get
    put names
    body <- lam (nlHsVar n1) (nlHsVar n2)
    return (mkHsLam [nlVarPat n1,nlVarPat n2] body)

-- "Con a1 a2 a3 -> fold [x1 a1, x2 a2, x3 a3]"
mkSimpleConMatch :: Monad m => (RdrName -> [LHsExpr RdrName] -> m (LHsExpr RdrName))
                 -> [LPat RdrName]
                 -> DataCon
                 -> [LHsExpr RdrName]
                 -> m (LMatch RdrName (LHsExpr RdrName))
mkSimpleConMatch fold extra_pats con insides = do
    let con_name = getRdrName con
    let vars_needed = takeList insides as_RDRs
    let pat = nlConVarPat con_name vars_needed
    rhs <- fold con_name (zipWith nlHsApp insides (map nlHsVar vars_needed))
    return $ mkMatch (extra_pats ++ [pat]) rhs emptyLocalBinds

-- "case x of (a1,a2,a3) -> fold [x1 a1, x2 a2, x3 a3]"
mkSimpleTupleCase :: Monad m => ([LPat RdrName] -> DataCon -> [a]
                                 -> m (LMatch RdrName (LHsExpr RdrName)))
                  -> TupleSort -> [a] -> LHsExpr RdrName -> m (LHsExpr RdrName)
mkSimpleTupleCase match_for_con sort insides x = do
    let con = tupleCon sort (length insides)
    match <- match_for_con [] con insides
    return $ nlHsCase x [match]
\end{code}


%************************************************************************
%*                                                                      *
                        Foldable instances

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html

%*                                                                      *
%************************************************************************

Deriving Foldable instances works the same way as Functor instances,
only Foldable instances are not possible for function types at all.
Here the derived instance for the type T above is:

  instance Foldable T where
      foldr f z (T1 x1 x2 x3) = $(foldr 'a 'b1) x1 ( $(foldr 'a 'a) x2 ( $(foldr 'a 'b2) x3 z ) )

The cases are:

  $(foldr 'a 'b)         =  \x z -> z     -- when b does not contain a
  $(foldr 'a 'a)         =  f
  $(foldr 'a '(b1,b2))   =  \x z -> case x of (x1,x2) -> $(foldr 'a 'b1) x1 ( $(foldr 'a 'b2) x2 z )
  $(foldr 'a '(T b1 b2)) =  \x z -> foldr $(foldr 'a 'b2) z x  -- when a only occurs in the last parameter, b2

Note that the arguments to the real foldr function are the wrong way around,
since (f :: a -> b -> b), while (foldr f :: b -> t a -> b).

\begin{code}
gen_Foldable_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Foldable_binds loc tycon
  = (listToBag [foldr_bind, foldMap_bind], emptyBag)
  where
    data_cons = tyConDataCons tycon

    foldr_bind = mkRdrFunBind (L loc foldable_foldr_RDR) eqns
    eqns = map foldr_eqn data_cons
    foldr_eqn con = evalState (match_foldr z_Expr [f_Pat,z_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_foldr con

    foldMap_bind = mkRdrFunBind (L loc foldMap_RDR) (map foldMap_eqn data_cons)
    foldMap_eqn con = evalState (match_foldMap [f_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_foldMap con

    ft_foldr :: FFoldType (State [RdrName] (LHsExpr RdrName))
    ft_foldr = FT { ft_triv    = mkSimpleLam2 $ \_ z -> return z       -- foldr f = \x z -> z
                  , ft_var     = return f_Expr                         -- foldr f = f
                  , ft_tup     = \t g -> do gg <- sequence g           -- foldr f = (\x z -> case x of ...)
                                            mkSimpleLam2 $ \x z -> mkSimpleTupleCase (match_foldr z) t gg x
                  , ft_ty_app  = \_ g -> do gg <- g                    -- foldr f = (\x z -> foldr g z x)
                                            mkSimpleLam2 $ \x z -> return $ nlHsApps foldable_foldr_RDR [gg,z,x]
                  , ft_forall  = \_ g -> g
                  , ft_co_var  = panic "contravariant"
                  , ft_fun     = panic "function"
                  , ft_bad_app = panic "in other argument" }

    match_foldr z = mkSimpleConMatch $ \_con_name xs -> return $ foldr nlHsApp z xs -- g1 v1 (g2 v2 (.. z))

    ft_foldMap :: FFoldType (State [RdrName] (LHsExpr RdrName))
    ft_foldMap = FT { ft_triv = mkSimpleLam $ \_ -> return mempty_Expr  -- foldMap f = \x -> mempty
                    , ft_var  = return f_Expr                           -- foldMap f = f
                    , ft_tup  = \t g -> do gg <- sequence g             -- foldMap f = \x -> case x of (..,)
                                           mkSimpleLam $ mkSimpleTupleCase match_foldMap t gg
                    , ft_ty_app = \_ g -> nlHsApp foldMap_Expr <$> g    -- foldMap f = foldMap g
                    , ft_forall = \_ g -> g
                    , ft_co_var = panic "contravariant"
                    , ft_fun = panic "function"
                    , ft_bad_app = panic "in other argument" }

    match_foldMap = mkSimpleConMatch $ \_con_name xs -> return $
        case xs of
            [] -> mempty_Expr
            xs -> foldr1 (\x y -> nlHsApps mappend_RDR [x,y]) xs

\end{code}


%************************************************************************
%*                                                                      *
                        Traversable instances

 see http://www.mail-archive.com/haskell-prime@haskell.org/msg02116.html
%*                                                                      *
%************************************************************************

Again, Traversable is much like Functor and Foldable.

The cases are:

  $(traverse 'a 'b)          =  pure     -- when b does not contain a
  $(traverse 'a 'a)          =  f
  $(traverse 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> (,) <$> $(traverse 'a 'b1) x1 <*> $(traverse 'a 'b2) x2
  $(traverse 'a '(T b1 b2))  =  traverse $(traverse 'a 'b2)  -- when a only occurs in the last parameter, b2

Note that the generated code is not as efficient as it could be. For instance:

  data T a = T Int a  deriving Traversable

gives the function: traverse f (T x y) = T <$> pure x <*> f y
instead of:         traverse f (T x y) = T x <$> f y

\begin{code}
gen_Traversable_binds :: SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff)
gen_Traversable_binds loc tycon
  = (unitBag traverse_bind, emptyBag)
  where
    data_cons = tyConDataCons tycon

    traverse_bind = mkRdrFunBind (L loc traverse_RDR) eqns
    eqns = map traverse_eqn data_cons
    traverse_eqn con = evalState (match_for_con [f_Pat] con =<< parts) bs_RDRs
      where
        parts = sequence $ foldDataConArgs ft_trav con


    ft_trav :: FFoldType (State [RdrName] (LHsExpr RdrName))
    ft_trav = FT { ft_triv    = return pure_Expr                  -- traverse f = pure x
                 , ft_var     = return f_Expr                     -- traverse f = f x
                 , ft_tup     = \t gs -> do                       -- traverse f = \x -> case x of (a1,a2,..) ->
                                    gg <- sequence gs             --                   (,,) <$> g1 a1 <*> g2 a2 <*> ..
                                    mkSimpleLam $ mkSimpleTupleCase match_for_con t gg
                 , ft_ty_app  = \_ g -> nlHsApp traverse_Expr <$> g  -- traverse f = travese g
                 , ft_forall  = \_ g -> g
                 , ft_co_var  = panic "contravariant"
                 , ft_fun     = panic "function"
                 , ft_bad_app = panic "in other argument" }

    -- Con a1 a2 ... -> Con <$> g1 a1 <*> g2 a2 <*> ...
    match_for_con = mkSimpleConMatch $
        \con_name xs -> return $ mkApCon (nlHsVar con_name) xs

    -- ((Con <$> x1) <*> x2) <*> ..
    mkApCon con []     = nlHsApps pure_RDR [con]
    mkApCon con (x:xs) = foldl appAp (nlHsApps fmap_RDR [con,x]) xs
       where appAp x y = nlHsApps ap_RDR [x,y]
\end{code}

%************************************************************************
%*                                                                      *
                     Newtype-deriving instances
%*                                                                      *
%************************************************************************

We take every method in the original instance and `coerce` it to fit
into the derived instance. We need a type annotation on the argument
to `coerce` to make it obvious what instantiation of the method we're
coercing from.

See #8503 for more discussion.

\begin{code}
mkCoerceClassMethEqn :: Class   -- the class being derived
                     -> [TyVar] -- the tvs in the instance head
                     -> [Type]  -- instance head parameters (incl. newtype)
                     -> Type    -- the representation type (already eta-reduced)
                     -> Id      -- the method to look at
                     -> Pair Type
mkCoerceClassMethEqn cls inst_tvs cls_tys rhs_ty id
  = Pair (substTy rhs_subst user_meth_ty) (substTy lhs_subst user_meth_ty)
  where
    cls_tvs = classTyVars cls
    in_scope = mkInScopeSet $ mkVarSet inst_tvs
    lhs_subst = mkTvSubst in_scope (zipTyEnv cls_tvs cls_tys)
    rhs_subst = mkTvSubst in_scope (zipTyEnv cls_tvs (changeLast cls_tys rhs_ty))
    (_class_tvs, _class_constraint, user_meth_ty) = tcSplitSigmaTy (varType id)

    changeLast :: [a] -> a -> [a]
    changeLast []     _  = panic "changeLast"
    changeLast [_]    x  = [x]
    changeLast (x:xs) x' = x : changeLast xs x'


gen_Newtype_binds :: SrcSpan
                  -> Class   -- the class being derived
                  -> [TyVar] -- the tvs in the instance head
                  -> [Type]  -- instance head parameters (incl. newtype)
                  -> Type    -- the representation type (already eta-reduced)
                  -> LHsBinds RdrName
gen_Newtype_binds loc cls inst_tvs cls_tys rhs_ty
  = listToBag $ zipWith mk_bind
        (classMethods cls)
        (map (mkCoerceClassMethEqn cls inst_tvs cls_tys rhs_ty) (classMethods cls))
  where
    coerce_RDR = getRdrName coerceId
    mk_bind :: Id -> Pair Type -> (Origin, LHsBind RdrName)
    mk_bind id (Pair tau_ty user_ty)
      = mkRdrFunBind (L loc meth_RDR) [mkSimpleMatch [] rhs_expr]
      where
        meth_RDR = getRdrName id
        rhs_expr
          = ( nlHsVar coerce_RDR
                `nlHsApp`
              (nlHsVar meth_RDR `nlExprWithTySig` toHsType tau_ty'))
            `nlExprWithTySig` toHsType user_ty
        -- Open the representation type here, so that it's forall'ed type
        -- variables refer to the ones bound in the user_ty
        (_, _, tau_ty')  = tcSplitSigmaTy tau_ty

    nlExprWithTySig e s = noLoc (ExprWithTySig e s)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Generating extra binds (@con2tag@ and @tag2con@)}
%*                                                                      *
%************************************************************************

\begin{verbatim}
data Foo ... = ...

con2tag_Foo :: Foo ... -> Int#
tag2con_Foo :: Int -> Foo ...   -- easier if Int, not Int#
maxtag_Foo  :: Int              -- ditto (NB: not unlifted)
\end{verbatim}

The `tags' here start at zero, hence the @fIRST_TAG@ (currently one)
fiddling around.

\begin{code}
genAuxBindSpec :: SrcSpan -> AuxBindSpec -> ((Origin, LHsBind RdrName), LSig RdrName)
genAuxBindSpec loc (DerivCon2Tag tycon)
  = (mk_FunBind loc rdr_name eqns,
     L loc (TypeSig [L loc rdr_name] (L loc sig_ty)))
  where
    rdr_name = con2tag_RDR tycon

    sig_ty = HsCoreTy $
             mkSigmaTy (tyConTyVars tycon) (tyConStupidTheta tycon) $
             mkParentType tycon `mkFunTy` intPrimTy

    lots_of_constructors = tyConFamilySize tycon > 8
                        -- was: mAX_FAMILY_SIZE_FOR_VEC_RETURNS
                        -- but we don't do vectored returns any more.

    eqns | lots_of_constructors = [get_tag_eqn]
         | otherwise = map mk_eqn (tyConDataCons tycon)

    get_tag_eqn = ([nlVarPat a_RDR], nlHsApp (nlHsVar getTag_RDR) a_Expr)

    mk_eqn :: DataCon -> ([LPat RdrName], LHsExpr RdrName)
    mk_eqn con = ([nlWildConPat con],
                  nlHsLit (HsIntPrim (toInteger ((dataConTag con) - fIRST_TAG))))

genAuxBindSpec loc (DerivTag2Con tycon)
  = (mk_FunBind loc rdr_name
        [([nlConVarPat intDataCon_RDR [a_RDR]],
           nlHsApp (nlHsVar tagToEnum_RDR) a_Expr)],
     L loc (TypeSig [L loc rdr_name] (L loc sig_ty)))
  where
    sig_ty = HsCoreTy $ mkForAllTys (tyConTyVars tycon) $
             intTy `mkFunTy` mkParentType tycon

    rdr_name = tag2con_RDR tycon

genAuxBindSpec loc (DerivMaxTag tycon)
  = (mkHsVarBind loc rdr_name rhs,
     L loc (TypeSig [L loc rdr_name] (L loc sig_ty)))
  where
    rdr_name = maxtag_RDR tycon
    sig_ty = HsCoreTy intTy
    rhs = nlHsApp (nlHsVar intDataCon_RDR) (nlHsLit (HsIntPrim max_tag))
    max_tag =  case (tyConDataCons tycon) of
                 data_cons -> toInteger ((length data_cons) - fIRST_TAG)

type SeparateBagsDerivStuff = -- AuxBinds and SYB bindings
                              ( Bag ((Origin, LHsBind RdrName), LSig RdrName)
                                -- Extra bindings (used by Generic only)
                              , Bag TyCon   -- Extra top-level datatypes
                              , Bag (FamInst)           -- Extra family instances
                              , Bag (InstInfo RdrName)) -- Extra instances

genAuxBinds :: SrcSpan -> BagDerivStuff -> SeparateBagsDerivStuff
genAuxBinds loc b = genAuxBinds' b2 where
  (b1,b2) = partitionBagWith splitDerivAuxBind b
  splitDerivAuxBind (DerivAuxBind x) = Left x
  splitDerivAuxBind  x               = Right x

  rm_dups = foldrBag dup_check emptyBag
  dup_check a b = if anyBag (== a) b then b else consBag a b

  genAuxBinds' :: BagDerivStuff -> SeparateBagsDerivStuff
  genAuxBinds' = foldrBag f ( mapBag (genAuxBindSpec loc) (rm_dups b1)
                            , emptyBag, emptyBag, emptyBag)
  f :: DerivStuff -> SeparateBagsDerivStuff -> SeparateBagsDerivStuff
  f (DerivAuxBind _) = panic "genAuxBinds'" -- We have removed these before
  f (DerivHsBind  b) = add1 b
  f (DerivTyCon   t) = add2 t
  f (DerivFamInst t) = add3 t
  f (DerivInst    i) = add4 i

  add1 x (a,b,c,d) = (x `consBag` a,b,c,d)
  add2 x (a,b,c,d) = (a,x `consBag` b,c,d)
  add3 x (a,b,c,d) = (a,b,x `consBag` c,d)
  add4 x (a,b,c,d) = (a,b,c,x `consBag` d)

mk_data_type_name :: TyCon -> RdrName   -- "$tT"
mk_data_type_name tycon = mkAuxBinderName (tyConName tycon) mkDataTOcc

mk_constr_name :: DataCon -> RdrName    -- "$cC"
mk_constr_name con = mkAuxBinderName (dataConName con) mkDataCOcc

mkParentType :: TyCon -> Type
-- Turn the representation tycon of a family into
-- a use of its family constructor
mkParentType tc
  = case tyConFamInst_maybe tc of
       Nothing  -> mkTyConApp tc (mkTyVarTys (tyConTyVars tc))
       Just (fam_tc,tys) -> mkTyConApp fam_tc tys
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Utility bits for generating bindings}
%*                                                                      *
%************************************************************************


\begin{code}
mk_FunBind :: SrcSpan -> RdrName
           -> [([LPat RdrName], LHsExpr RdrName)]
           -> (Origin, LHsBind RdrName)
mk_FunBind loc fun pats_and_exprs
  = mkRdrFunBind (L loc fun) matches
  where
    matches = [mkMatch p e emptyLocalBinds | (p,e) <-pats_and_exprs]

mkRdrFunBind :: Located RdrName -> [LMatch RdrName (LHsExpr RdrName)] -> (Origin, LHsBind RdrName)
mkRdrFunBind fun@(L loc fun_rdr) matches = (Generated, L loc (mkFunBind fun matches'))
 where
   -- Catch-all eqn looks like
   --     fmap = error "Void fmap"
   -- It's needed if there no data cons at all,
   -- which can happen with -XEmptyDataDecls
   -- See Trac #4302
   matches' = if null matches
              then [mkMatch [] (error_Expr str) emptyLocalBinds]
              else matches
   str = "Void " ++ occNameString (rdrNameOcc fun_rdr)
\end{code}

\begin{code}
box_if_necy :: String           -- The class involved
            -> TyCon            -- The tycon involved
            -> LHsExpr RdrName  -- The argument
            -> Type             -- The argument type
            -> LHsExpr RdrName  -- Boxed version of the arg
-- See Note [Deriving and unboxed types]
box_if_necy cls_str tycon arg arg_ty
  | isUnLiftedType arg_ty = nlHsApp (nlHsVar box_con) arg
  | otherwise             = arg
  where
    box_con = assoc_ty_id cls_str tycon boxConTbl arg_ty

---------------------
primOrdOps :: String    -- The class involved
           -> TyCon     -- The tycon involved
           -> Type      -- The type
           -> (RdrName, RdrName, RdrName, RdrName, RdrName)  -- (lt,le,eq,ge,gt)
-- See Note [Deriving and unboxed types]
primOrdOps str tycon ty = assoc_ty_id str tycon ordOpTbl ty

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
    | not (isUnLiftedType ty) = genOpApp a eq_RDR b
    | otherwise               = genPrimOpApp a prim_eq b
 where
   (_, _, prim_eq, _, _) = primOrdOps "Eq" tycon ty
\end{code}

\begin{code}
untag_Expr :: TyCon -> [( RdrName,  RdrName)] -> LHsExpr RdrName -> LHsExpr RdrName
untag_Expr _ [] expr = expr
untag_Expr tycon ((untag_this, put_tag_here) : more) expr
  = nlHsCase (nlHsPar (nlHsVarApps (con2tag_RDR tycon) [untag_this])) {-of-}
      [mkSimpleHsAlt (nlVarPat put_tag_here) (untag_Expr tycon more expr)]

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
\end{code}

\begin{code}
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

a_Expr, c_Expr, f_Expr, z_Expr, ltTag_Expr, eqTag_Expr, gtTag_Expr,
    false_Expr, true_Expr, fmap_Expr, pure_Expr, mempty_Expr, foldMap_Expr, traverse_Expr :: LHsExpr RdrName
a_Expr          = nlHsVar a_RDR
-- b_Expr       = nlHsVar b_RDR
c_Expr          = nlHsVar c_RDR
f_Expr          = nlHsVar f_RDR
z_Expr          = nlHsVar z_RDR
ltTag_Expr      = nlHsVar ltTag_RDR
eqTag_Expr      = nlHsVar eqTag_RDR
gtTag_Expr      = nlHsVar gtTag_RDR
false_Expr      = nlHsVar false_RDR
true_Expr       = nlHsVar true_RDR
fmap_Expr       = nlHsVar fmap_RDR
pure_Expr       = nlHsVar pure_RDR
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

con2tag_RDR, tag2con_RDR, maxtag_RDR :: TyCon -> RdrName
-- Generates Orig s RdrName, for the binding positions
con2tag_RDR tycon = mk_tc_deriv_name tycon mkCon2TagOcc
tag2con_RDR tycon = mk_tc_deriv_name tycon mkTag2ConOcc
maxtag_RDR  tycon = mk_tc_deriv_name tycon mkMaxTagOcc

mk_tc_deriv_name :: TyCon -> (OccName -> OccName) -> RdrName
mk_tc_deriv_name tycon occ_fun = mkAuxBinderName (tyConName tycon) occ_fun

mkAuxBinderName :: Name -> (OccName -> OccName) -> RdrName
mkAuxBinderName parent occ_fun = mkRdrUnqual (occ_fun (nameOccName parent))
-- Was: mkDerivedRdrName name occ_fun, which made an original name
-- But:  (a) that does not work well for standalone-deriving
--       (b) an unqualified name is just fine, provided it can't clash with user code

minusInt_RDR, tagToEnum_RDR, error_RDR :: RdrName
minusInt_RDR  = getRdrName (primOpId IntSubOp   )
tagToEnum_RDR = getRdrName (primOpId TagToEnumOp)
error_RDR     = getRdrName eRROR_ID
\end{code}
