{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Pattern-matching bindings (HsBinds and MonoBinds)

Handles @HsBinds@; those at the top level require different handling,
in that the @Rec@/@NonRec@/etc structure is thrown away (whereas at
lower levels it is preserved with @let@/@letrec@s).
-}

{-# LANGUAGE CPP #-}

module DsBinds ( dsTopLHsBinds, dsLHsBinds, decomposeRuleLhs, dsSpec,
                 dsHsWrapper, dsTcEvBinds, dsTcEvBinds_s, dsEvBinds
  ) where

#include "HsVersions.h"

import {-# SOURCE #-}   DsExpr( dsLExpr )
import {-# SOURCE #-}   Match( matchWrapper )

import DsMonad
import DsGRHSs
import DsUtils

import HsSyn            -- lots of things
import CoreSyn          -- lots of things
import Literal          ( Literal(MachStr) )
import CoreSubst
import OccurAnal        ( occurAnalyseExpr )
import MkCore
import CoreUtils
import CoreArity ( etaExpand )
import CoreUnfold
import CoreFVs
import UniqSupply
import Digraph

import PrelNames
import TysPrim ( mkProxyPrimTy )
import TyCon
import TcEvidence
import TcType
import Type
import Kind (returnsConstraintKind)
import Coercion hiding (substCo)
import TysWiredIn ( eqBoxDataCon, coercibleDataCon, mkListTy
                  , mkBoxedTupleTy, charTy, typeNatKind, typeSymbolKind )
import Id
import MkId(proxyHashId)
import Class
import DataCon  ( dataConTyCon )
import Name
import IdInfo   ( IdDetails(..) )
import Var
import VarSet
import Rules
import VarEnv
import Outputable
import Module
import SrcLoc
import Maybes
import OrdList
import Bag
import BasicTypes hiding ( TopLevel )
import DynFlags
import FastString
import Util
import MonadUtils
import Control.Monad(liftM)
import Fingerprint(Fingerprint(..), fingerprintString)

{-
************************************************************************
*                                                                      *
\subsection[dsMonoBinds]{Desugaring a @MonoBinds@}
*                                                                      *
************************************************************************
-}

dsTopLHsBinds :: LHsBinds Id -> DsM (OrdList (Id,CoreExpr))
dsTopLHsBinds binds = ds_lhs_binds binds

dsLHsBinds :: LHsBinds Id -> DsM [(Id,CoreExpr)]
dsLHsBinds binds = do { binds' <- ds_lhs_binds binds
                      ; return (fromOL binds') }

------------------------
ds_lhs_binds :: LHsBinds Id -> DsM (OrdList (Id,CoreExpr))

ds_lhs_binds binds = do { ds_bs <- mapBagM dsLHsBind binds
                        ; return (foldBag appOL id nilOL ds_bs) }

dsLHsBind :: LHsBind Id -> DsM (OrdList (Id,CoreExpr))
dsLHsBind (L loc bind) = putSrcSpanDs loc $ dsHsBind bind

dsHsBind :: HsBind Id -> DsM (OrdList (Id,CoreExpr))

dsHsBind (VarBind { var_id = var, var_rhs = expr, var_inline = inline_regardless })
  = do  { dflags <- getDynFlags
        ; core_expr <- dsLExpr expr

                -- Dictionary bindings are always VarBinds,
                -- so we only need do this here
        ; let var' | inline_regardless = var `setIdUnfolding` mkCompulsoryUnfolding core_expr
                   | otherwise         = var

        ; return (unitOL (makeCorePair dflags var' False 0 core_expr)) }

dsHsBind (FunBind { fun_id = L _ fun, fun_matches = matches
                  , fun_co_fn = co_fn, fun_tick = tick
                  , fun_infix = inf })
 = do   { dflags <- getDynFlags
        ; (args, body) <- matchWrapper (FunRhs (idName fun) inf) matches
        ; let body' = mkOptTickBox tick body
        ; rhs <- dsHsWrapper co_fn (mkLams args body')
        ; {- pprTrace "dsHsBind" (ppr fun <+> ppr (idInlinePragma fun)) $ -}
           return (unitOL (makeCorePair dflags fun False 0 rhs)) }

dsHsBind (PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty
                  , pat_ticks = (rhs_tick, var_ticks) })
  = do  { body_expr <- dsGuarded grhss ty
        ; let body' = mkOptTickBox rhs_tick body_expr
        ; sel_binds <- mkSelectorBinds var_ticks pat body'
          -- We silently ignore inline pragmas; no makeCorePair
          -- Not so cool, but really doesn't matter
    ; return (toOL sel_binds) }

        -- A common case: one exported variable
        -- Non-recursive bindings come through this way
        -- So do self-recursive bindings, and recursive bindings
        -- that have been chopped up with type signatures
dsHsBind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
                   , abs_exports = [export]
                   , abs_ev_binds = ev_binds, abs_binds = binds })
  | ABE { abe_wrap = wrap, abe_poly = global
        , abe_mono = local, abe_prags = prags } <- export
  = do  { dflags <- getDynFlags
        ; bind_prs <- ds_lhs_binds binds
        ; let core_bind = Rec (fromOL bind_prs)
        ; ds_binds <- dsTcEvBinds_s ev_binds
        ; rhs <- dsHsWrapper wrap $  -- Usually the identity
                            mkLams tyvars $ mkLams dicts $
                            mkCoreLets ds_binds $
                            Let core_bind $
                            Var local

        ; (spec_binds, rules) <- dsSpecs rhs prags

        ; let   global'   = addIdSpecialisations global rules
                main_bind = makeCorePair dflags global' (isDefaultMethod prags)
                                         (dictArity dicts) rhs

        ; return (main_bind `consOL` spec_binds) }

dsHsBind (AbsBinds { abs_tvs = tyvars, abs_ev_vars = dicts
                   , abs_exports = exports, abs_ev_binds = ev_binds
                   , abs_binds = binds })
         -- See Note [Desugaring AbsBinds]
  = do  { dflags <- getDynFlags
        ; bind_prs    <- ds_lhs_binds binds
        ; let core_bind = Rec [ makeCorePair dflags (add_inline lcl_id) False 0 rhs
                              | (lcl_id, rhs) <- fromOL bind_prs ]
                -- Monomorphic recursion possible, hence Rec

              locals       = map abe_mono exports
              tup_expr     = mkBigCoreVarTup locals
              tup_ty       = exprType tup_expr
        ; ds_binds <- dsTcEvBinds_s ev_binds
        ; let poly_tup_rhs = mkLams tyvars $ mkLams dicts $
                             mkCoreLets ds_binds $
                             Let core_bind $
                             tup_expr

        ; poly_tup_id <- newSysLocalDs (exprType poly_tup_rhs)

        ; let mk_bind (ABE { abe_wrap = wrap, abe_poly = global
                           , abe_mono = local, abe_prags = spec_prags })
                = do { tup_id  <- newSysLocalDs tup_ty
                     ; rhs <- dsHsWrapper wrap $
                                 mkLams tyvars $ mkLams dicts $
                                 mkTupleSelector locals local tup_id $
                                 mkVarApps (Var poly_tup_id) (tyvars ++ dicts)
                     ; let rhs_for_spec = Let (NonRec poly_tup_id poly_tup_rhs) rhs
                     ; (spec_binds, rules) <- dsSpecs rhs_for_spec spec_prags
                     ; let global' = (global `setInlinePragma` defaultInlinePragma)
                                             `addIdSpecialisations` rules
                           -- Kill the INLINE pragma because it applies to
                           -- the user written (local) function.  The global
                           -- Id is just the selector.  Hmm.
                     ; return ((global', rhs) `consOL` spec_binds) }

        ; export_binds_s <- mapM mk_bind exports

        ; return ((poly_tup_id, poly_tup_rhs) `consOL`
                    concatOL export_binds_s) }
  where
    inline_env :: IdEnv Id   -- Maps a monomorphic local Id to one with
                             -- the inline pragma from the source
                             -- The type checker put the inline pragma
                             -- on the *global* Id, so we need to transfer it
    inline_env = mkVarEnv [ (lcl_id, setInlinePragma lcl_id prag)
                          | ABE { abe_mono = lcl_id, abe_poly = gbl_id } <- exports
                          , let prag = idInlinePragma gbl_id ]

    add_inline :: Id -> Id    -- tran
    add_inline lcl_id = lookupVarEnv inline_env lcl_id `orElse` lcl_id

dsHsBind (PatSynBind{}) = panic "dsHsBind: PatSynBind"

------------------------
makeCorePair :: DynFlags -> Id -> Bool -> Arity -> CoreExpr -> (Id, CoreExpr)
makeCorePair dflags gbl_id is_default_method dict_arity rhs
  | is_default_method                 -- Default methods are *always* inlined
  = (gbl_id `setIdUnfolding` mkCompulsoryUnfolding rhs, rhs)

  | DFunId is_newtype <- idDetails gbl_id
  = (mk_dfun_w_stuff is_newtype, rhs)

  | otherwise
  = case inlinePragmaSpec inline_prag of
          EmptyInlineSpec -> (gbl_id, rhs)
          NoInline        -> (gbl_id, rhs)
          Inlinable       -> (gbl_id `setIdUnfolding` inlinable_unf, rhs)
          Inline          -> inline_pair

  where
    inline_prag   = idInlinePragma gbl_id
    inlinable_unf = mkInlinableUnfolding dflags rhs
    inline_pair
       | Just arity <- inlinePragmaSat inline_prag
        -- Add an Unfolding for an INLINE (but not for NOINLINE)
        -- And eta-expand the RHS; see Note [Eta-expanding INLINE things]
       , let real_arity = dict_arity + arity
        -- NB: The arity in the InlineRule takes account of the dictionaries
       = ( gbl_id `setIdUnfolding` mkInlineUnfolding (Just real_arity) rhs
         , etaExpand real_arity rhs)

       | otherwise
       = pprTrace "makeCorePair: arity missing" (ppr gbl_id) $
         (gbl_id `setIdUnfolding` mkInlineUnfolding Nothing rhs, rhs)

                -- See Note [ClassOp/DFun selection] in TcInstDcls
                -- See Note [Single-method classes]  in TcInstDcls
    mk_dfun_w_stuff is_newtype
       | is_newtype
       = gbl_id `setIdUnfolding`  mkInlineUnfolding (Just 0) rhs
                `setInlinePragma` alwaysInlinePragma { inl_sat = Just 0 }
       | otherwise
       = gbl_id `setIdUnfolding`  mkDFunUnfolding dfun_bndrs dfun_constr dfun_args
                `setInlinePragma` dfunInlinePragma
    (dfun_bndrs, dfun_body) = collectBinders (simpleOptExpr rhs)
    (dfun_con, dfun_args)   = collectArgs dfun_body
    dfun_constr | Var id <- dfun_con
                , DataConWorkId con <- idDetails id
                = con
                | otherwise = pprPanic "makeCorePair: dfun" (ppr rhs)


dictArity :: [Var] -> Arity
-- Don't count coercion variables in arity
dictArity dicts = count isId dicts

{-
[Desugaring AbsBinds]
~~~~~~~~~~~~~~~~~~~~~
In the general AbsBinds case we desugar the binding to this:

       tup a (d:Num a) = let fm = ...gm...
                             gm = ...fm...
                         in (fm,gm)
       f a d = case tup a d of { (fm,gm) -> fm }
       g a d = case tup a d of { (fm,gm) -> fm }

Note [Rules and inlining]
~~~~~~~~~~~~~~~~~~~~~~~~~
Common special case: no type or dictionary abstraction
This is a bit less trivial than you might suppose
The naive way woudl be to desguar to something like
        f_lcl = ...f_lcl...     -- The "binds" from AbsBinds
        M.f = f_lcl             -- Generated from "exports"
But we don't want that, because if M.f isn't exported,
it'll be inlined unconditionally at every call site (its rhs is
trivial).  That would be ok unless it has RULES, which would
thereby be completely lost.  Bad, bad, bad.

Instead we want to generate
        M.f = ...f_lcl...
        f_lcl = M.f
Now all is cool. The RULES are attached to M.f (by SimplCore),
and f_lcl is rapidly inlined away.

This does not happen in the same way to polymorphic binds,
because they desugar to
        M.f = /\a. let f_lcl = ...f_lcl... in f_lcl
Although I'm a bit worried about whether full laziness might
float the f_lcl binding out and then inline M.f at its call site

Note [Specialising in no-dict case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even if there are no tyvars or dicts, we may have specialisation pragmas.
Class methods can generate
      AbsBinds [] [] [( ... spec-prag]
         { AbsBinds [tvs] [dicts] ...blah }
So the overloading is in the nested AbsBinds. A good example is in GHC.Float:

  class  (Real a, Fractional a) => RealFrac a  where
    round :: (Integral b) => a -> b

  instance  RealFrac Float  where
    {-# SPECIALIZE round :: Float -> Int #-}

The top-level AbsBinds for $cround has no tyvars or dicts (because the
instance does not).  But the method is locally overloaded!

Note [Abstracting over tyvars only]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When abstracting over type variable only (not dictionaries), we don't really need to
built a tuple and select from it, as we do in the general case. Instead we can take

        AbsBinds [a,b] [ ([a,b], fg, fl, _),
                         ([b],   gg, gl, _) ]
                { fl = e1
                  gl = e2
                   h = e3 }

and desugar it to

        fg = /\ab. let B in e1
        gg = /\b. let a = () in let B in S(e2)
        h  = /\ab. let B in e3

where B is the *non-recursive* binding
        fl = fg a b
        gl = gg b
        h  = h a b    -- See (b); note shadowing!

Notice (a) g has a different number of type variables to f, so we must
             use the mkArbitraryType thing to fill in the gaps.
             We use a type-let to do that.

         (b) The local variable h isn't in the exports, and rather than
             clone a fresh copy we simply replace h by (h a b), where
             the two h's have different types!  Shadowing happens here,
             which looks confusing but works fine.

         (c) The result is *still* quadratic-sized if there are a lot of
             small bindings.  So if there are more than some small
             number (10), we filter the binding set B by the free
             variables of the particular RHS.  Tiresome.

Why got to this trouble?  It's a common case, and it removes the
quadratic-sized tuple desugaring.  Less clutter, hopefully faster
compilation, especially in a case where there are a *lot* of
bindings.


Note [Eta-expanding INLINE things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   foo :: Eq a => a -> a
   {-# INLINE foo #-}
   foo x = ...

If (foo d) ever gets floated out as a common sub-expression (which can
happen as a result of method sharing), there's a danger that we never
get to do the inlining, which is a Terribly Bad thing given that the
user said "inline"!

To avoid this we pre-emptively eta-expand the definition, so that foo
has the arity with which it is declared in the source code.  In this
example it has arity 2 (one for the Eq and one for x). Doing this
should mean that (foo d) is a PAP and we don't share it.

Note [Nested arities]
~~~~~~~~~~~~~~~~~~~~~
For reasons that are not entirely clear, method bindings come out looking like
this:

  AbsBinds [] [] [$cfromT <= [] fromT]
    $cfromT [InlPrag=INLINE] :: T Bool -> Bool
    { AbsBinds [] [] [fromT <= [] fromT_1]
        fromT :: T Bool -> Bool
        { fromT_1 ((TBool b)) = not b } } }

Note the nested AbsBind.  The arity for the InlineRule on $cfromT should be
gotten from the binding for fromT_1.

It might be better to have just one level of AbsBinds, but that requires more
thought!
-}

------------------------
dsSpecs :: CoreExpr     -- Its rhs
        -> TcSpecPrags
        -> DsM ( OrdList (Id,CoreExpr)  -- Binding for specialised Ids
               , [CoreRule] )           -- Rules for the Global Ids
-- See Note [Handling SPECIALISE pragmas] in TcBinds
dsSpecs _ IsDefaultMethod = return (nilOL, [])
dsSpecs poly_rhs (SpecPrags sps)
  = do { pairs <- mapMaybeM (dsSpec (Just poly_rhs)) sps
       ; let (spec_binds_s, rules) = unzip pairs
       ; return (concatOL spec_binds_s, rules) }

dsSpec :: Maybe CoreExpr        -- Just rhs => RULE is for a local binding
                                -- Nothing => RULE is for an imported Id
                                --            rhs is in the Id's unfolding
       -> Located TcSpecPrag
       -> DsM (Maybe (OrdList (Id,CoreExpr), CoreRule))
dsSpec mb_poly_rhs (L loc (SpecPrag poly_id spec_co spec_inl))
  | isJust (isClassOpId_maybe poly_id)
  = putSrcSpanDs loc $
    do { warnDs (ptext (sLit "Ignoring useless SPECIALISE pragma for class method selector")
                 <+> quotes (ppr poly_id))
       ; return Nothing  }  -- There is no point in trying to specialise a class op
                            -- Moreover, classops don't (currently) have an inl_sat arity set
                            -- (it would be Just 0) and that in turn makes makeCorePair bleat

  | no_act_spec && isNeverActive rule_act
  = putSrcSpanDs loc $
    do { warnDs (ptext (sLit "Ignoring useless SPECIALISE pragma for NOINLINE function:")
                 <+> quotes (ppr poly_id))
       ; return Nothing  }  -- Function is NOINLINE, and the specialiation inherits that
                            -- See Note [Activation pragmas for SPECIALISE]

  | otherwise
  = putSrcSpanDs loc $
    do { uniq <- newUnique
       ; let poly_name = idName poly_id
             spec_occ  = mkSpecOcc (getOccName poly_name)
             spec_name = mkInternalName uniq spec_occ (getSrcSpan poly_name)
       ; (bndrs, ds_lhs) <- liftM collectBinders
                                  (dsHsWrapper spec_co (Var poly_id))
       ; let spec_ty = mkPiTypes bndrs (exprType ds_lhs)
       ; -- pprTrace "dsRule" (vcat [ ptext (sLit "Id:") <+> ppr poly_id
         --                         , ptext (sLit "spec_co:") <+> ppr spec_co
         --                         , ptext (sLit "ds_rhs:") <+> ppr ds_lhs ]) $
         case decomposeRuleLhs bndrs ds_lhs of {
           Left msg -> do { warnDs msg; return Nothing } ;
           Right (rule_bndrs, _fn, args) -> do

       { dflags <- getDynFlags
       ; this_mod <- getModule
       ; let fn_unf    = realIdUnfolding poly_id
             unf_fvs   = stableUnfoldingVars fn_unf `orElse` emptyVarSet
             in_scope  = mkInScopeSet (unf_fvs `unionVarSet` exprsFreeVars args)
             spec_unf  = specUnfolding dflags (mkEmptySubst in_scope) bndrs args fn_unf
             spec_id   = mkLocalId spec_name spec_ty
                            `setInlinePragma` inl_prag
                            `setIdUnfolding`  spec_unf
             rule =  mkRule this_mod False {- Not auto -} is_local_id
                        (mkFastString ("SPEC " ++ showPpr dflags poly_name))
                        rule_act poly_name
                        rule_bndrs args
                        (mkVarApps (Var spec_id) bndrs)

       ; spec_rhs <- dsHsWrapper spec_co poly_rhs

-- Commented out: see Note [SPECIALISE on INLINE functions]
--       ; when (isInlinePragma id_inl)
--              (warnDs $ ptext (sLit "SPECIALISE pragma on INLINE function probably won't fire:")
--                        <+> quotes (ppr poly_name))

       ; return (Just (unitOL (spec_id, spec_rhs), rule))
            -- NB: do *not* use makeCorePair on (spec_id,spec_rhs), because
            --     makeCorePair overwrites the unfolding, which we have
            --     just created using specUnfolding
       } } }
  where
    is_local_id = isJust mb_poly_rhs
    poly_rhs | Just rhs <-  mb_poly_rhs
             = rhs          -- Local Id; this is its rhs
             | Just unfolding <- maybeUnfoldingTemplate (realIdUnfolding poly_id)
             = unfolding    -- Imported Id; this is its unfolding
                            -- Use realIdUnfolding so we get the unfolding
                            -- even when it is a loop breaker.
                            -- We want to specialise recursive functions!
             | otherwise = pprPanic "dsImpSpecs" (ppr poly_id)
                            -- The type checker has checked that it *has* an unfolding

    id_inl = idInlinePragma poly_id

    -- See Note [Activation pragmas for SPECIALISE]
    inl_prag | not (isDefaultInlinePragma spec_inl)    = spec_inl
             | not is_local_id  -- See Note [Specialising imported functions]
                                 -- in OccurAnal
             , isStrongLoopBreaker (idOccInfo poly_id) = neverInlinePragma
             | otherwise                               = id_inl
     -- Get the INLINE pragma from SPECIALISE declaration, or,
     -- failing that, from the original Id

    spec_prag_act = inlinePragmaActivation spec_inl

    -- See Note [Activation pragmas for SPECIALISE]
    -- no_act_spec is True if the user didn't write an explicit
    -- phase specification in the SPECIALISE pragma
    no_act_spec = case inlinePragmaSpec spec_inl of
                    NoInline -> isNeverActive  spec_prag_act
                    _        -> isAlwaysActive spec_prag_act
    rule_act | no_act_spec = inlinePragmaActivation id_inl   -- Inherit
             | otherwise   = spec_prag_act                   -- Specified by user



{- Note [SPECIALISE on INLINE functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to warn that using SPECIALISE for a function marked INLINE
would be a no-op; but it isn't!  Especially with worker/wrapper split
we might have
   {-# INLINE f #-}
   f :: Ord a => Int -> a -> ...
   f d x y = case x of I# x' -> $wf d x' y

We might want to specialise 'f' so that we in turn specialise '$wf'.
We can't even /name/ '$wf' in the source code, so we can't specialise
it even if we wanted to.  Trac #10721 is a case in point.

Note [Activation pragmas for SPECIALISE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From a user SPECIALISE pragma for f, we generate
  a) A top-level binding    spec_fn = rhs
  b) A RULE                 f dOrd = spec_fn

We need two pragma-like things:

* spec_fn's inline pragma: inherited from f's inline pragma (ignoring
                           activation on SPEC), unless overriden by SPEC INLINE

* Activation of RULE: from SPECIALISE pragma (if activation given)
                      otherwise from f's inline pragma

This is not obvious (see Trac #5237)!

Examples      Rule activation   Inline prag on spec'd fn
---------------------------------------------------------------------
SPEC [n] f :: ty            [n]   Always, or NOINLINE [n]
                                  copy f's prag

NOINLINE f
SPEC [n] f :: ty            [n]   NOINLINE
                                  copy f's prag

NOINLINE [k] f
SPEC [n] f :: ty            [n]   NOINLINE [k]
                                  copy f's prag

INLINE [k] f
SPEC [n] f :: ty            [n]   INLINE [k]
                                  copy f's prag

SPEC INLINE [n] f :: ty     [n]   INLINE [n]
                                  (ignore INLINE prag on f,
                                  same activation for rule and spec'd fn)

NOINLINE [k] f
SPEC f :: ty                [n]   INLINE [k]


************************************************************************
*                                                                      *
\subsection{Adding inline pragmas}
*                                                                      *
************************************************************************
-}

decomposeRuleLhs :: [Var] -> CoreExpr -> Either SDoc ([Var], Id, [CoreExpr])
-- (decomposeRuleLhs bndrs lhs) takes apart the LHS of a RULE,
-- The 'bndrs' are the quantified binders of the rules, but decomposeRuleLhs
-- may add some extra dictionary binders (see Note [Free dictionaries])
--
-- Returns Nothing if the LHS isn't of the expected shape
-- Note [Decomposing the left-hand side of a RULE]
decomposeRuleLhs orig_bndrs orig_lhs
  | not (null unbound)    -- Check for things unbound on LHS
                          -- See Note [Unused spec binders]
  = Left (vcat (map dead_msg unbound))

  | Just (fn_id, args) <- decompose fun2 args2
  , let extra_dict_bndrs = mk_extra_dict_bndrs fn_id args
  = -- pprTrace "decmposeRuleLhs" (vcat [ ptext (sLit "orig_bndrs:") <+> ppr orig_bndrs
    --                                  , ptext (sLit "orig_lhs:") <+> ppr orig_lhs
    --                                  , ptext (sLit "lhs1:")     <+> ppr lhs1
    --                                  , ptext (sLit "extra_dict_bndrs:") <+> ppr extra_dict_bndrs
    --                                  , ptext (sLit "fn_id:") <+> ppr fn_id
    --                                  , ptext (sLit "args:")   <+> ppr args]) $
    Right (orig_bndrs ++ extra_dict_bndrs, fn_id, args)

  | otherwise
  = Left bad_shape_msg
 where
   lhs1         = drop_dicts orig_lhs
   lhs2         = simpleOptExpr lhs1  -- See Note [Simplify rule LHS]
   (fun2,args2) = collectArgs lhs2

   lhs_fvs    = exprFreeVars lhs2
   unbound    = filterOut (`elemVarSet` lhs_fvs) orig_bndrs

   orig_bndr_set = mkVarSet orig_bndrs

        -- Add extra dict binders: Note [Free dictionaries]
   mk_extra_dict_bndrs fn_id args
     = [ mkLocalId (localiseName (idName d)) (idType d)
       | d <- varSetElems (exprsFreeVars args `delVarSetList` (fn_id : orig_bndrs))
              -- fn_id: do not quantify over the function itself, which may
              -- itself be a dictionary (in pathological cases, Trac #10251)
       , isDictId d ]

   decompose (Var fn_id) args
      | not (fn_id `elemVarSet` orig_bndr_set)
      = Just (fn_id, args)

   decompose _ _ = Nothing

   bad_shape_msg = hang (ptext (sLit "RULE left-hand side too complicated to desugar"))
                      2 (vcat [ text "Optimised lhs:" <+> ppr lhs2
                              , text "Orig lhs:" <+> ppr orig_lhs])
   dead_msg bndr = hang (sep [ ptext (sLit "Forall'd") <+> pp_bndr bndr
                             , ptext (sLit "is not bound in RULE lhs")])
                      2 (vcat [ text "Orig bndrs:" <+> ppr orig_bndrs
                              , text "Orig lhs:" <+> ppr orig_lhs
                              , text "optimised lhs:" <+> ppr lhs2 ])
   pp_bndr bndr
    | isTyVar bndr                      = ptext (sLit "type variable") <+> quotes (ppr bndr)
    | Just pred <- evVarPred_maybe bndr = ptext (sLit "constraint") <+> quotes (ppr pred)
    | otherwise                         = ptext (sLit "variable") <+> quotes (ppr bndr)

   drop_dicts :: CoreExpr -> CoreExpr
   drop_dicts e
       = wrap_lets needed bnds body
     where
       needed = orig_bndr_set `minusVarSet` exprFreeVars body
       (bnds, body) = split_lets (occurAnalyseExpr e)
           -- The occurAnalyseExpr drops dead bindings which is
           -- crucial to ensure that every binding is used later;
           -- which in turn makes wrap_lets work right

   split_lets :: CoreExpr -> ([(DictId,CoreExpr)], CoreExpr)
   split_lets e
     | Let (NonRec d r) body <- e
     , isDictId d
     , (bs, body') <- split_lets body
     = ((d,r):bs, body')
     | otherwise
     = ([], e)

   wrap_lets :: VarSet -> [(DictId,CoreExpr)] -> CoreExpr -> CoreExpr
   wrap_lets _ [] body = body
   wrap_lets needed ((d, r) : bs) body
     | rhs_fvs `intersectsVarSet` needed = Let (NonRec d r) (wrap_lets needed' bs body)
     | otherwise                         = wrap_lets needed bs body
     where
       rhs_fvs = exprFreeVars r
       needed' = (needed `minusVarSet` rhs_fvs) `extendVarSet` d

{-
Note [Decomposing the left-hand side of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are several things going on here.
* drop_dicts: see Note [Drop dictionary bindings on rule LHS]
* simpleOptExpr: see Note [Simplify rule LHS]
* extra_dict_bndrs: see Note [Free dictionaries]

Note [Drop dictionary bindings on rule LHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drop_dicts drops dictionary bindings on the LHS where possible.
   E.g.  let d:Eq [Int] = $fEqList $fEqInt in f d
     --> f d
   Reasoning here is that there is only one d:Eq [Int], and so we can
   quantify over it. That makes 'd' free in the LHS, but that is later
   picked up by extra_dict_bndrs (Note [Dead spec binders]).

   NB 1: We can only drop the binding if the RHS doesn't bind
         one of the orig_bndrs, which we assume occur on RHS.
         Example
            f :: (Eq a) => b -> a -> a
            {-# SPECIALISE f :: Eq a => b -> [a] -> [a] #-}
         Here we want to end up with
            RULE forall d:Eq a.  f ($dfEqList d) = f_spec d
         Of course, the ($dfEqlist d) in the pattern makes it less likely
         to match, but there is no other way to get d:Eq a

   NB 2: We do drop_dicts *before* simplOptEpxr, so that we expect all
         the evidence bindings to be wrapped around the outside of the
         LHS.  (After simplOptExpr they'll usually have been inlined.)
         dsHsWrapper does dependency analysis, so that civilised ones
         will be simple NonRec bindings.  We don't handle recursive
         dictionaries!

    NB3: In the common case of a non-overloaded, but perhaps-polymorphic
         specialisation, we don't need to bind *any* dictionaries for use
         in the RHS. For example (Trac #8331)
             {-# SPECIALIZE INLINE useAbstractMonad :: ReaderST s Int #-}
             useAbstractMonad :: MonadAbstractIOST m => m Int
         Here, deriving (MonadAbstractIOST (ReaderST s)) is a lot of code
         but the RHS uses no dictionaries, so we want to end up with
             RULE forall s (d :: MonadAbstractIOST (ReaderT s)).
                useAbstractMonad (ReaderT s) d = $suseAbstractMonad s

   Trac #8848 is a good example of where there are some intersting
   dictionary bindings to discard.

The drop_dicts algorithm is based on these observations:

  * Given (let d = rhs in e) where d is a DictId,
    matching 'e' will bind e's free variables.

  * So we want to keep the binding if one of the needed variables (for
    which we need a binding) is in fv(rhs) but not already in fv(e).

  * The "needed variables" are simply the orig_bndrs.  Consider
       f :: (Eq a, Show b) => a -> b -> String
       ... SPECIALISE f :: (Show b) => Int -> b -> String ...
    Then orig_bndrs includes the *quantified* dictionaries of the type
    namely (dsb::Show b), but not the one for Eq Int

So we work inside out, applying the above criterion at each step.


Note [Simplify rule LHS]
~~~~~~~~~~~~~~~~~~~~~~~~
simplOptExpr occurrence-analyses and simplifies the LHS:

   (a) Inline any remaining dictionary bindings (which hopefully
       occur just once)

   (b) Substitute trivial lets so that they don't get in the way
       Note that we substitute the function too; we might
       have this as a LHS:  let f71 = M.f Int in f71

   (c) Do eta reduction.  To see why, consider the fold/build rule,
       which without simplification looked like:
          fold k z (build (/\a. g a))  ==>  ...
       This doesn't match unless you do eta reduction on the build argument.
       Similarly for a LHS like
         augment g (build h)
       we do not want to get
         augment (\a. g a) (build h)
       otherwise we don't match when given an argument like
          augment (\a. h a a) (build h)

Note [Matching seqId]
~~~~~~~~~~~~~~~~~~~
The desugarer turns (seq e r) into (case e of _ -> r), via a special-case hack
and this code turns it back into an application of seq!
See Note [Rules for seq] in MkId for the details.

Note [Unused spec binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        f :: a -> a
        ... SPECIALISE f :: Eq a => a -> a ...
It's true that this *is* a more specialised type, but the rule
we get is something like this:
        f_spec d = f
        RULE: f = f_spec d
Note that the rule is bogus, because it mentions a 'd' that is
not bound on the LHS!  But it's a silly specialisation anyway, because
the constraint is unused.  We could bind 'd' to (error "unused")
but it seems better to reject the program because it's almost certainly
a mistake.  That's what the isDeadBinder call detects.

Note [Free dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~
When the LHS of a specialisation rule, (/\as\ds. f es) has a free dict,
which is presumably in scope at the function definition site, we can quantify
over it too.  *Any* dict with that type will do.

So for example when you have
        f :: Eq a => a -> a
        f = <rhs>
        ... SPECIALISE f :: Int -> Int ...

Then we get the SpecPrag
        SpecPrag (f Int dInt)

And from that we want the rule

        RULE forall dInt. f Int dInt = f_spec
        f_spec = let f = <rhs> in f Int dInt

But be careful!  That dInt might be GHC.Base.$fOrdInt, which is an External
Name, and you can't bind them in a lambda or forall without getting things
confused.   Likewise it might have an InlineRule or something, which would be
utterly bogus. So we really make a fresh Id, with the same unique and type
as the old one, but with an Internal name and no IdInfo.


************************************************************************
*                                                                      *
                Desugaring evidence
*                                                                      *
************************************************************************

-}

dsHsWrapper :: HsWrapper -> CoreExpr -> DsM CoreExpr
dsHsWrapper WpHole            e = return e
dsHsWrapper (WpTyApp ty)      e = return $ App e (Type ty)
dsHsWrapper (WpLet ev_binds)  e = do bs <- dsTcEvBinds ev_binds
                                     return (mkCoreLets bs e)
dsHsWrapper (WpCompose c1 c2) e = do { e1 <- dsHsWrapper c2 e
                                     ; dsHsWrapper c1 e1 }
dsHsWrapper (WpFun c1 c2 t1 _) e = do { x <- newSysLocalDs t1
                                      ; e1 <- dsHsWrapper c1 (Var x)
                                      ; e2 <- dsHsWrapper c2 (e `mkCoreAppDs` e1)
                                      ; return (Lam x e2) }
dsHsWrapper (WpCast co)       e = ASSERT(tcCoercionRole co == Representational)
                                  dsTcCoercion co (mkCastDs e)
dsHsWrapper (WpEvLam ev)      e = return $ Lam ev e
dsHsWrapper (WpTyLam tv)      e = return $ Lam tv e
dsHsWrapper (WpEvApp    tm)   e = liftM (App e) (dsEvTerm tm)

--------------------------------------
dsTcEvBinds_s :: [TcEvBinds] -> DsM [CoreBind]
dsTcEvBinds_s []       = return []
dsTcEvBinds_s (b:rest) = ASSERT( null rest )  -- Zonker ensures null
                         dsTcEvBinds b

dsTcEvBinds :: TcEvBinds -> DsM [CoreBind]
dsTcEvBinds (TcEvBinds {}) = panic "dsEvBinds"    -- Zonker has got rid of this
dsTcEvBinds (EvBinds bs)   = dsEvBinds bs

dsEvBinds :: Bag EvBind -> DsM [CoreBind]
dsEvBinds bs = mapM ds_scc (sccEvBinds bs)
  where
    ds_scc (AcyclicSCC (EvBind { eb_lhs = v, eb_rhs = r }))
                          = liftM (NonRec v) (dsEvTerm r)
    ds_scc (CyclicSCC bs) = liftM Rec (mapM ds_pair bs)

    ds_pair (EvBind { eb_lhs = v, eb_rhs = r }) = liftM ((,) v) (dsEvTerm r)

sccEvBinds :: Bag EvBind -> [SCC EvBind]
sccEvBinds bs = stronglyConnCompFromEdgedVertices edges
  where
    edges :: [(EvBind, EvVar, [EvVar])]
    edges = foldrBag ((:) . mk_node) [] bs

    mk_node :: EvBind -> (EvBind, EvVar, [EvVar])
    mk_node b@(EvBind { eb_lhs = var, eb_rhs = term })
       = (b, var, varSetElems (evVarsOfTerm term))


---------------------------------------
dsEvTerm :: EvTerm -> DsM CoreExpr
dsEvTerm (EvId v) = return (Var v)

dsEvTerm (EvCast tm co)
  = do { tm' <- dsEvTerm tm
       ; dsTcCoercion co $ mkCastDs tm' }
                        -- 'v' is always a lifted evidence variable so it is
                        -- unnecessary to call varToCoreExpr v here.

dsEvTerm (EvDFunApp df tys tms)     = return (Var df `mkTyApps` tys `mkApps` (map Var tms))
dsEvTerm (EvCoercion (TcCoVarCo v)) = return (Var v)  -- See Note [Simple coercions]
dsEvTerm (EvCoercion co)            = dsTcCoercion co mkEqBox
dsEvTerm (EvSuperClass d n)
  = do { d' <- dsEvTerm d
       ; let (cls, tys) = getClassPredTys (exprType d')
             sc_sel_id  = classSCSelId cls n    -- Zero-indexed
       ; return $ Var sc_sel_id `mkTyApps` tys `App` d' }

dsEvTerm (EvDelayedError ty msg) = return $ Var errorId `mkTyApps` [ty] `mkApps` [litMsg]
  where
    errorId = tYPE_ERROR_ID
    litMsg  = Lit (MachStr (fastStringToByteString msg))

dsEvTerm (EvLit l) =
  case l of
    EvNum n -> mkIntegerExpr n
    EvStr s -> mkStringExprFS s

dsEvTerm (EvCallStack cs) = dsEvCallStack cs

dsEvTerm (EvTypeable ev) = dsEvTypeable ev

dsEvTypeable :: EvTypeable -> DsM CoreExpr
dsEvTypeable ev =
  do tyCl      <- dsLookupTyCon typeableClassName
     typeRepTc <- dsLookupTyCon typeRepTyConName
     let tyRepType = mkTyConApp typeRepTc []

     (ty, rep) <-
        case ev of

          EvTypeableTyCon tc ks ->
            do ctr       <- dsLookupGlobalId mkPolyTyConAppName
               mkTyCon   <- dsLookupGlobalId mkTyConName
               dflags    <- getDynFlags
               let mkRep cRep kReps tReps =
                     mkApps (Var ctr) [ cRep, mkListExpr tyRepType kReps
                                            , mkListExpr tyRepType tReps ]

               let kindRep k =
                     case splitTyConApp_maybe k of
                       Nothing -> panic "dsEvTypeable: not a kind constructor"
                       Just (kc,ks) ->
                         do kcRep <- tyConRep dflags mkTyCon kc
                            reps  <- mapM kindRep ks
                            return (mkRep kcRep [] reps)

               tcRep     <- tyConRep dflags mkTyCon tc

               kReps     <- mapM kindRep ks

               return ( mkTyConApp tc ks
                      , mkRep tcRep kReps []
                      )

          EvTypeableTyApp t1 t2 ->
            do e1  <- getRep tyCl t1
               e2  <- getRep tyCl t2
               ctr <- dsLookupGlobalId mkAppTyName

               return ( mkAppTy (snd t1) (snd t2)
                      , mkApps (Var ctr) [ e1, e2 ]
                      )

          EvTypeableTyLit t ->
            do e <- tyLitRep t
               return (snd t, e)

     -- TyRep -> Typeable t
     -- see also: Note [Memoising typeOf]
     repName <- newSysLocalDs tyRepType
     let proxyT = mkProxyPrimTy (typeKind ty) ty
         method = bindNonRec repName rep
                $ mkLams [mkWildValBinder proxyT] (Var repName)

     -- package up the method as `Typeable` dictionary
     return $ mkCastDs method $ mkSymCo $ getTypeableCo tyCl ty

  where
  -- co: method -> Typeable k t
  getTypeableCo tc t =
    case instNewTyCon_maybe tc [typeKind t, t] of
      Just (_,co) -> co
      _           -> panic "Class `Typeable` is not a `newtype`."

  -- Typeable t -> TyRep
  getRep tc (ev,t) =
    do typeableExpr <- dsEvTerm ev
       let co     = getTypeableCo tc t
           method = mkCastDs typeableExpr co
           proxy  = mkTyApps (Var proxyHashId) [typeKind t, t]
       return (mkApps method [proxy])

  -- KnownNat t -> TyRep      (also used for KnownSymbol)
  tyLitRep (ev,t) =
    do dict <- dsEvTerm ev
       fun  <- dsLookupGlobalId $
               case typeKind t of
                 k | eqType k typeNatKind    -> typeNatTypeRepName
                   | eqType k typeSymbolKind -> typeSymbolTypeRepName
                   | otherwise -> panic "dsEvTypeable: unknown type lit kind"
       let finst  = mkTyApps (Var fun) [t]
           proxy  = mkTyApps (Var proxyHashId) [typeKind t, t]
       return (mkApps finst [ dict, proxy ])

  -- This part could be cached
  tyConRep dflags mkTyCon tc =
    do pkgStr  <- mkStringExprFS pkg_fs
       modStr  <- mkStringExprFS modl_fs
       nameStr <- mkStringExprFS name_fs
       return (mkApps (Var mkTyCon) [ int64 high, int64 low
                                    , pkgStr, modStr, nameStr
                                    ])
    where
    tycon_name                = tyConName tc
    modl                      = nameModule tycon_name
    pkg                       = modulePackageKey modl

    modl_fs                   = moduleNameFS (moduleName modl)
    pkg_fs                    = packageKeyFS pkg
    name_fs                   = occNameFS (nameOccName tycon_name)
    hash_name_fs
      | isPromotedTyCon tc    = appendFS (mkFastString "$k") name_fs
      | isPromotedDataCon tc  = appendFS (mkFastString "$c") name_fs
      | isTupleTyCon tc &&
        returnsConstraintKind (tyConKind tc)
                              = appendFS (mkFastString "$p") name_fs
      | otherwise             = name_fs

    hashThis = unwords $ map unpackFS [pkg_fs, modl_fs, hash_name_fs]
    Fingerprint high low = fingerprintString hashThis

    int64
      | wORD_SIZE dflags == 4 = mkWord64LitWord64
      | otherwise             = mkWordLit dflags . fromIntegral



{- Note [Memoising typeOf]
~~~~~~~~~~~~~~~~~~~~~~~~~~
See #3245, #9203

IMPORTANT: we don't want to recalculate the TypeRep once per call with
the proxy argument.  This is what went wrong in #3245 and #9203. So we
help GHC by manually keeping the 'rep' *outside* the lambda.
-}





dsEvCallStack :: EvCallStack -> DsM CoreExpr
-- See Note [Overview of implicit CallStacks] in TcEvidence.hs
dsEvCallStack cs = do
  df              <- getDynFlags
  m               <- getModule
  srcLocDataCon   <- dsLookupDataCon srcLocDataConName
  let srcLocTyCon  = dataConTyCon srcLocDataCon
  let srcLocTy     = mkTyConTy srcLocTyCon
  let mkSrcLoc l =
        liftM (mkCoreConApps srcLocDataCon)
              (sequence [ mkStringExpr (showPpr df $ modulePackageKey m)
                        , mkStringExprFS (moduleNameFS $ moduleName m)
                        , mkStringExprFS (srcSpanFile l)
                        , return $ mkIntExprInt df (srcSpanStartLine l)
                        , return $ mkIntExprInt df (srcSpanStartCol l)
                        , return $ mkIntExprInt df (srcSpanEndLine l)
                        , return $ mkIntExprInt df (srcSpanEndCol l)
                        ])

  -- Be careful to use [Char] instead of String here to avoid
  -- unnecessary dependencies on GHC.Base, particularly when
  -- building GHC.Err.absentError
  let callSiteTy = mkBoxedTupleTy [mkListTy charTy, srcLocTy]

  matchId         <- newSysLocalDs $ mkListTy callSiteTy

  callStackDataCon <- dsLookupDataCon callStackDataConName
  let callStackTyCon = dataConTyCon callStackDataCon
  let callStackTy    = mkTyConTy callStackTyCon
  let emptyCS        = mkCoreConApps callStackDataCon [mkNilExpr callSiteTy]
  let pushCS name loc rest =
        mkWildCase rest callStackTy callStackTy
                   [( DataAlt callStackDataCon
                    , [matchId]
                    , mkCoreConApps callStackDataCon
                       [mkConsExpr callSiteTy
                                   (mkCoreTup [name, loc])
                                   (Var matchId)]
                    )]
  let mkPush name loc tm = do
        nameExpr <- mkStringExprFS name
        locExpr <- mkSrcLoc loc
        case tm of
          EvCallStack EvCsEmpty -> return (pushCS nameExpr locExpr emptyCS)
          _ -> do tmExpr  <- dsEvTerm tm
                  -- at this point tmExpr :: IP sym CallStack
                  -- but we need the actual CallStack to pass to pushCS,
                  -- so we use unwrapIP to strip the dictionary wrapper
                  -- See Note [Overview of implicit CallStacks]
                  let ip_co = unwrapIP (exprType tmExpr)
                  return (pushCS nameExpr locExpr (mkCastDs tmExpr ip_co))
  case cs of
    EvCsTop name loc tm -> mkPush name loc tm
    EvCsPushCall name loc tm -> mkPush (occNameFS $ getOccName name) loc tm
    EvCsEmpty -> panic "Cannot have an empty CallStack"

---------------------------------------
dsTcCoercion :: TcCoercion -> (Coercion -> CoreExpr) -> DsM CoreExpr
-- This is the crucial function that moves
-- from TcCoercions to Coercions; see Note [TcCoercions] in Coercion
-- e.g.  dsTcCoercion (trans g1 g2) k
--       = case g1 of EqBox g1# ->
--         case g2 of EqBox g2# ->
--         k (trans g1# g2#)
-- thing_inside will get a coercion at the role requested
dsTcCoercion co thing_inside
  = do { us <- newUniqueSupply
       ; let eqvs_covs :: [(EqVar,CoVar)]
             eqvs_covs = zipWith mk_co_var (varSetElems (coVarsOfTcCo co))
                                           (uniqsFromSupply us)

             subst = mkCvSubst emptyInScopeSet [(eqv, mkCoVarCo cov) | (eqv, cov) <- eqvs_covs]
             result_expr = thing_inside (ds_tc_coercion subst co)
             result_ty   = exprType result_expr

       ; return (foldr (wrap_in_case result_ty) result_expr eqvs_covs) }
  where
    mk_co_var :: Id -> Unique -> (Id, Id)
    mk_co_var eqv uniq = (eqv, mkUserLocal occ uniq ty loc)
       where
         eq_nm = idName eqv
         occ = nameOccName eq_nm
         loc = nameSrcSpan eq_nm
         ty  = mkCoercionType (getEqPredRole (evVarPred eqv)) ty1 ty2
         (ty1, ty2) = getEqPredTys (evVarPred eqv)

    wrap_in_case result_ty (eqv, cov) body
      = case getEqPredRole (evVarPred eqv) of
         Nominal          -> Case (Var eqv) eqv result_ty [(DataAlt eqBoxDataCon, [cov], body)]
         Representational -> Case (Var eqv) eqv result_ty [(DataAlt coercibleDataCon, [cov], body)]
         Phantom          -> panic "wrap_in_case/phantom"

ds_tc_coercion :: CvSubst -> TcCoercion -> Coercion
-- If the incoming TcCoercion if of type (a ~ b)   (resp.  Coercible a b)
--                 the result is of type (a ~# b)  (reps.  a ~# b)
-- The VarEnv maps EqVars of type (a ~ b) to Coercions of type (a ~# b) (resp. and so on)
-- No need for InScope set etc because the
ds_tc_coercion subst tc_co
  = go tc_co
  where
    go (TcRefl r ty)            = Refl r (Coercion.substTy subst ty)
    go (TcTyConAppCo r tc cos)  = mkTyConAppCo r tc (map go cos)
    go (TcAppCo co1 co2)        = mkAppCo (go co1) (go co2)
    go (TcForAllCo tv co)       = mkForAllCo tv' (ds_tc_coercion subst' co)
                              where
                                (subst', tv') = Coercion.substTyVarBndr subst tv
    go (TcAxiomInstCo ax ind cos)
                                = AxiomInstCo ax ind (map go cos)
    go (TcPhantomCo ty1 ty2)    = UnivCo (fsLit "ds_tc_coercion") Phantom ty1 ty2
    go (TcSymCo co)             = mkSymCo (go co)
    go (TcTransCo co1 co2)      = mkTransCo (go co1) (go co2)
    go (TcNthCo n co)           = mkNthCo n (go co)
    go (TcLRCo lr co)           = mkLRCo lr (go co)
    go (TcSubCo co)             = mkSubCo (go co)
    go (TcLetCo bs co)          = ds_tc_coercion (ds_co_binds bs) co
    go (TcCastCo co1 co2)       = mkCoCast (go co1) (go co2)
    go (TcCoVarCo v)            = ds_ev_id subst v
    go (TcAxiomRuleCo co ts cs) = AxiomRuleCo co (map (Coercion.substTy subst) ts) (map go cs)
    go (TcCoercion co)          = co

    ds_co_binds :: TcEvBinds -> CvSubst
    ds_co_binds (EvBinds bs)      = foldl ds_scc subst (sccEvBinds bs)
    ds_co_binds eb@(TcEvBinds {}) = pprPanic "ds_co_binds" (ppr eb)

    ds_scc :: CvSubst -> SCC EvBind -> CvSubst
    ds_scc subst (AcyclicSCC (EvBind { eb_lhs = v, eb_rhs = ev_term }))
      = extendCvSubstAndInScope subst v (ds_co_term subst ev_term)
    ds_scc _ (CyclicSCC other) = pprPanic "ds_scc:cyclic" (ppr other $$ ppr tc_co)

    ds_co_term :: CvSubst -> EvTerm -> Coercion
    ds_co_term subst (EvCoercion tc_co) = ds_tc_coercion subst tc_co
    ds_co_term subst (EvId v)           = ds_ev_id subst v
    ds_co_term subst (EvCast tm co)     = mkCoCast (ds_co_term subst tm) (ds_tc_coercion subst co)
    ds_co_term _ other = pprPanic "ds_co_term" (ppr other $$ ppr tc_co)

    ds_ev_id :: CvSubst -> EqVar -> Coercion
    ds_ev_id subst v
     | Just co <- Coercion.lookupCoVar subst v = co
     | otherwise  = pprPanic "ds_tc_coercion" (ppr v $$ ppr tc_co)

{-
Note [Simple coercions]
~~~~~~~~~~~~~~~~~~~~~~~
We have a special case for coercions that are simple variables.
Suppose   cv :: a ~ b   is in scope
Lacking the special case, if we see
        f a b cv
we'd desguar to
        f a b (case cv of EqBox (cv# :: a ~# b) -> EqBox cv#)
which is a bit stupid.  The special case does the obvious thing.

This turns out to be important when desugaring the LHS of a RULE
(see Trac #7837).  Suppose we have
    normalise        :: (a ~ Scalar a) => a -> a
    normalise_Double :: Double -> Double
    {-# RULES "normalise" normalise = normalise_Double #-}

Then the RULE we want looks like
     forall a, (cv:a~Scalar a).
       normalise a cv = normalise_Double
But without the special case we generate the redundant box/unbox,
which simpleOpt (currently) doesn't remove. So the rule never matches.

Maybe simpleOpt should be smarter.  But it seems like a good plan
to simply never generate the redundant box/unbox in the first place.
-}
