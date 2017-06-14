{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


TcPat: Typechecking patterns
-}

{-# LANGUAGE CPP, RankNTypes, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module TcPat ( tcLetPat, newLetBndr, LetBndrSpec(..)
             , tcPat, tcPat_O, tcPats
             , addDataConStupidTheta, badFieldCon, polyPatSig ) where

#include "HsVersions.h"

import {-# SOURCE #-}   TcExpr( tcSyntaxOp, tcSyntaxOpGen, tcInferSigma )

import HsSyn
import TcHsSyn
import TcSigs( TcPragEnv, lookupPragEnv, addInlinePrags )
import TcRnMonad
import Inst
import Id
import Var
import Name
import RdrName
import TcEnv
import TcMType
import TcValidity( arityErr )
import Type ( pprTyVars )
import TcType
import TcUnify
import TcHsType
import TysWiredIn
import TcEvidence
import TyCon
import DataCon
import PatSyn
import ConLike
import PrelNames
import BasicTypes hiding (SuccessFlag(..))
import DynFlags
import SrcLoc
import VarSet
import Util
import Outputable
import qualified GHC.LanguageExtensions as LangExt
import Control.Arrow  ( second )
import ListSetOps ( getNth )

{-
************************************************************************
*                                                                      *
                External interface
*                                                                      *
************************************************************************
-}

tcLetPat :: (Name -> Maybe TcId)
         -> LetBndrSpec
         -> LPat GhcRn -> ExpSigmaType
         -> TcM a
         -> TcM (LPat GhcTcId, a)
tcLetPat sig_fn no_gen pat pat_ty thing_inside
  = do { bind_lvl <- getTcLevel
       ; let ctxt = LetPat { pc_lvl    = bind_lvl
                           , pc_sig_fn = sig_fn
                           , pc_new    = no_gen }
             penv = PE { pe_lazy = True
                       , pe_ctxt = ctxt
                       , pe_orig = PatOrigin }

       ; tc_lpat pat pat_ty penv thing_inside }

-----------------
tcPats :: HsMatchContext Name
       -> [LPat GhcRn]            -- Patterns,
       -> [ExpSigmaType]         --   and their types
       -> TcM a                  --   and the checker for the body
       -> TcM ([LPat GhcTcId], a)

-- This is the externally-callable wrapper function
-- Typecheck the patterns, extend the environment to bind the variables,
-- do the thing inside, use any existentially-bound dictionaries to
-- discharge parts of the returning LIE, and deal with pattern type
-- signatures

--   1. Initialise the PatState
--   2. Check the patterns
--   3. Check the body
--   4. Check that no existentials escape

tcPats ctxt pats pat_tys thing_inside
  = tc_lpats penv pats pat_tys thing_inside
  where
    penv = PE { pe_lazy = False, pe_ctxt = LamPat ctxt, pe_orig = PatOrigin }

tcPat :: HsMatchContext Name
      -> LPat GhcRn -> ExpSigmaType
      -> TcM a                     -- Checker for body
      -> TcM (LPat GhcTcId, a)
tcPat ctxt = tcPat_O ctxt PatOrigin

-- | A variant of 'tcPat' that takes a custom origin
tcPat_O :: HsMatchContext Name
        -> CtOrigin              -- ^ origin to use if the type needs inst'ing
        -> LPat GhcRn -> ExpSigmaType
        -> TcM a                 -- Checker for body
        -> TcM (LPat GhcTcId, a)
tcPat_O ctxt orig pat pat_ty thing_inside
  = tc_lpat pat pat_ty penv thing_inside
  where
    penv = PE { pe_lazy = False, pe_ctxt = LamPat ctxt, pe_orig = orig }


{-
************************************************************************
*                                                                      *
                PatEnv, PatCtxt, LetBndrSpec
*                                                                      *
************************************************************************
-}

data PatEnv
  = PE { pe_lazy :: Bool        -- True <=> lazy context, so no existentials allowed
       , pe_ctxt :: PatCtxt     -- Context in which the whole pattern appears
       , pe_orig :: CtOrigin    -- origin to use if the pat_ty needs inst'ing
       }

data PatCtxt
  = LamPat   -- Used for lambdas, case etc
       (HsMatchContext Name)

  | LetPat   -- Used only for let(rec) pattern bindings
             -- See Note [Typing patterns in pattern bindings]
       { pc_lvl    :: TcLevel
                   -- Level of the binding group

       , pc_sig_fn :: Name -> Maybe TcId
                   -- Tells the expected type
                   -- for binders with a signature

       , pc_new :: LetBndrSpec
                -- How to make a new binder
       }        -- for binders without signatures

data LetBndrSpec
  = LetLclBndr            -- We are going to generalise, and wrap in an AbsBinds
                          -- so clone a fresh binder for the local monomorphic Id

  | LetGblBndr TcPragEnv  -- Generalisation plan is NoGen, so there isn't going
                          -- to be an AbsBinds; So we must bind the global version
                          -- of the binder right away.
                          -- And here is the inline-pragma information

instance Outputable LetBndrSpec where
  ppr LetLclBndr      = text "LetLclBndr"
  ppr (LetGblBndr {}) = text "LetGblBndr"

makeLazy :: PatEnv -> PatEnv
makeLazy penv = penv { pe_lazy = True }

inPatBind :: PatEnv -> Bool
inPatBind (PE { pe_ctxt = LetPat {} }) = True
inPatBind (PE { pe_ctxt = LamPat {} }) = False

{- *********************************************************************
*                                                                      *
                Binders
*                                                                      *
********************************************************************* -}

tcPatBndr :: PatEnv -> Name -> ExpSigmaType -> TcM (HsWrapper, TcId)
-- (coi, xp) = tcPatBndr penv x pat_ty
-- Then coi : pat_ty ~ typeof(xp)
--
tcPatBndr penv@(PE { pe_ctxt = LetPat { pc_lvl    = bind_lvl
                                      , pc_sig_fn = sig_fn
                                      , pc_new    = no_gen } })
          bndr_name exp_pat_ty
  -- For the LetPat cases, see
  -- Note [Typechecking pattern bindings] in TcBinds

  | Just bndr_id <- sig_fn bndr_name   -- There is a signature
  = do { wrap <- tcSubTypePat penv exp_pat_ty (idType bndr_id)
           -- See Note [Subsumption check at pattern variables]
       ; traceTc "tcPatBndr(sig)" (ppr bndr_id $$ ppr (idType bndr_id) $$ ppr exp_pat_ty)
       ; return (wrap, bndr_id) }

  | otherwise                          -- No signature
  = do { (co, bndr_ty) <- case exp_pat_ty of
             Check pat_ty    -> promoteTcType bind_lvl pat_ty
             Infer infer_res -> ASSERT( bind_lvl == ir_lvl infer_res )
                                -- If we were under a constructor that bumped
                                -- the level, we'd be in checking mode
                                do { bndr_ty <- inferResultToType infer_res
                                   ; return (mkTcNomReflCo bndr_ty, bndr_ty) }
       ; bndr_id <- newLetBndr no_gen bndr_name bndr_ty
       ; traceTc "tcPatBndr(nosig)" (vcat [ ppr bind_lvl
                                          , ppr exp_pat_ty, ppr bndr_ty, ppr co
                                          , ppr bndr_id ])
       ; return (mkWpCastN co, bndr_id) }

tcPatBndr _ bndr_name pat_ty
  = do { pat_ty <- expTypeToType pat_ty
       ; traceTc "tcPatBndr(not let)" (ppr bndr_name $$ ppr pat_ty)
       ; return (idHsWrapper, mkLocalId bndr_name pat_ty) }
               -- Whether or not there is a sig is irrelevant,
               -- as this is local

newLetBndr :: LetBndrSpec -> Name -> TcType -> TcM TcId
-- Make up a suitable Id for the pattern-binder.
-- See Note [Typechecking pattern bindings], item (4) in TcBinds
--
-- In the polymorphic case when we are going to generalise
--    (plan InferGen, no_gen = LetLclBndr), generate a "monomorphic version"
--    of the Id; the original name will be bound to the polymorphic version
--    by the AbsBinds
-- In the monomorphic case when we are not going to generalise
--    (plan NoGen, no_gen = LetGblBndr) there is no AbsBinds,
--    and we use the original name directly
newLetBndr LetLclBndr name ty
  = do { mono_name <- cloneLocalName name
       ; return (mkLocalId mono_name ty) }
newLetBndr (LetGblBndr prags) name ty
  = addInlinePrags (mkLocalId name ty) (lookupPragEnv prags name)

tcSubTypePat :: PatEnv -> ExpSigmaType -> TcSigmaType -> TcM HsWrapper
-- tcSubTypeET with the UserTypeCtxt specialised to GenSigCtxt
-- Used when typechecking patterns
tcSubTypePat penv t1 t2 = tcSubTypeET (pe_orig penv) GenSigCtxt t1 t2

{- Note [Subsumption check at pattern variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we come across a variable with a type signature, we need to do a
subsumption, not equality, check against the context type.  e.g.

    data T = MkT (forall a. a->a)
      f :: forall b. [b]->[b]
      MkT f = blah

Since 'blah' returns a value of type T, its payload is a polymorphic
function of type (forall a. a->a).  And that's enough to bind the
less-polymorphic function 'f', but we need some impedance matching
to witness the instantiation.


************************************************************************
*                                                                      *
                The main worker functions
*                                                                      *
************************************************************************

Note [Nesting]
~~~~~~~~~~~~~~
tcPat takes a "thing inside" over which the pattern scopes.  This is partly
so that tcPat can extend the environment for the thing_inside, but also
so that constraints arising in the thing_inside can be discharged by the
pattern.

This does not work so well for the ErrCtxt carried by the monad: we don't
want the error-context for the pattern to scope over the RHS.
Hence the getErrCtxt/setErrCtxt stuff in tcMultiple
-}

--------------------
type Checker inp out =  forall r.
                          inp
                       -> PatEnv
                       -> TcM r
                       -> TcM (out, r)

tcMultiple :: Checker inp out -> Checker [inp] [out]
tcMultiple tc_pat args penv thing_inside
  = do  { err_ctxt <- getErrCtxt
        ; let loop _ []
                = do { res <- thing_inside
                     ; return ([], res) }

              loop penv (arg:args)
                = do { (p', (ps', res))
                                <- tc_pat arg penv $
                                   setErrCtxt err_ctxt $
                                   loop penv args
                -- setErrCtxt: restore context before doing the next pattern
                -- See note [Nesting] above

                     ; return (p':ps', res) }

        ; loop penv args }

--------------------
tc_lpat :: LPat GhcRn
        -> ExpSigmaType
        -> PatEnv
        -> TcM a
        -> TcM (LPat GhcTcId, a)
tc_lpat (L span pat) pat_ty penv thing_inside
  = setSrcSpan span $
    do  { (pat', res) <- maybeWrapPatCtxt pat (tc_pat penv pat pat_ty)
                                          thing_inside
        ; return (L span pat', res) }

tc_lpats :: PatEnv
         -> [LPat GhcRn] -> [ExpSigmaType]
         -> TcM a
         -> TcM ([LPat GhcTcId], a)
tc_lpats penv pats tys thing_inside
  = ASSERT2( equalLength pats tys, ppr pats $$ ppr tys )
    tcMultiple (\(p,t) -> tc_lpat p t)
                (zipEqual "tc_lpats" pats tys)
                penv thing_inside

--------------------
tc_pat  :: PatEnv
        -> Pat GhcRn
        -> ExpSigmaType  -- Fully refined result type
        -> TcM a                -- Thing inside
        -> TcM (Pat GhcTcId,    -- Translated pattern
                a)              -- Result of thing inside

tc_pat penv (VarPat (L l name)) pat_ty thing_inside
  = do  { (wrap, id) <- tcPatBndr penv name pat_ty
        ; res <- tcExtendIdEnv1 name id thing_inside
        ; pat_ty <- readExpType pat_ty
        ; return (mkHsWrapPat wrap (VarPat (L l id)) pat_ty, res) }

tc_pat penv (ParPat pat) pat_ty thing_inside
  = do  { (pat', res) <- tc_lpat pat pat_ty penv thing_inside
        ; return (ParPat pat', res) }

tc_pat penv (BangPat pat) pat_ty thing_inside
  = do  { (pat', res) <- tc_lpat pat pat_ty penv thing_inside
        ; return (BangPat pat', res) }

tc_pat penv (LazyPat pat) pat_ty thing_inside
  = do  { (pat', (res, pat_ct))
                <- tc_lpat pat pat_ty (makeLazy penv) $
                   captureConstraints thing_inside
                -- Ignore refined penv', revert to penv

        ; emitConstraints pat_ct
        -- captureConstraints/extendConstraints:
        --   see Note [Hopping the LIE in lazy patterns]

        -- Check that the expected pattern type is itself lifted
        ; pat_ty <- readExpType pat_ty
        ; _ <- unifyType Nothing (typeKind pat_ty) liftedTypeKind

        ; return (LazyPat pat', res) }

tc_pat _ (WildPat _) pat_ty thing_inside
  = do  { res <- thing_inside
        ; pat_ty <- expTypeToType pat_ty
        ; return (WildPat pat_ty, res) }

tc_pat penv (AsPat (L nm_loc name) pat) pat_ty thing_inside
  = do  { (wrap, bndr_id) <- setSrcSpan nm_loc (tcPatBndr penv name pat_ty)
        ; (pat', res) <- tcExtendIdEnv1 name bndr_id $
                         tc_lpat pat (mkCheckExpType $ idType bndr_id)
                                 penv thing_inside
            -- NB: if we do inference on:
            --          \ (y@(x::forall a. a->a)) = e
            -- we'll fail.  The as-pattern infers a monotype for 'y', which then
            -- fails to unify with the polymorphic type for 'x'.  This could
            -- perhaps be fixed, but only with a bit more work.
            --
            -- If you fix it, don't forget the bindInstsOfPatIds!
        ; pat_ty <- readExpType pat_ty
        ; return (mkHsWrapPat wrap (AsPat (L nm_loc bndr_id) pat') pat_ty, res) }

tc_pat penv (ViewPat expr pat _) overall_pat_ty thing_inside
  = do  {
         -- Expr must have type `forall a1...aN. OPT' -> B`
         -- where overall_pat_ty is an instance of OPT'.
        ; (expr',expr'_inferred) <- tcInferSigma expr

         -- expression must be a function
        ; let expr_orig = lexprCtOrigin expr
              herald    = text "A view pattern expression expects"
        ; (expr_wrap1, [inf_arg_ty], inf_res_ty)
            <- matchActualFunTys herald expr_orig (Just (unLoc expr)) 1 expr'_inferred
            -- expr_wrap1 :: expr'_inferred "->" (inf_arg_ty -> inf_res_ty)

         -- check that overall pattern is more polymorphic than arg type
        ; expr_wrap2 <- tcSubTypePat penv overall_pat_ty inf_arg_ty
            -- expr_wrap2 :: overall_pat_ty "->" inf_arg_ty

         -- pattern must have inf_res_ty
        ; (pat', res) <- tc_lpat pat (mkCheckExpType inf_res_ty) penv thing_inside

        ; overall_pat_ty <- readExpType overall_pat_ty
        ; let expr_wrap2' = mkWpFun expr_wrap2 idHsWrapper
                                    overall_pat_ty inf_res_ty doc
               -- expr_wrap2' :: (inf_arg_ty -> inf_res_ty) "->"
               --                (overall_pat_ty -> inf_res_ty)
              expr_wrap = expr_wrap2' <.> expr_wrap1
              doc = text "When checking the view pattern function:" <+> (ppr expr)
        ; return (ViewPat (mkLHsWrap expr_wrap expr') pat' overall_pat_ty, res) }

-- Type signatures in patterns
-- See Note [Pattern coercions] below
tc_pat penv (SigPatIn pat sig_ty) pat_ty thing_inside
  = do  { (inner_ty, tv_binds, wcs, wrap) <- tcPatSig (inPatBind penv)
                                                            sig_ty pat_ty
        ; (pat', res) <- tcExtendTyVarEnv2 wcs      $
                         tcExtendTyVarEnv2 tv_binds $
                         tc_lpat pat (mkCheckExpType inner_ty) penv thing_inside
        ; pat_ty <- readExpType pat_ty
        ; return (mkHsWrapPat wrap (SigPatOut pat' inner_ty) pat_ty, res) }

------------------------
-- Lists, tuples, arrays
tc_pat penv (ListPat pats _ Nothing) pat_ty thing_inside
  = do  { (coi, elt_ty) <- matchExpectedPatTy matchExpectedListTy penv pat_ty
        ; (pats', res) <- tcMultiple (\p -> tc_lpat p (mkCheckExpType elt_ty))
                                     pats penv thing_inside
        ; pat_ty <- readExpType pat_ty
        ; return (mkHsWrapPat coi (ListPat pats' elt_ty Nothing) pat_ty, res)
        }

tc_pat penv (ListPat pats _ (Just (_,e))) pat_ty thing_inside
  = do  { tau_pat_ty <- expTypeToType pat_ty
        ; ((pats', res, elt_ty), e')
            <- tcSyntaxOpGen ListOrigin e [SynType (mkCheckExpType tau_pat_ty)]
                                          SynList $
                 \ [elt_ty] ->
                 do { (pats', res) <- tcMultiple (\p -> tc_lpat p (mkCheckExpType elt_ty))
                                                 pats penv thing_inside
                    ; return (pats', res, elt_ty) }
        ; return (ListPat pats' elt_ty (Just (tau_pat_ty,e')), res)
        }

tc_pat penv (PArrPat pats _) pat_ty thing_inside
  = do  { (coi, elt_ty) <- matchExpectedPatTy matchExpectedPArrTy penv pat_ty
        ; (pats', res) <- tcMultiple (\p -> tc_lpat p (mkCheckExpType elt_ty))
                                     pats penv thing_inside
        ; pat_ty <- readExpType pat_ty
        ; return (mkHsWrapPat coi (PArrPat pats' elt_ty) pat_ty, res)
        }

tc_pat penv (TuplePat pats boxity _) pat_ty thing_inside
  = do  { let arity = length pats
              tc = tupleTyCon boxity arity
        ; (coi, arg_tys) <- matchExpectedPatTy (matchExpectedTyConApp tc)
                                               penv pat_ty
                     -- Unboxed tuples have RuntimeRep vars, which we discard:
                     -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
        ; let con_arg_tys = case boxity of Unboxed -> drop arity arg_tys
                                           Boxed   -> arg_tys
        ; (pats', res) <- tc_lpats penv pats (map mkCheckExpType con_arg_tys)
                                   thing_inside

        ; dflags <- getDynFlags

        -- Under flag control turn a pattern (x,y,z) into ~(x,y,z)
        -- so that we can experiment with lazy tuple-matching.
        -- This is a pretty odd place to make the switch, but
        -- it was easy to do.
        ; let
              unmangled_result = TuplePat pats' boxity con_arg_tys
                                 -- pat_ty /= pat_ty iff coi /= IdCo
              possibly_mangled_result
                | gopt Opt_IrrefutableTuples dflags &&
                  isBoxed boxity            = LazyPat (noLoc unmangled_result)
                | otherwise                 = unmangled_result

        ; pat_ty <- readExpType pat_ty
        ; ASSERT( con_arg_tys `equalLength` pats ) -- Syntactically enforced
          return (mkHsWrapPat coi possibly_mangled_result pat_ty, res)
        }

tc_pat penv (SumPat pat alt arity _) pat_ty thing_inside
  = do  { let tc = sumTyCon arity
        ; (coi, arg_tys) <- matchExpectedPatTy (matchExpectedTyConApp tc)
                                               penv pat_ty
        ; -- Drop levity vars, we don't care about them here
          let con_arg_tys = drop arity arg_tys
        ; (pat', res) <- tc_lpat pat (mkCheckExpType (con_arg_tys `getNth` (alt - 1)))
                                 penv thing_inside
        ; pat_ty <- readExpType pat_ty
        ; return (mkHsWrapPat coi (SumPat pat' alt arity con_arg_tys) pat_ty, res)
        }

------------------------
-- Data constructors
tc_pat penv (ConPatIn con arg_pats) pat_ty thing_inside
  = tcConPat penv con pat_ty arg_pats thing_inside

------------------------
-- Literal patterns
tc_pat penv (LitPat simple_lit) pat_ty thing_inside
  = do  { let lit_ty = hsLitType simple_lit
        ; wrap   <- tcSubTypePat penv pat_ty lit_ty
        ; res    <- thing_inside
        ; pat_ty <- readExpType pat_ty
        ; return ( mkHsWrapPat wrap (LitPat (convertLit simple_lit)) pat_ty
                 , res) }

------------------------
-- Overloaded patterns: n, and n+k

-- In the case of a negative literal (the more complicated case),
-- we get
--
--   case v of (-5) -> blah
--
-- becoming
--
--   if v == (negate (fromInteger 5)) then blah else ...
--
-- There are two bits of rebindable syntax:
--   (==)   :: pat_ty -> neg_lit_ty -> Bool
--   negate :: lit_ty -> neg_lit_ty
-- where lit_ty is the type of the overloaded literal 5.
--
-- When there is no negation, neg_lit_ty and lit_ty are the same
tc_pat _ (NPat (L l over_lit) mb_neg eq _) pat_ty thing_inside
  = do  { let orig = LiteralOrigin over_lit
        ; ((lit', mb_neg'), eq')
            <- tcSyntaxOp orig eq [SynType pat_ty, SynAny]
                          (mkCheckExpType boolTy) $
               \ [neg_lit_ty] ->
               let new_over_lit lit_ty = newOverloadedLit over_lit
                                           (mkCheckExpType lit_ty)
               in case mb_neg of
                 Nothing  -> (, Nothing) <$> new_over_lit neg_lit_ty
                 Just neg -> -- Negative literal
                             -- The 'negate' is re-mappable syntax
                   second Just <$>
                   (tcSyntaxOp orig neg [SynRho] (mkCheckExpType neg_lit_ty) $
                    \ [lit_ty] -> new_over_lit lit_ty)

        ; res <- thing_inside
        ; pat_ty <- readExpType pat_ty
        ; return (NPat (L l lit') mb_neg' eq' pat_ty, res) }

{-
Note [NPlusK patterns]
~~~~~~~~~~~~~~~~~~~~~~
From

  case v of x + 5 -> blah

we get

  if v >= 5 then (\x -> blah) (v - 5) else ...

There are two bits of rebindable syntax:
  (>=) :: pat_ty -> lit1_ty -> Bool
  (-)  :: pat_ty -> lit2_ty -> var_ty

lit1_ty and lit2_ty could conceivably be different.
var_ty is the type inferred for x, the variable in the pattern.

If the pushed-down pattern type isn't a tau-type, the two pat_ty's above
could conceivably be different specializations. But this is very much
like the situation in Note [Case branches must be taus] in TcMatches.
So we tauify the pat_ty before proceeding.

Note that we need to type-check the literal twice, because it is used
twice, and may be used at different types. The second HsOverLit stored in the
AST is used for the subtraction operation.
-}

-- See Note [NPlusK patterns]
tc_pat penv (NPlusKPat (L nm_loc name) (L loc lit) _ ge minus _) pat_ty thing_inside
  = do  { pat_ty <- expTypeToType pat_ty
        ; let orig = LiteralOrigin lit
        ; (lit1', ge')
            <- tcSyntaxOp orig ge [synKnownType pat_ty, SynRho]
                                  (mkCheckExpType boolTy) $
               \ [lit1_ty] ->
               newOverloadedLit lit (mkCheckExpType lit1_ty)
        ; ((lit2', minus_wrap, bndr_id), minus')
            <- tcSyntaxOpGen orig minus [synKnownType pat_ty, SynRho] SynAny $
               \ [lit2_ty, var_ty] ->
               do { lit2' <- newOverloadedLit lit (mkCheckExpType lit2_ty)
                  ; (wrap, bndr_id) <- setSrcSpan nm_loc $
                                     tcPatBndr penv name (mkCheckExpType var_ty)
                           -- co :: var_ty ~ idType bndr_id

                           -- minus_wrap is applicable to minus'
                  ; return (lit2', wrap, bndr_id) }

        -- The Report says that n+k patterns must be in Integral
        -- but it's silly to insist on this in the RebindableSyntax case
        ; unlessM (xoptM LangExt.RebindableSyntax) $
          do { icls <- tcLookupClass integralClassName
             ; instStupidTheta orig [mkClassPred icls [pat_ty]] }

        ; res <- tcExtendIdEnv1 name bndr_id thing_inside

        ; let minus'' = minus' { syn_res_wrap =
                                    minus_wrap <.> syn_res_wrap minus' }
              pat' = NPlusKPat (L nm_loc bndr_id) (L loc lit1') lit2'
                               ge' minus'' pat_ty
        ; return (pat', res) }

-- HsSpliced is an annotation produced by 'RnSplice.rnSplicePat'.
-- Here we get rid of it and add the finalizers to the global environment.
--
-- See Note [Delaying modFinalizers in untyped splices] in RnSplice.
tc_pat penv (SplicePat (HsSpliced mod_finalizers (HsSplicedPat pat)))
            pat_ty thing_inside
  = do addModFinalizersWithLclEnv mod_finalizers
       tc_pat penv pat pat_ty thing_inside

tc_pat _ _other_pat _ _ = panic "tc_pat"        -- ConPatOut, SigPatOut


{-
Note [Hopping the LIE in lazy patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a lazy pattern, we must *not* discharge constraints from the RHS
from dictionaries bound in the pattern.  E.g.
        f ~(C x) = 3
We can't discharge the Num constraint from dictionaries bound by
the pattern C!

So we have to make the constraints from thing_inside "hop around"
the pattern.  Hence the captureConstraints and emitConstraints.

The same thing ensures that equality constraints in a lazy match
are not made available in the RHS of the match. For example
        data T a where { T1 :: Int -> T Int; ... }
        f :: T a -> Int -> a
        f ~(T1 i) y = y
It's obviously not sound to refine a to Int in the right
hand side, because the argument might not match T1 at all!

Finally, a lazy pattern should not bind any existential type variables
because they won't be in scope when we do the desugaring


************************************************************************
*                                                                      *
        Most of the work for constructors is here
        (the rest is in the ConPatIn case of tc_pat)
*                                                                      *
************************************************************************

[Pattern matching indexed data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following declarations:

  data family Map k :: * -> *
  data instance Map (a, b) v = MapPair (Map a (Pair b v))

and a case expression

  case x :: Map (Int, c) w of MapPair m -> ...

As explained by [Wrappers for data instance tycons] in MkIds.hs, the
worker/wrapper types for MapPair are

  $WMapPair :: forall a b v. Map a (Map a b v) -> Map (a, b) v
  $wMapPair :: forall a b v. Map a (Map a b v) -> :R123Map a b v

So, the type of the scrutinee is Map (Int, c) w, but the tycon of MapPair is
:R123Map, which means the straight use of boxySplitTyConApp would give a type
error.  Hence, the smart wrapper function boxySplitTyConAppWithFamily calls
boxySplitTyConApp with the family tycon Map instead, which gives us the family
type list {(Int, c), w}.  To get the correct split for :R123Map, we need to
unify the family type list {(Int, c), w} with the instance types {(a, b), v}
(provided by tyConFamInst_maybe together with the family tycon).  This
unification yields the substitution [a -> Int, b -> c, v -> w], which gives us
the split arguments for the representation tycon :R123Map as {Int, c, w}

In other words, boxySplitTyConAppWithFamily implicitly takes the coercion

  Co123Map a b v :: {Map (a, b) v ~ :R123Map a b v}

moving between representation and family type into account.  To produce type
correct Core, this coercion needs to be used to case the type of the scrutinee
from the family to the representation type.  This is achieved by
unwrapFamInstScrutinee using a CoPat around the result pattern.

Now it might appear seem as if we could have used the previous GADT type
refinement infrastructure of refineAlt and friends instead of the explicit
unification and CoPat generation.  However, that would be wrong.  Why?  The
whole point of GADT refinement is that the refinement is local to the case
alternative.  In contrast, the substitution generated by the unification of
the family type list and instance types needs to be propagated to the outside.
Imagine that in the above example, the type of the scrutinee would have been
(Map x w), then we would have unified {x, w} with {(a, b), v}, yielding the
substitution [x -> (a, b), v -> w].  In contrast to GADT matching, the
instantiation of x with (a, b) must be global; ie, it must be valid in *all*
alternatives of the case expression, whereas in the GADT case it might vary
between alternatives.

RIP GADT refinement: refinements have been replaced by the use of explicit
equality constraints that are used in conjunction with implication constraints
to express the local scope of GADT refinements.
-}

--      Running example:
-- MkT :: forall a b c. (a~[b]) => b -> c -> T a
--       with scrutinee of type (T ty)

tcConPat :: PatEnv -> Located Name
         -> ExpSigmaType           -- Type of the pattern
         -> HsConPatDetails GhcRn -> TcM a
         -> TcM (Pat GhcTcId, a)
tcConPat penv con_lname@(L _ con_name) pat_ty arg_pats thing_inside
  = do  { con_like <- tcLookupConLike con_name
        ; case con_like of
            RealDataCon data_con -> tcDataConPat penv con_lname data_con
                                                 pat_ty arg_pats thing_inside
            PatSynCon pat_syn -> tcPatSynPat penv con_lname pat_syn
                                             pat_ty arg_pats thing_inside
        }

tcDataConPat :: PatEnv -> Located Name -> DataCon
             -> ExpSigmaType               -- Type of the pattern
             -> HsConPatDetails GhcRn -> TcM a
             -> TcM (Pat GhcTcId, a)
tcDataConPat penv (L con_span con_name) data_con pat_ty arg_pats thing_inside
  = do  { let tycon = dataConTyCon data_con
                  -- For data families this is the representation tycon
              (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _)
                = dataConFullSig data_con
              header = L con_span (RealDataCon data_con)

          -- Instantiate the constructor type variables [a->ty]
          -- This may involve doing a family-instance coercion,
          -- and building a wrapper
        ; (wrap, ctxt_res_tys) <- matchExpectedConTy penv tycon pat_ty
        ; pat_ty <- readExpType pat_ty

          -- Add the stupid theta
        ; setSrcSpan con_span $ addDataConStupidTheta data_con ctxt_res_tys

        ; let all_arg_tys = eqSpecPreds eq_spec ++ theta ++ arg_tys
        ; checkExistentials ex_tvs all_arg_tys penv
        ; (tenv, ex_tvs') <- tcInstSuperSkolTyVarsX
                               (zipTvSubst univ_tvs ctxt_res_tys) ex_tvs
                     -- Get location from monad, not from ex_tvs

        ; let -- pat_ty' = mkTyConApp tycon ctxt_res_tys
              -- pat_ty' is type of the actual constructor application
              -- pat_ty' /= pat_ty iff coi /= IdCo

              arg_tys' = substTys tenv arg_tys

        ; traceTc "tcConPat" (vcat [ ppr con_name
                                   , pprTyVars univ_tvs
                                   , pprTyVars ex_tvs
                                   , ppr eq_spec
                                   , ppr theta
                                   , pprTyVars ex_tvs'
                                   , ppr ctxt_res_tys
                                   , ppr arg_tys'
                                   , ppr arg_pats ])
        ; if null ex_tvs && null eq_spec && null theta
          then do { -- The common case; no class bindings etc
                    -- (see Note [Arrows and patterns])
                    (arg_pats', res) <- tcConArgs (RealDataCon data_con) arg_tys'
                                                  arg_pats penv thing_inside
                  ; let res_pat = ConPatOut { pat_con = header,
                                              pat_tvs = [], pat_dicts = [],
                                              pat_binds = emptyTcEvBinds,
                                              pat_args = arg_pats',
                                              pat_arg_tys = ctxt_res_tys,
                                              pat_wrap = idHsWrapper }

                  ; return (mkHsWrapPat wrap res_pat pat_ty, res) }

          else do   -- The general case, with existential,
                    -- and local equality constraints
        { let theta'     = substTheta tenv (eqSpecPreds eq_spec ++ theta)
                           -- order is *important* as we generate the list of
                           -- dictionary binders from theta'
              no_equalities = not (any isNomEqPred theta')
              skol_info = PatSkol (RealDataCon data_con) mc
              mc = case pe_ctxt penv of
                     LamPat mc -> mc
                     LetPat {} -> PatBindRhs

        ; gadts_on    <- xoptM LangExt.GADTs
        ; families_on <- xoptM LangExt.TypeFamilies
        ; checkTc (no_equalities || gadts_on || families_on)
                  (text "A pattern match on a GADT requires the" <+>
                   text "GADTs or TypeFamilies language extension")
                  -- Trac #2905 decided that a *pattern-match* of a GADT
                  -- should require the GADT language flag.
                  -- Re TypeFamilies see also #7156

        ; given <- newEvVars theta'
        ; (ev_binds, (arg_pats', res))
             <- checkConstraints skol_info ex_tvs' given $
                tcConArgs (RealDataCon data_con) arg_tys' arg_pats penv thing_inside

        ; let res_pat = ConPatOut { pat_con   = header,
                                    pat_tvs   = ex_tvs',
                                    pat_dicts = given,
                                    pat_binds = ev_binds,
                                    pat_args  = arg_pats',
                                    pat_arg_tys = ctxt_res_tys,
                                    pat_wrap  = idHsWrapper }
        ; return (mkHsWrapPat wrap res_pat pat_ty, res)
        } }

tcPatSynPat :: PatEnv -> Located Name -> PatSyn
            -> ExpSigmaType                -- Type of the pattern
            -> HsConPatDetails GhcRn -> TcM a
            -> TcM (Pat GhcTcId, a)
tcPatSynPat penv (L con_span _) pat_syn pat_ty arg_pats thing_inside
  = do  { let (univ_tvs, req_theta, ex_tvs, prov_theta, arg_tys, ty) = patSynSig pat_syn

        ; (subst, univ_tvs') <- newMetaTyVars univ_tvs

        ; let all_arg_tys = ty : prov_theta ++ arg_tys
        ; checkExistentials ex_tvs all_arg_tys penv
        ; (tenv, ex_tvs') <- tcInstSuperSkolTyVarsX subst ex_tvs
        ; let ty'         = substTy tenv ty
              arg_tys'    = substTys tenv arg_tys
              prov_theta' = substTheta tenv prov_theta
              req_theta'  = substTheta tenv req_theta

        ; wrap <- tcSubTypePat penv pat_ty ty'
        ; traceTc "tcPatSynPat" (ppr pat_syn $$
                                 ppr pat_ty $$
                                 ppr ty' $$
                                 ppr ex_tvs' $$
                                 ppr prov_theta' $$
                                 ppr req_theta' $$
                                 ppr arg_tys')

        ; prov_dicts' <- newEvVars prov_theta'

        ; let skol_info = case pe_ctxt penv of
                            LamPat mc -> PatSkol (PatSynCon pat_syn) mc
                            LetPat {} -> UnkSkol -- Doesn't matter

        ; req_wrap <- instCall PatOrigin (mkTyVarTys univ_tvs') req_theta'
        ; traceTc "instCall" (ppr req_wrap)

        ; traceTc "checkConstraints {" Outputable.empty
        ; (ev_binds, (arg_pats', res))
             <- checkConstraints skol_info ex_tvs' prov_dicts' $
                tcConArgs (PatSynCon pat_syn) arg_tys' arg_pats penv thing_inside

        ; traceTc "checkConstraints }" (ppr ev_binds)
        ; let res_pat = ConPatOut { pat_con   = L con_span $ PatSynCon pat_syn,
                                    pat_tvs   = ex_tvs',
                                    pat_dicts = prov_dicts',
                                    pat_binds = ev_binds,
                                    pat_args  = arg_pats',
                                    pat_arg_tys = mkTyVarTys univ_tvs',
                                    pat_wrap  = req_wrap }
        ; pat_ty <- readExpType pat_ty
        ; return (mkHsWrapPat wrap res_pat pat_ty, res) }

----------------------------
-- | Convenient wrapper for calling a matchExpectedXXX function
matchExpectedPatTy :: (TcRhoType -> TcM (TcCoercionN, a))
                    -> PatEnv -> ExpSigmaType -> TcM (HsWrapper, a)
-- See Note [Matching polytyped patterns]
-- Returns a wrapper : pat_ty ~R inner_ty
matchExpectedPatTy inner_match (PE { pe_orig = orig }) pat_ty
  = do { pat_ty <- expTypeToType pat_ty
       ; (wrap, pat_rho) <- topInstantiate orig pat_ty
       ; (co, res) <- inner_match pat_rho
       ; traceTc "matchExpectedPatTy" (ppr pat_ty $$ ppr wrap)
       ; return (mkWpCastN (mkTcSymCo co) <.> wrap, res) }

----------------------------
matchExpectedConTy :: PatEnv
                   -> TyCon      -- The TyCon that this data
                                 -- constructor actually returns
                                 -- In the case of a data family this is
                                 -- the /representation/ TyCon
                   -> ExpSigmaType  -- The type of the pattern; in the case
                                    -- of a data family this would mention
                                    -- the /family/ TyCon
                   -> TcM (HsWrapper, [TcSigmaType])
-- See Note [Matching constructor patterns]
-- Returns a wrapper : pat_ty "->" T ty1 ... tyn
matchExpectedConTy (PE { pe_orig = orig }) data_tc exp_pat_ty
  | Just (fam_tc, fam_args, co_tc) <- tyConFamInstSig_maybe data_tc
         -- Comments refer to Note [Matching constructor patterns]
         -- co_tc :: forall a. T [a] ~ T7 a
  = do { pat_ty <- expTypeToType exp_pat_ty
       ; (wrap, pat_rho) <- topInstantiate orig pat_ty

       ; (subst, tvs') <- newMetaTyVars (tyConTyVars data_tc)
             -- tys = [ty1,ty2]

       ; traceTc "matchExpectedConTy" (vcat [ppr data_tc,
                                             ppr (tyConTyVars data_tc),
                                             ppr fam_tc, ppr fam_args,
                                             ppr exp_pat_ty,
                                             ppr pat_ty,
                                             ppr pat_rho, ppr wrap])
       ; co1 <- unifyType Nothing (mkTyConApp fam_tc (substTys subst fam_args)) pat_rho
             -- co1 : T (ty1,ty2) ~N pat_rho
             -- could use tcSubType here... but it's the wrong way round
             -- for actual vs. expected in error messages.

       ; let tys' = mkTyVarTys tvs'
             co2 = mkTcUnbranchedAxInstCo co_tc tys' []
             -- co2 : T (ty1,ty2) ~R T7 ty1 ty2

             full_co = mkTcSubCo (mkTcSymCo co1) `mkTcTransCo` co2
             -- full_co :: pat_rho ~R T7 ty1 ty2

       ; return ( mkWpCastR full_co <.> wrap, tys') }

  | otherwise
  = do { pat_ty <- expTypeToType exp_pat_ty
       ; (wrap, pat_rho) <- topInstantiate orig pat_ty
       ; (coi, tys) <- matchExpectedTyConApp data_tc pat_rho
       ; return (mkWpCastN (mkTcSymCo coi) <.> wrap, tys) }

{-
Note [Matching constructor patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose (coi, tys) = matchExpectedConType data_tc pat_ty

 * In the simple case, pat_ty = tc tys

 * If pat_ty is a polytype, we want to instantiate it
   This is like part of a subsumption check.  Eg
      f :: (forall a. [a]) -> blah
      f [] = blah

 * In a type family case, suppose we have
          data family T a
          data instance T (p,q) = A p | B q
       Then we'll have internally generated
              data T7 p q = A p | B q
              axiom coT7 p q :: T (p,q) ~ T7 p q

       So if pat_ty = T (ty1,ty2), we return (coi, [ty1,ty2]) such that
           coi = coi2 . coi1 : T7 t ~ pat_ty
           coi1 : T (ty1,ty2) ~ pat_ty
           coi2 : T7 ty1 ty2 ~ T (ty1,ty2)

   For families we do all this matching here, not in the unifier,
   because we never want a whisper of the data_tycon to appear in
   error messages; it's a purely internal thing
-}

tcConArgs :: ConLike -> [TcSigmaType]
          -> Checker (HsConPatDetails GhcRn) (HsConPatDetails GhcTc)

tcConArgs con_like arg_tys (PrefixCon arg_pats) penv thing_inside
  = do  { checkTc (con_arity == no_of_args)     -- Check correct arity
                  (arityErr (text "constructor") con_like con_arity no_of_args)
        ; let pats_w_tys = zipEqual "tcConArgs" arg_pats arg_tys
        ; (arg_pats', res) <- tcMultiple tcConArg pats_w_tys
                                              penv thing_inside
        ; return (PrefixCon arg_pats', res) }
  where
    con_arity  = conLikeArity con_like
    no_of_args = length arg_pats

tcConArgs con_like arg_tys (InfixCon p1 p2) penv thing_inside
  = do  { checkTc (con_arity == 2)      -- Check correct arity
                  (arityErr (text "constructor") con_like con_arity 2)
        ; let [arg_ty1,arg_ty2] = arg_tys       -- This can't fail after the arity check
        ; ([p1',p2'], res) <- tcMultiple tcConArg [(p1,arg_ty1),(p2,arg_ty2)]
                                              penv thing_inside
        ; return (InfixCon p1' p2', res) }
  where
    con_arity  = conLikeArity con_like

tcConArgs con_like arg_tys (RecCon (HsRecFields rpats dd)) penv thing_inside
  = do  { (rpats', res) <- tcMultiple tc_field rpats penv thing_inside
        ; return (RecCon (HsRecFields rpats' dd), res) }
  where
    tc_field :: Checker (LHsRecField GhcRn (LPat GhcRn))
                        (LHsRecField GhcTcId (LPat GhcTcId))
    tc_field (L l (HsRecField (L loc (FieldOcc (L lr rdr) sel)) pat pun)) penv
                                                                    thing_inside
      = do { sel'   <- tcLookupId sel
           ; pat_ty <- setSrcSpan loc $ find_field_ty (occNameFS $ rdrNameOcc rdr)
           ; (pat', res) <- tcConArg (pat, pat_ty) penv thing_inside
           ; return (L l (HsRecField (L loc (FieldOcc (L lr rdr) sel')) pat'
                                                                    pun), res) }

    find_field_ty :: FieldLabelString -> TcM TcType
    find_field_ty lbl
        = case [ty | (fl, ty) <- field_tys, flLabel fl == lbl] of

                -- No matching field; chances are this field label comes from some
                -- other record type (or maybe none).  If this happens, just fail,
                -- otherwise we get crashes later (Trac #8570), and similar:
                --      f (R { foo = (a,b) }) = a+b
                -- If foo isn't one of R's fields, we don't want to crash when
                -- typechecking the "a+b".
           [] -> failWith (badFieldCon con_like lbl)

                -- The normal case, when the field comes from the right constructor
           (pat_ty : extras) -> do
                traceTc "find_field" (ppr pat_ty <+> ppr extras)
                ASSERT( null extras ) (return pat_ty)

    field_tys :: [(FieldLabel, TcType)]
    field_tys = zip (conLikeFieldLabels con_like) arg_tys
          -- Don't use zipEqual! If the constructor isn't really a record, then
          -- dataConFieldLabels will be empty (and each field in the pattern
          -- will generate an error below).

tcConArg :: Checker (LPat GhcRn, TcSigmaType) (LPat GhcTc)
tcConArg (arg_pat, arg_ty) penv thing_inside
  = tc_lpat arg_pat (mkCheckExpType arg_ty) penv thing_inside

addDataConStupidTheta :: DataCon -> [TcType] -> TcM ()
-- Instantiate the "stupid theta" of the data con, and throw
-- the constraints into the constraint set
addDataConStupidTheta data_con inst_tys
  | null stupid_theta = return ()
  | otherwise         = instStupidTheta origin inst_theta
  where
    origin = OccurrenceOf (dataConName data_con)
        -- The origin should always report "occurrence of C"
        -- even when C occurs in a pattern
    stupid_theta = dataConStupidTheta data_con
    univ_tvs     = dataConUnivTyVars data_con
    tenv = zipTvSubst univ_tvs (takeList univ_tvs inst_tys)
         -- NB: inst_tys can be longer than the univ tyvars
         --     because the constructor might have existentials
    inst_theta = substTheta tenv stupid_theta

{-
Note [Arrows and patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~
(Oct 07) Arrow notation has the odd property that it involves
"holes in the scope". For example:
  expr :: Arrow a => a () Int
  expr = proc (y,z) -> do
          x <- term -< y
          expr' -< x

Here the 'proc (y,z)' binding scopes over the arrow tails but not the
arrow body (e.g 'term').  As things stand (bogusly) all the
constraints from the proc body are gathered together, so constraints
from 'term' will be seen by the tcPat for (y,z).  But we must *not*
bind constraints from 'term' here, because the desugarer will not make
these bindings scope over 'term'.

The Right Thing is not to confuse these constraints together. But for
now the Easy Thing is to ensure that we do not have existential or
GADT constraints in a 'proc', and to short-cut the constraint
simplification for such vanilla patterns so that it binds no
constraints. Hence the 'fast path' in tcConPat; but it's also a good
plan for ordinary vanilla patterns to bypass the constraint
simplification step.

************************************************************************
*                                                                      *
                Note [Pattern coercions]
*                                                                      *
************************************************************************

In principle, these program would be reasonable:

        f :: (forall a. a->a) -> Int
        f (x :: Int->Int) = x 3

        g :: (forall a. [a]) -> Bool
        g [] = True

In both cases, the function type signature restricts what arguments can be passed
in a call (to polymorphic ones).  The pattern type signature then instantiates this
type.  For example, in the first case,  (forall a. a->a) <= Int -> Int, and we
generate the translated term
        f = \x' :: (forall a. a->a).  let x = x' Int in x 3

From a type-system point of view, this is perfectly fine, but it's *very* seldom useful.
And it requires a significant amount of code to implement, because we need to decorate
the translated pattern with coercion functions (generated from the subsumption check
by tcSub).

So for now I'm just insisting on type *equality* in patterns.  No subsumption.

Old notes about desugaring, at a time when pattern coercions were handled:

A SigPat is a type coercion and must be handled one at at time.  We can't
combine them unless the type of the pattern inside is identical, and we don't
bother to check for that.  For example:

        data T = T1 Int | T2 Bool
        f :: (forall a. a -> a) -> T -> t
        f (g::Int->Int)   (T1 i) = T1 (g i)
        f (g::Bool->Bool) (T2 b) = T2 (g b)

We desugar this as follows:

        f = \ g::(forall a. a->a) t::T ->
            let gi = g Int
            in case t of { T1 i -> T1 (gi i)
                           other ->
            let gb = g Bool
            in case t of { T2 b -> T2 (gb b)
                           other -> fail }}

Note that we do not treat the first column of patterns as a
column of variables, because the coerced variables (gi, gb)
would be of different types.  So we get rather grotty code.
But I don't think this is a common case, and if it was we could
doubtless improve it.

Meanwhile, the strategy is:
        * treat each SigPat coercion (always non-identity coercions)
                as a separate block
        * deal with the stuff inside, and then wrap a binding round
                the result to bind the new variable (gi, gb, etc)


************************************************************************
*                                                                      *
\subsection{Errors and contexts}
*                                                                      *
************************************************************************

Note [Existential check]
~~~~~~~~~~~~~~~~~~~~~~~~
Lazy patterns can't bind existentials.  They arise in two ways:
  * Let bindings      let { C a b = e } in b
  * Twiddle patterns  f ~(C a b) = e
The pe_lazy field of PatEnv says whether we are inside a lazy
pattern (perhaps deeply)

See also Note [Typechecking pattern bindings] in TcBinds
-}

maybeWrapPatCtxt :: Pat GhcRn -> (TcM a -> TcM b) -> TcM a -> TcM b
-- Not all patterns are worth pushing a context
maybeWrapPatCtxt pat tcm thing_inside
  | not (worth_wrapping pat) = tcm thing_inside
  | otherwise                = addErrCtxt msg $ tcm $ popErrCtxt thing_inside
                               -- Remember to pop before doing thing_inside
  where
   worth_wrapping (VarPat {}) = False
   worth_wrapping (ParPat {}) = False
   worth_wrapping (AsPat {})  = False
   worth_wrapping _           = True
   msg = hang (text "In the pattern:") 2 (ppr pat)

-----------------------------------------------
checkExistentials :: [TyVar]   -- existentials
                  -> [Type]    -- argument types
                  -> PatEnv -> TcM ()
    -- See Note [Existential check]]
    -- See Note [Arrows and patterns]
checkExistentials ex_tvs tys _
  | all (not . (`elemVarSet` tyCoVarsOfTypes tys)) ex_tvs = return ()
checkExistentials _ _ (PE { pe_ctxt = LetPat {}})         = return ()
checkExistentials _ _ (PE { pe_ctxt = LamPat ProcExpr })  = failWithTc existentialProcPat
checkExistentials _ _ (PE { pe_lazy = True })             = failWithTc existentialLazyPat
checkExistentials _ _ _                                   = return ()

existentialLazyPat :: SDoc
existentialLazyPat
  = hang (text "An existential or GADT data constructor cannot be used")
       2 (text "inside a lazy (~) pattern")

existentialProcPat :: SDoc
existentialProcPat
  = text "Proc patterns cannot use existential or GADT data constructors"

badFieldCon :: ConLike -> FieldLabelString -> SDoc
badFieldCon con field
  = hsep [text "Constructor" <+> quotes (ppr con),
          text "does not have field", quotes (ppr field)]

polyPatSig :: TcType -> SDoc
polyPatSig sig_ty
  = hang (text "Illegal polymorphic type signature in pattern:")
       2 (ppr sig_ty)
