{-
c%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcExpr]{Typecheck an expression}
-}

{-# LANGUAGE CPP #-}

module TcExpr ( tcPolyMonoExpr, tcPolyMonoExprNC,
                tcInferRho, tcInferRhoNC,
                tcSyntaxOp, tcCheckId,
                addExprErrCtxt) where

#include "HsVersions.h"

import {-# SOURCE #-}   TcSplice( tcSpliceExpr, tcTypedBracket, tcUntypedBracket )
import THNames( liftStringName, liftName )

import HsSyn
import TcHsSyn
import TcRnMonad
import TcUnify
import BasicTypes
import Inst
import TcBinds
import FamInst          ( tcGetFamInstEnvs, tcLookupDataFamInst )
import TcEnv
import TcArrows
import TcMatches
import TcHsType
import TcPatSyn( tcPatSynBuilderOcc )
import TcPat
import TcMType
import TcType
import DsMonad
import Id
import ConLike
import DataCon
import Name
import TyCon
import Type
import TcEvidence
import Var
import VarSet
import VarEnv
import TysWiredIn
import TysPrim( intPrimTy, addrPrimTy )
import PrimOp( tagToEnumKey )
import PrelNames
import DynFlags
import SrcLoc
import Util
import ListSetOps
import Maybes
import ErrUtils
import Outputable
import FastString
import Control.Monad
import Class(classTyCon)
import Data.Function
import Data.List
import qualified Data.Set as Set

{-
************************************************************************
*                                                                      *
\subsection{Main wrappers}
*                                                                      *
************************************************************************
-}

tcPolyMonoExpr, tcPolyMonoExprNC
         :: LHsExpr Name        -- Expression to type check
         -> TcSigmaType         -- Expected type (could be a polytype)
         -> TcM (LHsExpr TcId)  -- Generalised expr with expected type

-- tcPolyMonoExpr is a convenient place (frequent but not too frequent)
-- place to add context information.
-- The NC version does not do so, usually because the caller wants
-- to do so himself.

tcPolyMonoExpr expr res_ty
  = addExprErrCtxt expr $
    do { traceTc "tcPolyMonoExpr" (ppr res_ty); tcPolyMonoExprNC expr res_ty }

tcPolyMonoExprNC expr res_ty
  | isSigmaTy res_ty
  = do { traceTc "tcPolyMonoExprNC" (ppr res_ty)
       ; (gen_fn, expr') <- tcGen GenSigCtxt res_ty $ \ _ rho ->
                            tcPolyMonoExprNC expr rho
       ; return (mkLHsWrap gen_fn expr') }
tcPolyMonoExprNC (L loc expr) res_ty
  = setSrcSpan loc $
    do  { expr' <- tcExpr expr res_ty
        ; return (L loc expr') }

---------------
tcInferRho, tcInferRhoNC :: LHsExpr Name -> TcM (LHsExpr TcId, TcRhoType)
-- Infer a *rho*-type.  This is, in effect, a special case
-- for ids and partial applications, so that if
--     f :: Int -> (forall a. a -> a) -> Int
-- then we can infer
--     f 3 :: (forall a. a -> a) -> Int
-- And that in turn is useful
--  (a) for the function part of any application (see tcApp)
--  (b) for the special rule for '$'
tcInferRho expr = addErrCtxt (exprCtxt expr) (tcInferRhoNC expr)

tcInferRhoNC (L loc expr)
  = setSrcSpan loc $
    do { (expr', rho) <- tcInfer (tcExpr expr)
       ; return (L loc expr', rho) }

tcUnboundId :: OccName -> TcRhoType -> TcM (HsExpr TcId)
-- Typechedk an occurrence of an unbound Id
--
-- Some of these started life as a true hole "_".  Others might simply
-- be variables that accidentally have no binding site
--
-- We turn all of them into HsVar, since HsUnboundVar can't contain an
-- Id; and indeed the evidence for the CHoleCan does bind it, so it's
-- not unbound any more!
tcUnboundId occ res_ty
 = do { ty <- newFlexiTyVarTy liftedTypeKind
      ; name <- newSysName occ
      ; let ev = mkLocalId name ty
      ; loc <- getCtLocM HoleOrigin
      ; let can = CHoleCan { cc_ev = CtWanted ty ev loc, cc_occ = occ
                           , cc_hole = ExprHole }
      ; emitInsoluble can
      ; tcWrapResult (HsVar ev) ty res_ty }

{-
************************************************************************
*                                                                      *
        tcExpr: the main expression typechecker
*                                                                      *
************************************************************************
-}

tcExpr :: HsExpr Name -> TcRhoType -> TcM (HsExpr TcId)
tcExpr e res_ty | debugIsOn && isSigmaTy res_ty     -- Sanity check
                = pprPanic "tcExpr: sigma" (ppr res_ty $$ ppr e)

tcExpr (HsVar name)     res_ty = tcCheckId name res_ty
tcExpr (HsUnboundVar v) res_ty = tcUnboundId v res_ty

tcExpr app@(HsApp _ _) res_ty = tcApp app res_ty

tcExpr (HsLit lit)   res_ty = do { let lit_ty = hsLitType lit
                                 ; tcWrapResult (HsLit lit) lit_ty res_ty }

tcExpr (HsPar expr)  res_ty = do { expr' <- tcPolyMonoExprNC expr res_ty
                                 ; return (HsPar expr') }

tcExpr (HsSCC src lbl expr) res_ty
  = do { expr' <- tcPolyMonoExpr expr res_ty
       ; return (HsSCC src lbl expr') }

tcExpr (HsTickPragma src info expr) res_ty
  = do { expr' <- tcPolyMonoExpr expr res_ty
       ; return (HsTickPragma src info expr') }

tcExpr (HsCoreAnn src lbl expr) res_ty
  = do  { expr' <- tcPolyMonoExpr expr res_ty
        ; return (HsCoreAnn src lbl expr') }

tcExpr (HsOverLit lit) res_ty
  = do  { lit' <- newOverloadedLit (LiteralOrigin lit) lit res_ty
        ; return (HsOverLit lit') }

tcExpr (NegApp expr neg_expr) res_ty
  = do  { neg_expr' <- tcSyntaxOp NegateOrigin neg_expr
                                  (mkFunTy res_ty res_ty)
        ; expr' <- tcPolyMonoExpr expr res_ty
        ; return (NegApp expr' neg_expr') }

tcExpr (HsIPVar x) res_ty
  = do { let origin = IPOccOrigin x
       ; ipClass <- tcLookupClass ipClassName
           {- Implicit parameters must have a *tau-type* not a.
              type scheme.  We enforce this by creating a fresh
              type variable as its type.  (Because res_ty may not
              be a tau-type.) -}
       ; ip_ty <- newFlexiTyVarTy openTypeKind
       ; let ip_name = mkStrLitTy (hsIPNameFS x)
       ; ip_var <- emitWanted origin (mkClassPred ipClass [ip_name, ip_ty])
       ; tcWrapResult (fromDict ipClass ip_name ip_ty (HsVar ip_var)) ip_ty res_ty }
  where
  -- Coerces a dictionary for `IP "x" t` into `t`.
  fromDict ipClass x ty = HsWrap $ mkWpCast $ TcCoercion $
                          unwrapIP $ mkClassPred ipClass [x,ty]

tcExpr (HsLam match) res_ty
  = do  { (co_fn, match') <- tcMatchLambda match res_ty
        ; return (mkHsWrap co_fn (HsLam match')) }

tcExpr e@(HsLamCase _ matches) res_ty
  = do  { (co_fn, [arg_ty], body_ty) <- matchExpectedFunTys msg 1 res_ty
        ; matches' <- tcMatchesCase match_ctxt arg_ty matches body_ty
        ; return $ mkHsWrapCo co_fn $ HsLamCase arg_ty matches' }
  where msg = sep [ ptext (sLit "The function") <+> quotes (ppr e)
                  , ptext (sLit "requires")]
        match_ctxt = MC { mc_what = CaseAlt, mc_body = tcBody }

tcExpr (ExprWithTySig expr sig_ty wcs) res_ty
 = do { nwc_tvs <- mapM newWildcardVarMetaKind wcs
      ; tcExtendTyVarEnv nwc_tvs $ do {
        sig_tc_ty <- tcHsSigType ExprSigCtxt sig_ty
      ; (gen_fn, expr')
            <- tcGen ExprSigCtxt sig_tc_ty $ \ skol_tvs res_ty ->

                  -- Remember to extend the lexical type-variable environment
                  -- See Note [More instantiated than scoped] in TcBinds
               tcExtendTyVarEnv2
                  [(n,tv) | (Just n, tv) <- findScopedTyVars sig_ty sig_tc_ty skol_tvs] $

               tcPolyMonoExprNC expr res_ty

      ; let inner_expr = ExprWithTySigOut (mkLHsWrap gen_fn expr') sig_ty

      ; (inst_wrap, rho) <- deeplyInstantiate ExprSigOrigin sig_tc_ty
      ; addErrCtxt (pprSigCtxt ExprSigCtxt empty (ppr sig_ty)) $
        emitWildcardHoleConstraints (zip wcs nwc_tvs)
      ; tcWrapResult (mkHsWrap inst_wrap inner_expr) rho res_ty } }

tcExpr (HsType ty) _
  = failWithTc (text "Can't handle type argument:" <+> ppr ty)
        -- This is the syntax for type applications that I was planning
        -- but there are difficulties (e.g. what order for type args)
        -- so it's not enabled yet.
        -- Can't eliminate it altogether from the parser, because the
        -- same parser parses *patterns*.

{-
************************************************************************
*                                                                      *
                Infix operators and sections
*                                                                      *
************************************************************************

Note [Left sections]
~~~~~~~~~~~~~~~~~~~~
Left sections, like (4 *), are equivalent to
        \ x -> (*) 4 x,
or, if PostfixOperators is enabled, just
        (*) 4
With PostfixOperators we don't actually require the function to take
two arguments at all.  For example, (x `not`) means (not x); you get
postfix operators!  Not Haskell 98, but it's less work and kind of
useful.
-}

tcExpr app@(OpApp _ _ _ _) res_ty = tcApp app res_ty
{-
  | (L loc (HsVar op_name)) <- op
  , op_name `hasKey` dollarIdKey        -- Note [Typing rule for ($)]
  = do { traceTc "Application rule" (ppr op)
       ; (arg1', arg1_ty) <- tcInferRho arg1

       ; let doc = ptext (sLit "The first argument of ($) takes")
       ; (co_arg1, [arg2_ty], op_res_ty) <- matchExpectedFunTys doc 1 arg1_ty

         -- We have (arg1 $ arg2)
         -- So: arg1_ty = arg2_ty -> op_res_ty
         -- where arg2_ty maybe polymorphic; that's the point

       ; arg2' <- tcArg op (arg2, arg2_ty, 2)
       ; co_b  <- unifyType op_res_ty res_ty    -- op_res ~ res

       -- Make sure that the argument type has kind '*'
       --    ($) :: forall (a2:*) (r:Open). (a2->r) -> a2 -> r
       -- Eg we do not want to allow  (D#  $  4.0#)   Trac #5570
       --    (which gives a seg fault)
       -- We do this by unifying with a MetaTv; but of course
       -- it must allow foralls in the type it unifies with (hence ReturnTv)!
       --
       -- The *result* type can have any kind (Trac #8739),
       -- so we don't need to check anything for that
       ; a2_tv <- newReturnTyVar liftedTypeKind
       ; let a2_ty = mkTyVarTy a2_tv
       ; co_a <- unifyType arg2_ty a2_ty     -- arg2 ~ a2

       ; op_id  <- tcLookupId op_name
       ; let op' = L loc (HsWrap (mkWpTyApps [a2_ty, res_ty]) (HsVar op_id))
       ; return $
         OpApp (mkLHsWrapCo (mkTcFunCo Nominal co_a co_b) $
                mkLHsWrapCo co_arg1 arg1')
               op' fix
               (mkLHsWrapCo co_a arg2') }
-}

tcExpr (SectionL arg1 op) res_ty
  = do { dflags <- getDynFlags      -- Note [Left sections]
       ; let n_reqd_args | xopt Opt_PostfixOperators dflags = 0
                         | otherwise                        = 1
       ; (co_fun, args_tys, rest_ty) <-
           matchExpectedFunTys (mk_app_msg op) n_reqd_args res_ty
       ; arg1_ty <- newFlexiTyVarTy openTypeKind
       ; let op_ty = mkFunTys (arg1_ty:args_tys) rest_ty
          -- typecheck op and arg1
       ; op'   <- tcPolyMonoExprNC op op_ty
       ; arg1' <- tcArg op' (arg1, arg1_ty, 1)
       ; return $ SectionL arg1' (mkLHsWrapCo co_fun op') }

-- Right sections, equivalent to \ x -> x `op` expr, or
--      \ x -> op x expr
tcExpr (SectionR op arg2) res_ty
  = do { -- res_ty = arg1_ty -> rest_ty
         (co_fun, [arg1_ty], rest_ty) <-
           matchExpectedFunTys (mk_app_msg op) 1 res_ty
       ; arg2_ty <- newFlexiTyVarTy openTypeKind
         -- op_ty = arg1_ty -> arg2_ty -> rest_ty
       ; let op_ty = mkFunTys [arg1_ty, arg2_ty] rest_ty
         -- typecheck op and arg2
       ; op'   <- tcPolyMonoExprNC op op_ty
       ; arg2' <- tcArg op' (arg2, arg2_ty, 2)
       ; return $ SectionR (mkLHsWrapCo co_fun op') arg2' }

tcExpr (ExplicitTuple tup_args boxity) res_ty
  | all tupArgPresent tup_args
  = do { let tup_tc = tupleTyCon boxity (length tup_args)
       ; (coi, arg_tys) <- matchExpectedTyConApp tup_tc res_ty
       ; tup_args1 <- tcTupArgs tup_args arg_tys
       ; return $ mkHsWrapCo coi (ExplicitTuple tup_args1 boxity) }

  | otherwise
  = -- The tup_args are a mixture of Present and Missing (for tuple sections)
    do { let kind = case boxity of { Boxed   -> liftedTypeKind
                                   ; Unboxed -> openTypeKind }
             arity = length tup_args
             tup_tc = tupleTyCon boxity arity

       ; arg_tys <- newFlexiTyVarTys (tyConArity tup_tc) kind
       ; let actual_res_ty
               = mkFunTys [ty | (ty, L _ (Missing _)) <- arg_tys `zip` tup_args]
                          (mkTyConApp tup_tc arg_tys)

       ; coi <- unifyType actual_res_ty res_ty

       -- Handle tuple sections where
       ; tup_args1 <- tcTupArgs tup_args arg_tys

       ; return $ mkHsWrapCo coi (ExplicitTuple tup_args1 boxity) }

tcExpr (ExplicitList _ witness exprs) res_ty
  = case witness of
      Nothing   -> do  { (coi, elt_ty) <- matchExpectedListTy res_ty
                       ; exprs' <- mapM (tc_elt elt_ty) exprs
                       ; return $ mkHsWrapCo coi (ExplicitList elt_ty Nothing exprs') }

      Just fln -> do  { list_ty <- newFlexiTyVarTy liftedTypeKind
                     ; fln' <- tcSyntaxOp ListOrigin fln (mkFunTys [intTy, list_ty] res_ty)
                     ; (coi, elt_ty) <- matchExpectedListTy list_ty
                     ; exprs' <- mapM (tc_elt elt_ty) exprs
                     ; return $ mkHsWrapCo coi (ExplicitList elt_ty (Just fln') exprs') }
     where tc_elt elt_ty expr = tcPolyMonoExpr expr elt_ty

tcExpr (ExplicitPArr _ exprs) res_ty    -- maybe empty
  = do  { (coi, elt_ty) <- matchExpectedPArrTy res_ty
        ; exprs' <- mapM (tc_elt elt_ty) exprs
        ; return $ mkHsWrapCo coi (ExplicitPArr elt_ty exprs') }
  where
    tc_elt elt_ty expr = tcPolyMonoExpr expr elt_ty

{-
************************************************************************
*                                                                      *
                Let, case, if, do
*                                                                      *
************************************************************************
-}

tcExpr (HsLet binds expr) res_ty
  = do  { (binds', expr') <- tcLocalBinds binds $
                             tcPolyMonoExpr expr res_ty
        ; return (HsLet binds' expr') }

tcExpr (HsCase scrut matches) exp_ty
  = do  {  -- We used to typecheck the case alternatives first.
           -- The case patterns tend to give good type info to use
           -- when typechecking the scrutinee.  For example
           --   case (map f) of
           --     (x:xs) -> ...
           -- will report that map is applied to too few arguments
           --
           -- But now, in the GADT world, we need to typecheck the scrutinee
           -- first, to get type info that may be refined in the case alternatives
          (scrut', scrut_ty) <- tcInferRho scrut

        ; traceTc "HsCase" (ppr scrut_ty)
        ; matches' <- tcMatchesCase match_ctxt scrut_ty matches exp_ty
        ; return (HsCase scrut' matches') }
 where
    match_ctxt = MC { mc_what = CaseAlt,
                      mc_body = tcBody }

tcExpr (HsIf Nothing pred b1 b2) res_ty    -- Ordinary 'if'
  = do { pred' <- tcPolyMonoExpr pred boolTy
       ; b1' <- tcPolyMonoExpr b1 res_ty
       ; b2' <- tcPolyMonoExpr b2 res_ty
       ; return (HsIf Nothing pred' b1' b2') }

tcExpr (HsIf (Just fun) pred b1 b2) res_ty   -- Note [Rebindable syntax for if]
  = do { pred_ty <- newFlexiTyVarTy openTypeKind
       ; b1_ty   <- newFlexiTyVarTy openTypeKind
       ; b2_ty   <- newFlexiTyVarTy openTypeKind
       ; let if_ty = mkFunTys [pred_ty, b1_ty, b2_ty] res_ty
       ; fun'  <- tcSyntaxOp IfOrigin fun if_ty
       ; pred' <- tcPolyMonoExpr pred pred_ty
       ; b1'   <- tcPolyMonoExpr b1 b1_ty
       ; b2'   <- tcPolyMonoExpr b2 b2_ty
       -- Fundamentally we are just typing (ifThenElse e1 e2 e3)
       -- so maybe we should use the code for function applications
       -- (which would allow ifThenElse to be higher rank).
       -- But it's a little awkward, so I'm leaving it alone for now
       -- and it maintains uniformity with other rebindable syntax
       ; return (HsIf (Just fun') pred' b1' b2') }

tcExpr (HsMultiIf _ alts) res_ty
  = do { alts' <- mapM (wrapLocM $ tcGRHS match_ctxt res_ty) alts
       ; return $ HsMultiIf res_ty alts' }
  where match_ctxt = MC { mc_what = IfAlt, mc_body = tcBody }

tcExpr (HsDo do_or_lc stmts _) res_ty
  = tcDoStmts do_or_lc stmts res_ty

tcExpr (HsProc pat cmd) res_ty
  = do  { (pat', cmd', coi) <- tcProc pat cmd res_ty
        ; return $ mkHsWrapCo coi (HsProc pat' cmd') }

tcExpr (HsStatic expr) res_ty
  = do  { staticPtrTyCon  <- tcLookupTyCon staticPtrTyConName
        ; (co, [expr_ty]) <- matchExpectedTyConApp staticPtrTyCon res_ty
        ; (expr', lie)    <- captureConstraints $
            addErrCtxt (hang (ptext (sLit "In the body of a static form:"))
                             2 (ppr expr)
                       ) $
            tcPolyMonoExprNC expr expr_ty
        -- Require the type of the argument to be Typeable.
        -- The evidence is not used, but asking the constraint ensures that
        -- the current implementation is as restrictive as future versions
        -- of the StaticPointers extension.
        ; typeableClass <- tcLookupClass typeableClassName
        ; _ <- emitWanted StaticOrigin $
                  mkTyConApp (classTyCon typeableClass)
                             [liftedTypeKind, expr_ty]
        -- Insert the static form in a global list for later validation.
        ; stWC <- tcg_static_wc <$> getGblEnv
        ; updTcRef stWC (andWC lie)
        ; return $ mkHsWrapCo co $ HsStatic expr'
        }

{-
Note [Rebindable syntax for if]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The rebindable syntax for 'if' uses the most flexible possible type
for conditionals:
  ifThenElse :: p -> b1 -> b2 -> res
to support expressions like this:

 ifThenElse :: Maybe a -> (a -> b) -> b -> b
 ifThenElse (Just a) f _ = f a
 ifThenElse Nothing  _ e = e

 example :: String
 example = if Just 2
              then \v -> show v
              else "No value"


************************************************************************
*                                                                      *
                Record construction and update
*                                                                      *
************************************************************************
-}

tcExpr (RecordCon (L loc con_name) _ rbinds) res_ty
  = do  { data_con <- tcLookupDataCon con_name

        -- Check for missing fields
        ; checkMissingFields data_con rbinds

        ; con_expr <- tcCheckId con_name res_ty
        ; let arity = dataConSourceArity data_con
              (arg_tys, _actual_res_ty) = tcSplitFunTysN res_ty arity
              con_id = dataConWrapId data_con

        -- ; co_res <- unifyType actual_res_ty res_ty
        ; rbinds' <- tcRecordBinds data_con arg_tys rbinds
        ; return $ RecordCon (L loc con_id) con_expr rbinds' }

{-
Note [Type of a record update]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The main complication with RecordUpd is that we need to explicitly
handle the *non-updated* fields.  Consider:

        data T a b c = MkT1 { fa :: a, fb :: (b,c) }
                     | MkT2 { fa :: a, fb :: (b,c), fc :: c -> c }
                     | MkT3 { fd :: a }

        upd :: T a b c -> (b',c) -> T a b' c
        upd t x = t { fb = x}

The result type should be (T a b' c)
not (T a b c),   because 'b' *is not* mentioned in a non-updated field
not (T a b' c'), because 'c' *is*     mentioned in a non-updated field
NB that it's not good enough to look at just one constructor; we must
look at them all; cf Trac #3219

After all, upd should be equivalent to:
        upd t x = case t of
                        MkT1 p q -> MkT1 p x
                        MkT2 a b -> MkT2 p b
                        MkT3 d   -> error ...

So we need to give a completely fresh type to the result record,
and then constrain it by the fields that are *not* updated ("p" above).
We call these the "fixed" type variables, and compute them in getFixedTyVars.

Note that because MkT3 doesn't contain all the fields being updated,
its RHS is simply an error, so it doesn't impose any type constraints.
Hence the use of 'relevant_cont'.

Note [Implicit type sharing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We also take into account any "implicit" non-update fields.  For example
        data T a b where { MkT { f::a } :: T a a; ... }
So the "real" type of MkT is: forall ab. (a~b) => a -> T a b

Then consider
        upd t x = t { f=x }
We infer the type
        upd :: T a b -> a -> T a b
        upd (t::T a b) (x::a)
           = case t of { MkT (co:a~b) (_:a) -> MkT co x }
We can't give it the more general type
        upd :: T a b -> c -> T c b

Note [Criteria for update]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to allow update for existentials etc, provided the updated
field isn't part of the existential. For example, this should be ok.
  data T a where { MkT { f1::a, f2::b->b } :: T a }
  f :: T a -> b -> T b
  f t b = t { f1=b }

The criterion we use is this:

  The types of the updated fields
  mention only the universally-quantified type variables
  of the data constructor

NB: this is not (quite) the same as being a "naughty" record selector
(See Note [Naughty record selectors]) in TcTyClsDecls), at least
in the case of GADTs. Consider
   data T a where { MkT :: { f :: a } :: T [a] }
Then f is not "naughty" because it has a well-typed record selector.
But we don't allow updates for 'f'.  (One could consider trying to
allow this, but it makes my head hurt.  Badly.  And no one has asked
for it.)

In principle one could go further, and allow
  g :: T a -> T a
  g t = t { f2 = \x -> x }
because the expression is polymorphic...but that seems a bridge too far.

Note [Data family example]
~~~~~~~~~~~~~~~~~~~~~~~~~~
    data instance T (a,b) = MkT { x::a, y::b }
  --->
    data :TP a b = MkT { a::a, y::b }
    coTP a b :: T (a,b) ~ :TP a b

Suppose r :: T (t1,t2), e :: t3
Then  r { x=e } :: T (t3,t1)
  --->
      case r |> co1 of
        MkT x y -> MkT e y |> co2
      where co1 :: T (t1,t2) ~ :TP t1 t2
            co2 :: :TP t3 t2 ~ T (t3,t2)
The wrapping with co2 is done by the constructor wrapper for MkT

Outgoing invariants
~~~~~~~~~~~~~~~~~~~
In the outgoing (HsRecordUpd scrut binds cons in_inst_tys out_inst_tys):

  * cons are the data constructors to be updated

  * in_inst_tys, out_inst_tys have same length, and instantiate the
        *representation* tycon of the data cons.  In Note [Data
        family example], in_inst_tys = [t1,t2], out_inst_tys = [t3,t2]
-}

tcExpr (RecordUpd record_expr rbinds _ _ _) res_ty
  = ASSERT( notNull upd_fld_names )
    do  {
        -- STEP 0
        -- Check that the field names are really field names
        ; sel_ids <- mapM tcLookupField upd_fld_names
                        -- The renamer has already checked that
                        -- selectors are all in scope
        ; let bad_guys = [ setSrcSpan loc $ addErrTc (notSelector fld_name)
                         | (fld, sel_id) <- rec_flds rbinds `zip` sel_ids,
                           not (isRecordSelector sel_id),       -- Excludes class ops
                           let L loc fld_name = hsRecFieldId (unLoc fld) ]
        ; unless (null bad_guys) (sequence bad_guys >> failM)

        -- STEP 1
        -- Figure out the tycon and data cons from the first field name
        ; let   -- It's OK to use the non-tc splitters here (for a selector)
              sel_id : _  = sel_ids
              (tycon, _)  = recordSelectorFieldLabel sel_id     -- We've failed already if
              data_cons   = tyConDataCons tycon                 -- it's not a field label
                -- NB: for a data type family, the tycon is the instance tycon

              relevant_cons   = filter is_relevant data_cons
              is_relevant con = all (`elem` dataConFieldLabels con) upd_fld_names
                -- A constructor is only relevant to this process if
                -- it contains *all* the fields that are being updated
                -- Other ones will cause a runtime error if they occur

                -- Take apart a representative constructor
              con1 = ASSERT( not (null relevant_cons) ) head relevant_cons
              (con1_tvs, _, _, _, con1_arg_tys, _) = dataConFullSig con1
              con1_flds = dataConFieldLabels con1
              con1_res_ty = mkFamilyTyConApp tycon (mkTyVarTys con1_tvs)

        -- Step 2
        -- Check that at least one constructor has all the named fields
        -- i.e. has an empty set of bad fields returned by badFields
        ; checkTc (not (null relevant_cons)) (badFieldsUpd rbinds data_cons)

        -- STEP 3    Note [Criteria for update]
        -- Check that each updated field is polymorphic; that is, its type
        -- mentions only the universally-quantified variables of the data con
        ; let flds1_w_tys = zipEqual "tcExpr:RecConUpd" con1_flds con1_arg_tys
              upd_flds1_w_tys = filter is_updated flds1_w_tys
              is_updated (fld,_) = fld `elem` upd_fld_names

              bad_upd_flds = filter bad_fld upd_flds1_w_tys
              con1_tv_set = mkVarSet con1_tvs
              bad_fld (fld, ty) = fld `elem` upd_fld_names &&
                                      not (tyVarsOfType ty `subVarSet` con1_tv_set)
        ; checkTc (null bad_upd_flds) (badFieldTypes bad_upd_flds)

        -- STEP 4  Note [Type of a record update]
        -- Figure out types for the scrutinee and result
        -- Both are of form (T a b c), with fresh type variables, but with
        -- common variables where the scrutinee and result must have the same type
        -- These are variables that appear in *any* arg of *any* of the
        -- relevant constructors *except* in the updated fields
        --
        ; let fixed_tvs = getFixedTyVars con1_tvs relevant_cons
              is_fixed_tv tv = tv `elemVarSet` fixed_tvs

              mk_inst_ty :: TvSubst -> (TKVar, TcType) -> TcM (TvSubst, TcType)
              -- Deals with instantiation of kind variables
              --   c.f. TcMType.tcInstTyVars
              mk_inst_ty subst (tv, result_inst_ty)
                | is_fixed_tv tv   -- Same as result type
                = return (extendTvSubst subst tv result_inst_ty, result_inst_ty)
                | otherwise        -- Fresh type, of correct kind
                = do { new_ty <- newFlexiTyVarTy (TcType.substTy subst (tyVarKind tv))
                     ; return (extendTvSubst subst tv new_ty, new_ty) }

        ; (result_subst, con1_tvs') <- tcInstTyVars con1_tvs
        ; let result_inst_tys = mkTyVarTys con1_tvs'

        ; (scrut_subst, scrut_inst_tys) <- mapAccumLM mk_inst_ty emptyTvSubst
                                                      (con1_tvs `zip` result_inst_tys)

        ; let rec_res_ty    = TcType.substTy result_subst con1_res_ty
              scrut_ty      = TcType.substTy scrut_subst  con1_res_ty
              con1_arg_tys' = map (TcType.substTy result_subst) con1_arg_tys

        ; co_res <- unifyType rec_res_ty res_ty

        -- STEP 5
        -- Typecheck the thing to be updated, and the bindings
        ; record_expr' <- tcPolyMonoExpr record_expr scrut_ty
        ; rbinds'      <- tcRecordBinds con1 con1_arg_tys' rbinds

        -- STEP 6: Deal with the stupid theta
        ; let theta' = substTheta scrut_subst (dataConStupidTheta con1)
        ; instStupidTheta RecordUpdOrigin theta'

        -- Step 7: make a cast for the scrutinee, in the case that it's from a type family
        ; let scrut_co | Just co_con <- tyConFamilyCoercion_maybe tycon
                       = mkWpCast (mkTcUnbranchedAxInstCo Representational co_con scrut_inst_tys)
                       | otherwise
                       = idHsWrapper
        -- Phew!
        ; return $ mkHsWrapCo co_res $
          RecordUpd (mkLHsWrap scrut_co record_expr') rbinds'
                    relevant_cons scrut_inst_tys result_inst_tys  }
  where
    upd_fld_names = hsRecFields rbinds

    getFixedTyVars :: [TyVar] -> [DataCon] -> TyVarSet
    -- These tyvars must not change across the updates
    getFixedTyVars tvs1 cons
      = mkVarSet [tv1 | con <- cons
                      , let (tvs, theta, arg_tys, _) = dataConSig con
                            flds = dataConFieldLabels con
                            fixed_tvs = exactTyVarsOfTypes fixed_tys
                                    -- fixed_tys: See Note [Type of a record update]
                                        `unionVarSet` tyVarsOfTypes theta
                                    -- Universally-quantified tyvars that
                                    -- appear in any of the *implicit*
                                    -- arguments to the constructor are fixed
                                    -- See Note [Implicit type sharing]

                            fixed_tys = [ty | (fld,ty) <- zip flds arg_tys
                                            , not (fld `elem` upd_fld_names)]
                      , (tv1,tv) <- tvs1 `zip` tvs      -- Discards existentials in tvs
                      , tv `elemVarSet` fixed_tvs ]

{-
************************************************************************
*                                                                      *
        Arithmetic sequences                    e.g. [a,b..]
        and their parallel-array counterparts   e.g. [: a,b.. :]

*                                                                      *
************************************************************************
-}

tcExpr (ArithSeq _ witness seq) res_ty
  = tcArithSeq witness seq res_ty

tcExpr (PArrSeq _ seq@(FromTo expr1 expr2)) res_ty
  = do  { (coi, elt_ty) <- matchExpectedPArrTy res_ty
        ; expr1' <- tcPolyMonoExpr expr1 elt_ty
        ; expr2' <- tcPolyMonoExpr expr2 elt_ty
        ; enumFromToP <- initDsTc $ dsDPHBuiltin enumFromToPVar
        ; enum_from_to <- newMethodFromName (PArrSeqOrigin seq)
                                 (idName enumFromToP) elt_ty
        ; return $ mkHsWrapCo coi
                     (PArrSeq enum_from_to (FromTo expr1' expr2')) }

tcExpr (PArrSeq _ seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = do  { (coi, elt_ty) <- matchExpectedPArrTy res_ty
        ; expr1' <- tcPolyMonoExpr expr1 elt_ty
        ; expr2' <- tcPolyMonoExpr expr2 elt_ty
        ; expr3' <- tcPolyMonoExpr expr3 elt_ty
        ; enumFromThenToP <- initDsTc $ dsDPHBuiltin enumFromThenToPVar
        ; eft <- newMethodFromName (PArrSeqOrigin seq)
                      (idName enumFromThenToP) elt_ty        -- !!!FIXME: chak
        ; return $ mkHsWrapCo coi
                     (PArrSeq eft (FromThenTo expr1' expr2' expr3')) }

tcExpr (PArrSeq _ _) _
  = panic "TcExpr.tcExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer shouldn't have
    -- let it through

{-
************************************************************************
*                                                                      *
                Template Haskell
*                                                                      *
************************************************************************
-}

tcExpr (HsSpliceE splice)        res_ty = tcSpliceExpr splice res_ty
tcExpr (HsBracket brack)         res_ty = tcTypedBracket   brack res_ty
tcExpr (HsRnBracketOut brack ps) res_ty = tcUntypedBracket brack ps res_ty

{-
************************************************************************
*                                                                      *
                Catch-all
*                                                                      *
************************************************************************
-}

tcExpr other _ = pprPanic "tcMonoExpr" (ppr other)
  -- Include ArrForm, ArrApp, which shouldn't appear at all
  -- Also HsTcBracketOut, HsQuasiQuoteE

{-
************************************************************************
*                                                                      *
                Arithmetic sequences [a..b] etc
*                                                                      *
************************************************************************
-}

tcArithSeq :: Maybe (SyntaxExpr Name) -> ArithSeqInfo Name -> TcRhoType
           -> TcM (HsExpr TcId)

tcArithSeq witness seq@(From expr) res_ty
  = do { (coi, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr' <- tcPolyMonoExpr expr elt_ty
       ; enum_from <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromName elt_ty
       ; return $ mkHsWrapCo coi (ArithSeq enum_from wit' (From expr')) }

tcArithSeq witness seq@(FromThen expr1 expr2) res_ty
  = do { (coi, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr1' <- tcPolyMonoExpr expr1 elt_ty
       ; expr2' <- tcPolyMonoExpr expr2 elt_ty
       ; enum_from_then <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromThenName elt_ty
       ; return $ mkHsWrapCo coi (ArithSeq enum_from_then wit' (FromThen expr1' expr2')) }

tcArithSeq witness seq@(FromTo expr1 expr2) res_ty
  = do { (coi, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr1' <- tcPolyMonoExpr expr1 elt_ty
       ; expr2' <- tcPolyMonoExpr expr2 elt_ty
       ; enum_from_to <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromToName elt_ty
       ; return $ mkHsWrapCo coi (ArithSeq enum_from_to wit' (FromTo expr1' expr2')) }

tcArithSeq witness seq@(FromThenTo expr1 expr2 expr3) res_ty
  = do { (coi, elt_ty, wit') <- arithSeqEltType witness res_ty
        ; expr1' <- tcPolyMonoExpr expr1 elt_ty
        ; expr2' <- tcPolyMonoExpr expr2 elt_ty
        ; expr3' <- tcPolyMonoExpr expr3 elt_ty
        ; eft <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromThenToName elt_ty
        ; return $ mkHsWrapCo coi (ArithSeq eft wit' (FromThenTo expr1' expr2' expr3')) }

-----------------
arithSeqEltType :: Maybe (SyntaxExpr Name) -> TcRhoType
              -> TcM (TcCoercion, TcType, Maybe (SyntaxExpr Id))
arithSeqEltType Nothing res_ty
  = do { (coi, elt_ty) <- matchExpectedListTy res_ty
       ; return (coi, elt_ty, Nothing) }
arithSeqEltType (Just fl) res_ty
  = do { list_ty <- newFlexiTyVarTy liftedTypeKind
       ; fl' <- tcSyntaxOp ListOrigin fl (mkFunTy list_ty res_ty)
       ; (coi, elt_ty) <- matchExpectedListTy list_ty
       ; return (coi, elt_ty, Just fl') }

{-
************************************************************************
*                                                                      *
                Applications
*                                                                      *
************************************************************************
-}

-- Some function applications have special treatment,
--   return those directly.
-- For the rest, return the pieces of information to assembly.
data TcAppResult
  = TcAppResult  (LHsExpr TcId)  -- Function
                 [LHsExpr TcId]  -- Args
                 HsWrapper       -- Wrap this around (fn args)

stepTcAppResult :: TcAppResult
                -> (LHsExpr TcId -> [LHsExpr TcId] -> HsWrapper -> TcAppResult)
                -> TcAppResult
stepTcAppResult (TcAppResult fun args co) f = f fun args co

-------------------------
tcApp :: HsExpr Name -> TcRhoType   -- An application and its expected type
      -> TcM (HsExpr TcId)          -- Translated application

tcApp e res_ty = do { TcAppResult fn [] co <- tcAppWorker (L noSrcSpan e) [] res_ty
                    ; return $ unLoc (mkLHsWrap co fn) }

-------------------------
tcAppWorker :: LHsExpr Name -> [LHsExpr Name]   -- Function and arguments so far
            -> TcRhoType                        -- Expected type of the application
            -> TcM TcAppResult

tcAppWorker (L _ (HsPar e)) args res_ty
  = tcAppWorker e args res_ty

tcAppWorker (L _ (HsApp e1 e2)) args res_ty
  = do { -- Accumulate the arguments
         result <- tcAppWorker e1 (e2:args) res_ty
         -- Rebuild the application
       ; return $ stepTcAppResult result $ \e1' (e2':args') co' ->
                  TcAppResult (mkHsApp e1' e2') args' co' }

tcAppWorker (L loc (OpApp arg1 op fix arg2)) args res_ty
  = do { result <- tcAppWorker op (arg1:arg2:args) res_ty
       ; return $ stepTcAppResult result $ \op' (arg1':arg2':args') co' ->
                  TcAppResult (L loc (OpApp arg1' op' fix arg2')) args' co' }

-- Left section with some given arguments -> nothing special to do
tcAppWorker (L loc (SectionL arg1 op)) args res_ty
  = do { result <- tcAppWorker op (arg1:args) res_ty
       ; return $ stepTcAppResult result $ \op' (arg1':args') co' ->
                  TcAppResult (L loc (SectionL arg1' op')) args' co' }

-- Right section with some given arguments -> insert argument in second place
tcAppWorker (L loc (SectionR op arg2)) (arg1:args) res_ty
  = do { result <- tcAppWorker op (arg1:arg2:args) res_ty
       ; return $ stepTcAppResult result $ \op' (arg1':arg2':args') co' ->
                  TcAppResult (L loc (SectionR op' arg2')) (arg1':args') co' }

{-
Note [Typing rule for ($)]
~~~~~~~~~~~~~~~~~~~~~~~~~~
People write
   runST $ blah
so much, where
   runST :: (forall s. ST s a) -> a
that I have finally given in and written a special type-checking
rule just for saturated appliations of ($).

Note [Typing rule for seq]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to allow
       x `seq` (# p,q #)
which suggests this type for seq:
   seq :: forall (a:*) (b:??). a -> b -> b,
with (b:??) meaning that be can be instantiated with an unboxed tuple.
But that's ill-kinded!  Function arguments can't be unboxed tuples.
And indeed, you could not expect to do this with a partially-applied
'seq'; it's only going to work when it's fully applied.  so it turns
into
    case x of _ -> (# p,q #)

For a while I slid by by giving 'seq' an ill-kinded type, but then
the simplifier eta-reduced an application of seq and Lint blew up
with a kind error.  It seems more uniform to treat 'seq' as it it
was a language construct.

See Note [seqId magic] in MkId, and
-}

-- PRECONDITION: at this point we know 'args' is non-empty
--               so we must instantiate the function,
--               rather than using an InstanceOf constraint
tcAppWorker (L loc (HsVar fun_name)) args res_ty
  | fun_name `hasKey` tagToEnumKey
  , [arg] <- args
  = tcTagToEnum loc fun_name arg res_ty

  | fun_name `hasKey` seqIdKey
  , [arg1,arg2] <- args
  = tcSeq loc fun_name arg1 arg2 res_ty

  | fun_name `hasKey` dollarIdKey   -- Note [Typing rule for ($)]
  , ((L actual_loc (HsVar actual_fun_name)) : rest_args) <- args
  = do {   -- Typing without ($)
         (fun_expr, _, fun_sigma) <- tcIdOcc (OccurrenceOf actual_fun_name) actual_fun_name
       ; (wrap, fun_rho) <- instFunTy fun_sigma   -- Eagerly instantiate
       ; result <- tc_app (L actual_loc (mkHsWrap wrap fun_expr)) rest_args fun_rho res_ty

           -- Build the ($) application
       ; dollar <- tcCheckId fun_name (mkFunTy fun_rho fun_rho)
       ; return $ stepTcAppResult result $ \actual_fun' rest_args' co' ->
                  TcAppResult (L loc dollar) (actual_fun' : rest_args') co' }

  | otherwise  -- The common case: a variable other than '$', 'tagToEnum, or 'seq',
               --                  applied to a list of arguments
  = do { (fun_expr, _, fun_sigma) <- tcIdOcc (OccurrenceOf fun_name) fun_name
       ; (wrap, fun_rho) <- instFunTy fun_sigma   -- Eagerly instantiate
       ; tc_app (L loc (mkHsWrap wrap fun_expr)) args fun_rho res_ty }

tcAppWorker hs_fun args res_ty
    -- Normal case, where the function is not a variable
  = do  {   -- Create function type schema
        ; (fun, fun_ty) <- tcInfer (tcPolyMonoExpr hs_fun)
            -- Run with new type schema
        ; tc_app fun args fun_ty res_ty }

instFunTy :: TcType -> TcM (HsWrapper, TcType)
-- Instantate the type until there are no top-level
-- foralls or '=>' constraints
instFunTy ty = go (tcSplitSigmaTy ty)
  where go (tvs, theta, rho)
           | null tvs, null theta
           = return (idHsWrapper, ty)
           | otherwise
           = do { (subst, tvs') <- tcInstTyVars tvs
                ; let tys'   = mkTyVarTys tvs'
                      theta' = substTheta subst theta
                      rho'   = substTy subst rho
                ; wrap1 <- instCall AppOrigin tys' theta'
                ; (wrap2, tau) <- instFunTy rho'
                ; return (wrap2 <.> wrap1, tau) }

tc_app :: LHsExpr TcId    -- Function
       -> [LHsExpr Name]  -- Arguments
       -> TcSigmaType     -- Type of the function
       -> TcRhoType       -- Type of the result
       -> TcM TcAppResult
tc_app fun_expr args fun_ty res_ty
  = do  { traceTc "tc_app/1" (vcat [ppr fun_expr, ppr fun_ty, ppr args, ppr res_ty])

            -- Extract its argument types
        ; (co_fun, expected_arg_tys, actual_res_ty)
             <- matchExpectedFunTys (mk_app_msg fun_expr) (length args) fun_ty

        ; traceTc "tc_app/2" (vcat [ppr expected_arg_tys, ppr actual_res_ty])

        -- Typecheck the arguments
        ; args1 <- tcArgs fun_expr args expected_arg_tys

        -- Both actual_res_ty and res_ty are deeply skolemised
        -- Split in cases depending on whether res_ty is a variable or not
        -- When it is, generate a equality constraint instead of instantiation
        -- This is needed to compile some programs such as
        -- > data S a = S a
        -- > f :: [Char] -> S a
        -- > f x = S (error x)
        -- Without it, the `a` coming from `f` cannot be unified with
        -- the second type variable of `error`
        ; case getTyVar_maybe actual_res_ty of
          { Nothing
              -> do { ev_res <- addErrCtxtM (funResCtxt True (unLoc fun_expr) actual_res_ty res_ty) $
                                emitWanted AppOrigin (mkInstanceOfPred actual_res_ty res_ty)
                    ; return $ TcAppResult
                       (mkLHsWrapCo co_fun fun_expr)  -- Instantiated function
                       args1                          -- Arguments
                                                      -- Coercion to expected result type
                       (mkWpInstanceOf ev_res) }
          ; Just _
              -> do { co_res <- addErrCtxtM (funResCtxt True (unLoc fun_expr) actual_res_ty res_ty) $
                                unifyType actual_res_ty res_ty
                    ; return $ TcAppResult
                       (mkLHsWrapCo co_fun fun_expr)  -- Instantiated function
                       args1                          -- Arguments
                       (coToHsWrapper co_res) } } }   -- Coercion to expected result type

mk_app_msg :: Outputable a => a -> SDoc
mk_app_msg fun = sep [ ptext (sLit "The function") <+> quotes (ppr fun)
                     , ptext (sLit "is applied to")]

{-
tcInferFun :: LHsExpr Name -> TcM (LHsExpr TcId, TcRhoType)
-- Infer and instantiate the type of a function
tcInferFun (L loc (HsVar name))
  = do { (fun, ty) <- setSrcSpan loc (tcInferId name)
               -- Don't wrap a context around a plain Id
       ; return (L loc fun, ty) }

tcInferFun fun
  = do { (fun, fun_ty) <- tcInfer (tcPolyMonoExpr fun)

         -- Zonk the function type carefully, to expose any polymorphism
         -- E.g. (( \(x::forall a. a->a). blah ) e)
         -- We can see the rank-2 type of the lambda in time to generalise e
       ; fun_ty' <- zonkTcType fun_ty

       ; (wrap, rho) <- deeplyInstantiate AppOrigin fun_ty'
       ; return (mkLHsWrap wrap fun, rho) }
-}

----------------
tcArgs :: LHsExpr TcId                          -- The function (for error messages)
       -> [LHsExpr Name] -> [TcSigmaType]       -- Actual arguments and expected arg types
       -> TcM [LHsExpr TcId]                    -- Resulting args

tcArgs fun args expected_arg_tys
  = mapM (tcArg fun) (zip3 args expected_arg_tys [1..])

----------------
tcArg :: LHsExpr TcId                           -- The function (for error messages)
       -> (LHsExpr Name, TcSigmaType, Int)      -- Actual argument and expected arg type
       -> TcM (LHsExpr TcId)                    -- Resulting argument
tcArg fun (arg, ty, arg_no) = addErrCtxt (funAppCtxt fun arg arg_no)
                                         (tcPolyMonoExprNC arg ty)

----------------
tcTupArgs :: [LHsTupArg Name] -> [TcSigmaType] -> TcM [LHsTupArg TcId]
tcTupArgs args tys
  = ASSERT( equalLength args tys ) mapM go (args `zip` tys)
  where
    go (L l (Missing {}),   arg_ty) = return (L l (Missing arg_ty))
    go (L l (Present expr), arg_ty) = do { expr' <- tcPolyMonoExpr expr arg_ty
                                         ; return (L l (Present expr')) }

{----------------
unifyOpFunTysWrap :: LHsExpr Name -> Arity -> TcRhoType
                  -> TcM (TcCoercion, [TcSigmaType], TcRhoType)
-- A wrapper for matchExpectedFunTys
unifyOpFunTysWrap op arity ty = matchExpectedFunTys herald arity ty
  where
    herald = ptext (sLit "The operator") <+> quotes (ppr op) <+> ptext (sLit "takes")
-}

---------------------------
tcSyntaxOp :: CtOrigin -> HsExpr Name -> TcType -> TcM (HsExpr TcId)
-- Typecheck a syntax operator, checking that it has the specified type
-- The operator is always a variable at this stage (i.e. renamer output)
-- This version assumes res_ty is a monotype
tcSyntaxOp orig (HsVar op) res_ty = tcCheckIdWithOrig orig op res_ty
tcSyntaxOp _ other         _      = pprPanic "tcSyntaxOp" (ppr other)

{-
Note [Push result type in]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Unify with expected result before type-checking the args so that the
info from res_ty percolates to args.  This is when we might detect a
too-few args situation.  (One can think of cases when the opposite
order would give a better error message.)
experimenting with putting this first.

Here's an example where it actually makes a real difference

   class C t a b | t a -> b
   instance C Char a Bool

   data P t a = forall b. (C t a b) => MkP b
   data Q t   = MkQ (forall a. P t a)

   f1, f2 :: Q Char;
   f1 = MkQ (MkP True)
   f2 = MkQ (MkP True :: forall a. P Char a)

With the change, f1 will type-check, because the 'Char' info from
the signature is propagated into MkQ's argument. With the check
in the other order, the extra signature in f2 is reqd.


************************************************************************
*                                                                      *
                 tcCheckId
*                                                                      *
************************************************************************
-}

tcCheckId :: Name -> TcRhoType -> TcM (HsExpr TcId)
tcCheckId n = tcCheckIdWithOrig (OccurrenceOf n) n

tcCheckIdWithOrig :: CtOrigin -> Name -> TcRhoType -> TcM (HsExpr TcId)
tcCheckIdWithOrig orig id_name res_ty
  | id_name `hasKey` tagToEnumKey
  = failWithTc (ptext (sLit "tagToEnum# must appear applied to one argument"))
        -- tcApp catches the case (tagToEnum# arg)

  | id_name `hasKey` assertIdKey
  = do { dflags <- getDynFlags
       ; if gopt Opt_IgnoreAsserts dflags
         then tc_check_id orig id_name res_ty
         else do { (expr, actual_res_ty) <- tc_infer_assert dflags orig
                 ; traceTc "tcCheckIdWithOrig/assert" (vcat [ppr id_name, ppr actual_res_ty, ppr res_ty])
                 ; addErrCtxtM (funResCtxt False expr actual_res_ty res_ty) $
                   tcWrapResult expr actual_res_ty res_ty } }

  | otherwise
  = tc_check_id orig id_name res_ty

tc_infer_assert :: DynFlags -> CtOrigin -> TcM (HsExpr TcId, TcRhoType)
-- Deal with an occurrence of 'assert'
-- See Note [Adding the implicit parameter to 'assert']
tc_infer_assert dflags orig
  = do { sloc <- getSrcSpanM
       ; assert_error_id <- tcLookupId assertErrorName
       ; (wrap, id_rho) <- deeplyInstantiate orig (idType assert_error_id)
       ; let (arg_ty, res_ty) = case tcSplitFunTy_maybe id_rho of
                                   Nothing      -> pprPanic "assert type" (ppr id_rho)
                                   Just arg_res -> arg_res
       ; ASSERT( arg_ty `tcEqType` addrPrimTy )
         return (HsApp (L sloc (mkHsWrap wrap (HsVar assert_error_id)))
                       (L sloc (srcSpanPrimLit dflags sloc))
                , res_ty) }

tc_check_id :: CtOrigin -> Name -> TcRhoType -> TcM (HsExpr TcId)
-- Return type is deeply instantiated
tc_check_id orig id_name res_ty
 = do { (id_expr, flavour, id_ty) <- tcIdOcc orig id_name
      ; case flavour of
          TcIdMonomorphic  -- Generate an equality constraint
            -> do { co <- unifyType id_ty res_ty
                  ; return (mkHsWrapCo co id_expr) }

          TcIdUnrestricted  -- Generate an instance-of constraint
                 -> do { ev <- emitWanted orig (mkInstanceOfPred id_ty res_ty)
                       ; return (mkHsWrap (mkWpInstanceOf ev) id_expr) } }

tcIdOcc :: CtOrigin -> Name -> TcM (HsExpr TcId, TcIdFlavor, TcSigmaType)
-- Check an occurrence of an Id in a term
-- Do not instantiate it, except in the legacy case
-- of data constructors with a stupid theta
tcIdOcc orig name
  = do { thing <- tcLookup name
       ; case thing of
             ATcId { tct_id = id, tct_flavor = flavor }
               -> do { check_naughty id        -- Note [Local record selectors]
                     ; checkThLocalId id
                     ; return (HsVar id, flavor, idType id) }

             AGlobal (AnId id)
               -> do { check_naughty id
                     ; return (HsVar id, TcIdUnrestricted, idType id) }
                    -- A global cannot possibly be ill-staged
                    -- nor does it need the 'lifting' treatment
                    -- hence no checkTh stuff here

             AGlobal (AConLike (PatSynCon ps))
               -> do { (expr, res_ty) <- tcPatSynBuilderOcc orig ps
                     ; return (expr, TcIdMonomorphic, res_ty) }
                     -- ToDo: instantiate pattern synonyms lazily

             AGlobal (AConLike (RealDataCon con))
               | null (dataConStupidTheta con)
               -> return (HsVar (dataConWrapId con), TcIdUnrestricted, idType (dataConWrapId con))
               | otherwise  -- Legacy case: always instantiate eagerly
               -> inst_stupid_data_con con

             _ -> failWithTc $
                  ppr thing <+> ptext (sLit "used where a value identifier was expected") }
  where
    check_naughty id
      | isNaughtyRecordSelector id = failWithTc (naughtyRecordSel id)
      | otherwise                  = return ()

    inst_stupid_data_con con
      -- A data constructor with a "stupid theta".  Always instantiate.
      --   * Must perform the stupid-theta check
      --   * No need to deeply instantiate because type has all foralls at top
      = do { let (tvs, theta, rho) = tcSplitSigmaTy (idType (dataConWrapId con))
           ; (subst, tvs') <- tcInstTyVars tvs
           ; let tys'   = mkTyVarTys tvs'
                 theta' = substTheta subst theta
                 rho'   = substTy subst rho
           ; wrap <- instCall orig tys' theta'
           ; addDataConStupidTheta con tys'
           ; return (mkHsWrap wrap (HsVar (dataConWrapId con)), TcIdMonomorphic, rho') }


srcSpanPrimLit :: DynFlags -> SrcSpan -> HsExpr TcId
srcSpanPrimLit dflags span
    = HsLit (HsStringPrim "" (unsafeMkByteString
                             (showSDocOneLine dflags (ppr span))))

{-
Note [Adding the implicit parameter to 'assert']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker transforms (assert e1 e2) to (assertError "Foo.hs:27"
e1 e2).  This isn't really the Right Thing because there's no way to
"undo" if you want to see the original source code in the typechecker
output.  We'll have fix this in due course, when we care more about
being able to reconstruct the exact original program.

Note [tagToEnum#]
~~~~~~~~~~~~~~~~~
Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  Unification may refine the type later, but this
check won't see that, alas.  It's crude, because it relies on our
knowing *now* that the type is ok, which in turn relies on the
eager-unification part of the type checker pushing enough information
here.  In theory the Right Thing to do is to have a new form of
constraint but I definitely cannot face that!  And it works ok as-is.

Here's are two cases that should fail
        f :: forall a. a
        f = tagToEnum# 0        -- Can't do tagToEnum# at a type variable

        g :: Int
        g = tagToEnum# 0        -- Int is not an enumeration

When data type families are involved it's a bit more complicated.
     data family F a
     data instance F [Int] = A | B | C
Then we want to generate something like
     tagToEnum# R:FListInt 3# |> co :: R:FListInt ~ F [Int]
Usually that coercion is hidden inside the wrappers for
constructors of F [Int] but here we have to do it explicitly.

It's all grotesquely complicated.
-}

tcSeq :: SrcSpan -> Name -> LHsExpr Name -> LHsExpr Name
      -> TcRhoType -> TcM TcAppResult
-- (seq e1 e2) :: res_ty
-- We need a special typing rule because res_ty can be unboxed
tcSeq loc fun_name arg1 arg2 res_ty
  = do  { fun <- tcLookupId fun_name
        ; (arg1', arg1_ty) <- tcInfer (tcPolyMonoExpr arg1)
        ; arg2' <- tcPolyMonoExpr arg2 res_ty
        ; let fun'    = L loc (HsWrap ty_args (HsVar fun))
              ty_args = WpTyApp res_ty <.> WpTyApp arg1_ty
        ; return (TcAppResult fun' [arg1', arg2'] idHsWrapper) }

tcTagToEnum :: SrcSpan -> Name -> LHsExpr Name -> TcRhoType -> TcM TcAppResult
-- tagToEnum# :: forall a. Int# -> a
-- See Note [tagToEnum#]   Urgh!
tcTagToEnum loc fun_name arg res_ty
  = do  { fun <- tcLookupId fun_name
        ; ty' <- zonkTcType res_ty

        -- Check that the type is algebraic
        ; let mb_tc_app = tcSplitTyConApp_maybe ty'
              Just (tc, tc_args) = mb_tc_app
        ; checkTc (isJust mb_tc_app)
                  (mk_error ty' doc1)

        -- Look through any type family
        ; fam_envs <- tcGetFamInstEnvs
        ; let (rep_tc, rep_args, coi) = tcLookupDataFamInst fam_envs tc tc_args
             -- coi :: tc tc_args ~R rep_tc rep_args

        ; checkTc (isEnumerationTyCon rep_tc)
                  (mk_error ty' doc2)

        ; arg' <- tcPolyMonoExpr arg intPrimTy
        ; let fun' = L loc (HsWrap (WpTyApp rep_ty) (HsVar fun))
              rep_ty = mkTyConApp rep_tc rep_args
              wrapper = coToHsWrapperR (mkTcSymCo $ TcCoercion coi)

        ; return (TcAppResult fun' [arg'] wrapper) }
                  -- coi is a Representational coercion
  where
    doc1 = vcat [ ptext (sLit "Specify the type by giving a type signature")
                , ptext (sLit "e.g. (tagToEnum# x) :: Bool") ]
    doc2 = ptext (sLit "Result type must be an enumeration type")

    mk_error :: TcType -> SDoc -> SDoc
    mk_error ty what
      = hang (ptext (sLit "Bad call to tagToEnum#")
               <+> ptext (sLit "at type") <+> ppr ty)
           2 what

{-
************************************************************************
*                                                                      *
                 Template Haskell checks
*                                                                      *
************************************************************************
-}

checkThLocalId :: Id -> TcM ()
checkThLocalId id
  = do  { mb_local_use <- getStageAndBindLevel (idName id)
        ; case mb_local_use of
             Just (top_lvl, bind_lvl, use_stage)
                | thLevel use_stage > bind_lvl
                , isNotTopLevel top_lvl
                -> checkCrossStageLifting id use_stage
             _  -> return ()   -- Not a locally-bound thing, or
                               -- no cross-stage link
    }

--------------------------------------
checkCrossStageLifting :: Id -> ThStage -> TcM ()
-- If we are inside typed brackets, and (use_lvl > bind_lvl)
-- we must check whether there's a cross-stage lift to do
-- Examples   \x -> [|| x ||]
--            [|| map ||]
-- There is no error-checking to do, because the renamer did that
--
-- This is similar to checkCrossStageLifting in RnSplice, but
-- this code is applied to *typed* brackets.

checkCrossStageLifting id (Brack _ (TcPending ps_var lie_var))
  =     -- Nested identifiers, such as 'x' in
        -- E.g. \x -> [|| h x ||]
        -- We must behave as if the reference to x was
        --      h $(lift x)
        -- We use 'x' itself as the splice proxy, used by
        -- the desugarer to stitch it all back together.
        -- If 'x' occurs many times we may get many identical
        -- bindings of the same splice proxy, but that doesn't
        -- matter, although it's a mite untidy.
    do  { let id_ty = idType id
        ; checkTc (isTauTy id_ty) (polySpliceErr id)
               -- If x is polymorphic, its occurrence sites might
               -- have different instantiations, so we can't use plain
               -- 'x' as the splice proxy name.  I don't know how to
               -- solve this, and it's probably unimportant, so I'm
               -- just going to flag an error for now

        ; lift <- if isStringTy id_ty then
                     do { sid <- tcLookupId THNames.liftStringName
                                     -- See Note [Lifting strings]
                        ; return (HsVar sid) }
                  else
                     setConstraintVar lie_var   $
                          -- Put the 'lift' constraint into the right LIE
                     newMethodFromName (OccurrenceOf (idName id))
                                       THNames.liftName id_ty

                   -- Update the pending splices
        ; ps <- readMutVar ps_var
        ; let pending_splice = PendingTcSplice (idName id) (nlHsApp (noLoc lift) (nlHsVar id))
        ; writeMutVar ps_var (pending_splice : ps)

        ; return () }

checkCrossStageLifting _ _ = return ()

polySpliceErr :: Id -> SDoc
polySpliceErr id
  = ptext (sLit "Can't splice the polymorphic local variable") <+> quotes (ppr id)

{-
Note [Lifting strings]
~~~~~~~~~~~~~~~~~~~~~~
If we see $(... [| s |] ...) where s::String, we don't want to
generate a mass of Cons (CharL 'x') (Cons (CharL 'y') ...)) etc.
So this conditional short-circuits the lifting mechanism to generate
(liftString "xy") in that case.  I didn't want to use overlapping instances
for the Lift class in TH.Syntax, because that can lead to overlapping-instance
errors in a polymorphic situation.

If this check fails (which isn't impossible) we get another chance; see
Note [Converting strings] in Convert.hs

Local record selectors
~~~~~~~~~~~~~~~~~~~~~~
Record selectors for TyCons in this module are ordinary local bindings,
which show up as ATcIds rather than AGlobals.  So we need to check for
naughtiness in both branches.  c.f. TcTyClsBindings.mkAuxBinds.


************************************************************************
*                                                                      *
\subsection{Record bindings}
*                                                                      *
************************************************************************

Game plan for record bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Find the TyCon for the bindings, from the first field label.

2. Instantiate its tyvars and unify (T a1 .. an) with expected_ty.

For each binding field = value

3. Instantiate the field type (from the field label) using the type
   envt from step 2.

4  Type check the value using tcArg, passing the field type as
   the expected argument type.

This extends OK when the field types are universally quantified.
-}

tcRecordBinds
        :: DataCon
        -> [TcType]     -- Expected type for each field
        -> HsRecordBinds Name
        -> TcM (HsRecordBinds TcId)

tcRecordBinds data_con arg_tys (HsRecFields rbinds dd)
  = do  { mb_binds <- mapM do_bind rbinds
        ; return (HsRecFields (catMaybes mb_binds) dd) }
  where
    flds_w_tys = zipEqual "tcRecordBinds" (dataConFieldLabels data_con) arg_tys
    do_bind (L l fld@(HsRecField { hsRecFieldId = L loc field_lbl
                                 , hsRecFieldArg = rhs }))
      | Just field_ty <- assocMaybe flds_w_tys field_lbl
      = addErrCtxt (fieldCtxt field_lbl)        $
        do { rhs' <- tcPolyMonoExprNC rhs field_ty
           ; let field_id = mkUserLocal (nameOccName field_lbl)
                                        (nameUnique field_lbl)
                                        field_ty loc
                -- Yuk: the field_id has the *unique* of the selector Id
                --          (so we can find it easily)
                --      but is a LocalId with the appropriate type of the RHS
                --          (so the desugarer knows the type of local binder to make)
           ; return (Just (L l (fld { hsRecFieldId = L loc field_id
                                    , hsRecFieldArg = rhs' }))) }
      | otherwise
      = do { addErrTc (badFieldCon (RealDataCon data_con) field_lbl)
           ; return Nothing }

checkMissingFields :: DataCon -> HsRecordBinds Name -> TcM ()
checkMissingFields data_con rbinds
  | null field_labels   -- Not declared as a record;
                        -- But C{} is still valid if no strict fields
  = if any isBanged field_strs then
        -- Illegal if any arg is strict
        addErrTc (missingStrictFields data_con [])
    else
        return ()

  | otherwise = do              -- A record
    unless (null missing_s_fields)
           (addErrTc (missingStrictFields data_con missing_s_fields))

    warn <- woptM Opt_WarnMissingFields
    unless (not (warn && notNull missing_ns_fields))
           (warnTc True (missingFields data_con missing_ns_fields))

  where
    missing_s_fields
        = [ fl | (fl, str) <- field_info,
                 isBanged str,
                 not (fl `elem` field_names_used)
          ]
    missing_ns_fields
        = [ fl | (fl, str) <- field_info,
                 not (isBanged str),
                 not (fl `elem` field_names_used)
          ]

    field_names_used = hsRecFields rbinds
    field_labels     = dataConFieldLabels data_con

    field_info = zipEqual "missingFields"
                          field_labels
                          field_strs

    field_strs = dataConSrcBangs data_con

{-
************************************************************************
*                                                                      *
\subsection{Errors and contexts}
*                                                                      *
************************************************************************

Boring and alphabetical:
-}

addExprErrCtxt :: LHsExpr Name -> TcM a -> TcM a
addExprErrCtxt expr = addErrCtxt (exprCtxt expr)

exprCtxt :: LHsExpr Name -> SDoc
exprCtxt expr
  = hang (ptext (sLit "In the expression:")) 2 (ppr expr)

fieldCtxt :: Name -> SDoc
fieldCtxt field_name
  = ptext (sLit "In the") <+> quotes (ppr field_name) <+> ptext (sLit "field of a record")

funAppCtxt :: LHsExpr TcId -> LHsExpr Name -> Int -> SDoc
funAppCtxt fun arg arg_no
  = hang (hsep [ ptext (sLit "In the"), speakNth arg_no, ptext (sLit "argument of"),
                    quotes (ppr fun) <> text ", namely"])
       2 (quotes (ppr arg))

funResCtxt :: Bool  -- There is at least one argument
           -> HsExpr TcId -> TcType -> TcType
           -> TidyEnv -> TcM (TidyEnv, MsgDoc)
-- When we have a mis-match in the return type of a function
-- try to give a helpful message about too many/few arguments
--
-- Used for naked variables too; but with has_args = False
funResCtxt has_args fun fun_res_ty env_ty tidy_env
  = do { fun_res' <- zonkTcType fun_res_ty
       ; env'     <- zonkTcType env_ty
       ; let (args_fun, res_fun) = tcSplitFunTys fun_res'
             (args_env, res_env) = tcSplitFunTys env'
             n_fun = length args_fun
             n_env = length args_env
             info  | n_fun == n_env = Outputable.empty
                   | n_fun > n_env
                   , not_fun res_env = ptext (sLit "Probable cause:") <+> quotes (ppr fun)
                                       <+> ptext (sLit "is applied to too few arguments")
                   | has_args
                   , not_fun res_fun = ptext (sLit "Possible cause:") <+> quotes (ppr fun)
                                       <+> ptext (sLit "is applied to too many arguments")
                   | otherwise       = Outputable.empty  -- Never suggest that a naked variable is
                                                         -- applied to too many args!
       ; return (tidy_env, info) }
  where
    not_fun ty   -- ty is definitely not an arrow type,
                 -- and cannot conceivably become one
      = case tcSplitTyConApp_maybe ty of
          Just (tc, _) -> isAlgTyCon tc
          Nothing      -> False

badFieldTypes :: [(Name,TcType)] -> SDoc
badFieldTypes prs
  = hang (ptext (sLit "Record update for insufficiently polymorphic field")
                         <> plural prs <> colon)
       2 (vcat [ ppr f <+> dcolon <+> ppr ty | (f,ty) <- prs ])

badFieldsUpd
  :: HsRecFields Name a -- Field names that don't belong to a single datacon
  -> [DataCon] -- Data cons of the type which the first field name belongs to
  -> SDoc
badFieldsUpd rbinds data_cons
  = hang (ptext (sLit "No constructor has all these fields:"))
       2 (pprQuotedList conflictingFields)
          -- See Note [Finding the conflicting fields]
  where
    -- A (preferably small) set of fields such that no constructor contains
    -- all of them.  See Note [Finding the conflicting fields]
    conflictingFields = case nonMembers of
        -- nonMember belongs to a different type.
        (nonMember, _) : _ -> [aMember, nonMember]
        [] -> let
            -- All of rbinds belong to one type. In this case, repeatedly add
            -- a field to the set until no constructor contains the set.

            -- Each field, together with a list indicating which constructors
            -- have all the fields so far.
            growingSets :: [(Name, [Bool])]
            growingSets = scanl1 combine membership
            combine (_, setMem) (field, fldMem)
              = (field, zipWith (&&) setMem fldMem)
            in
            -- Fields that don't change the membership status of the set
            -- are redundant and can be dropped.
            map (fst . head) $ groupBy ((==) `on` snd) growingSets

    aMember = ASSERT( not (null members) ) fst (head members)
    (members, nonMembers) = partition (or . snd) membership

    -- For each field, which constructors contain the field?
    membership :: [(Name, [Bool])]
    membership = sortMembership $
        map (\fld -> (fld, map (Set.member fld) fieldLabelSets)) $
          hsRecFields rbinds

    fieldLabelSets :: [Set.Set Name]
    fieldLabelSets = map (Set.fromList . dataConFieldLabels) data_cons

    -- Sort in order of increasing number of True, so that a smaller
    -- conflicting set can be found.
    sortMembership =
      map snd .
      sortBy (compare `on` fst) .
      map (\ item@(_, membershipRow) -> (countTrue membershipRow, item))

    countTrue = length . filter id

{-
Note [Finding the conflicting fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  data A = A {a0, a1 :: Int}
         | B {b0, b1 :: Int}
and we see a record update
  x { a0 = 3, a1 = 2, b0 = 4, b1 = 5 }
Then we'd like to find the smallest subset of fields that no
constructor has all of.  Here, say, {a0,b0}, or {a0,b1}, etc.
We don't really want to report that no constructor has all of
{a0,a1,b0,b1}, because when there are hundreds of fields it's
hard to see what was really wrong.

We may need more than two fields, though; eg
  data T = A { x,y :: Int, v::Int }
          | B { y,z :: Int, v::Int }
          | C { z,x :: Int, v::Int }
with update
   r { x=e1, y=e2, z=e3 }, we

Finding the smallest subset is hard, so the code here makes
a decent stab, no more.  See Trac #7989.
-}

naughtyRecordSel :: TcId -> SDoc
naughtyRecordSel sel_id
  = ptext (sLit "Cannot use record selector") <+> quotes (ppr sel_id) <+>
    ptext (sLit "as a function due to escaped type variables") $$
    ptext (sLit "Probable fix: use pattern-matching syntax instead")

notSelector :: Name -> SDoc
notSelector field
  = hsep [quotes (ppr field), ptext (sLit "is not a record selector")]

missingStrictFields :: DataCon -> [FieldLabel] -> SDoc
missingStrictFields con fields
  = header <> rest
  where
    rest | null fields = Outputable.empty  -- Happens for non-record constructors
                                           -- with strict fields
         | otherwise   = colon <+> pprWithCommas ppr fields

    header = ptext (sLit "Constructor") <+> quotes (ppr con) <+>
             ptext (sLit "does not have the required strict field(s)")

missingFields :: DataCon -> [FieldLabel] -> SDoc
missingFields con fields
  = ptext (sLit "Fields of") <+> quotes (ppr con) <+> ptext (sLit "not initialised:")
        <+> pprWithCommas ppr fields

-- callCtxt fun args = ptext (sLit "In the call") <+> parens (ppr (foldl mkHsApp fun args))
