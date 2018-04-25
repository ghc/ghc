{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


TcMatches: Typecheck some @Matches@
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module TcMatches ( tcMatchesFun, tcGRHS, tcGRHSsPat, tcMatchesCase, tcMatchLambda,
                   TcMatchCtxt(..), TcStmtChecker, TcExprStmtChecker, TcCmdStmtChecker,
                   tcStmts, tcStmtsAndThen, tcDoStmts, tcBody,
                   tcDoStmt, tcGuardStmt
       ) where

import GhcPrelude

import {-# SOURCE #-}   TcExpr( tcSyntaxOp, tcInferSigmaNC, tcInferSigma
                              , tcCheckId, tcMonoExpr, tcMonoExprNC, tcPolyExpr )

import BasicTypes (LexicalFixity(..))
import HsSyn
import TcRnMonad
import TcEnv
import TcPat
import TcMType
import TcType
import TcBinds
import TcUnify
import Name
import TysWiredIn
import Id
import TyCon
import TysPrim
import TcEvidence
import Outputable
import Util
import SrcLoc
import DynFlags
import PrelNames (monadFailClassName)
import qualified GHC.LanguageExtensions as LangExt

-- Create chunkified tuple tybes for monad comprehensions
import MkCore

import Control.Monad
import Control.Arrow ( second )

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
\subsection{tcMatchesFun, tcMatchesCase}
*                                                                      *
************************************************************************

@tcMatchesFun@ typechecks a @[Match]@ list which occurs in a
@FunMonoBind@.  The second argument is the name of the function, which
is used in error messages.  It checks that all the equations have the
same number of arguments before using @tcMatches@ to do the work.

Note [Polymorphic expected type for tcMatchesFun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcMatchesFun may be given a *sigma* (polymorphic) type
so it must be prepared to use tcSkolemise to skolemise it.
See Note [sig_tau may be polymorphic] in TcPat.
-}

tcMatchesFun :: Located Name
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpRhoType     -- Expected type of function
             -> TcM (HsWrapper, MatchGroup GhcTcId (LHsExpr GhcTcId))
                                -- Returns type of body
tcMatchesFun fn@(L _ fun_name) matches exp_ty
  = do  {  -- Check that they all have the same no of arguments
           -- Location is in the monad, set the caller so that
           -- any inter-equation error messages get some vaguely
           -- sensible location.        Note: we have to do this odd
           -- ann-grabbing, because we don't always have annotations in
           -- hand when we call tcMatchesFun...
          traceTc "tcMatchesFun" (ppr fun_name $$ ppr exp_ty)
        ; checkArgs fun_name matches

        ; (wrap_gen, (wrap_fun, group))
            <- tcSkolemiseET (FunSigCtxt fun_name True) exp_ty $ \ exp_rho ->
                  -- Note [Polymorphic expected type for tcMatchesFun]
               do { (matches', wrap_fun)
                       <- matchExpectedFunTys herald arity exp_rho $
                          \ pat_tys rhs_ty ->
                          tcMatches match_ctxt pat_tys rhs_ty matches
                  ; return (wrap_fun, matches') }
        ; return (wrap_gen <.> wrap_fun, group) }
  where
    arity = matchGroupArity matches
    herald = text "The equation(s) for"
             <+> quotes (ppr fun_name) <+> text "have"
    what = FunRhs { mc_fun = fn, mc_fixity = Prefix, mc_strictness = strictness }
    match_ctxt = MC { mc_what = what, mc_body = tcBody }
    strictness
      | [L _ match] <- unLoc $ mg_alts matches
      , FunRhs{ mc_strictness = SrcStrict } <- m_ctxt match
      = SrcStrict
      | otherwise
      = NoSrcStrict

{-
@tcMatchesCase@ doesn't do the argument-count check because the
parser guarantees that each equation has exactly one argument.
-}

tcMatchesCase :: (Outputable (body GhcRn)) =>
                TcMatchCtxt body                        -- Case context
             -> TcSigmaType                             -- Type of scrutinee
             -> MatchGroup GhcRn (Located (body GhcRn)) -- The case alternatives
             -> ExpRhoType                    -- Type of whole case expressions
             -> TcM (MatchGroup GhcTcId (Located (body GhcTcId)))
                -- Translated alternatives
                -- wrapper goes from MatchGroup's ty to expected ty

tcMatchesCase ctxt scrut_ty matches res_ty
  = tcMatches ctxt [mkCheckExpType scrut_ty] res_ty matches

tcMatchLambda :: SDoc -- see Note [Herald for matchExpectedFunTys] in TcUnify
              -> TcMatchCtxt HsExpr
              -> MatchGroup GhcRn (LHsExpr GhcRn)
              -> ExpRhoType   -- deeply skolemised
              -> TcM (MatchGroup GhcTcId (LHsExpr GhcTcId), HsWrapper)
tcMatchLambda herald match_ctxt match res_ty
  = matchExpectedFunTys herald n_pats res_ty $ \ pat_tys rhs_ty ->
    tcMatches match_ctxt pat_tys rhs_ty match
  where
    n_pats | isEmptyMatchGroup match = 1   -- must be lambda-case
           | otherwise               = matchGroupArity match

-- @tcGRHSsPat@ typechecks @[GRHSs]@ that occur in a @PatMonoBind@.

tcGRHSsPat :: GRHSs GhcRn (LHsExpr GhcRn) -> TcRhoType
           -> TcM (GRHSs GhcTcId (LHsExpr GhcTcId))
-- Used for pattern bindings
tcGRHSsPat grhss res_ty = tcGRHSs match_ctxt grhss (mkCheckExpType res_ty)
  where
    match_ctxt = MC { mc_what = PatBindRhs,
                      mc_body = tcBody }

{-
************************************************************************
*                                                                      *
\subsection{tcMatch}
*                                                                      *
************************************************************************

Note [Case branches must never infer a non-tau type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  case ... of
    ... -> \(x :: forall a. a -> a) -> x
    ... -> \y -> y

Should that type-check? The problem is that, if we check the second branch
first, then we'll get a type (b -> b) for the branches, which won't unify
with the polytype in the first branch. If we check the first branch first,
then everything is OK. This order-dependency is terrible. So we want only
proper tau-types in branches (unless a sigma-type is pushed down).
This is what expTypeToType ensures: it replaces an Infer with a fresh
tau-type.

An even trickier case looks like

  f x True  = x undefined
  f x False = x ()

Here, we see that the arguments must also be non-Infer. Thus, we must
use expTypeToType on the output of matchExpectedFunTys, not the input.

But we make a special case for a one-branch case. This is so that

  f = \(x :: forall a. a -> a) -> x

still gets assigned a polytype.
-}

-- | When the MatchGroup has multiple RHSs, convert an Infer ExpType in the
-- expected type into TauTvs.
-- See Note [Case branches must never infer a non-tau type]
tauifyMultipleMatches :: [LMatch id body]
                      -> [ExpType] -> TcM [ExpType]
tauifyMultipleMatches group exp_tys
  | isSingletonMatchGroup group = return exp_tys
  | otherwise                   = mapM tauifyExpType exp_tys
  -- NB: In the empty-match case, this ensures we fill in the ExpType

-- | Type-check a MatchGroup.
tcMatches :: (Outputable (body GhcRn)) => TcMatchCtxt body
          -> [ExpSigmaType]      -- Expected pattern types
          -> ExpRhoType          -- Expected result-type of the Match.
          -> MatchGroup GhcRn (Located (body GhcRn))
          -> TcM (MatchGroup GhcTcId (Located (body GhcTcId)))

data TcMatchCtxt body   -- c.f. TcStmtCtxt, also in this module
  = MC { mc_what :: HsMatchContext Name,  -- What kind of thing this is
         mc_body :: Located (body GhcRn)         -- Type checker for a body of
                                                -- an alternative
                 -> ExpRhoType
                 -> TcM (Located (body GhcTcId)) }

tcMatches ctxt pat_tys rhs_ty (MG { mg_alts = L l matches
                                  , mg_origin = origin })
  = do { rhs_ty:pat_tys <- tauifyMultipleMatches matches (rhs_ty:pat_tys)
            -- See Note [Case branches must never infer a non-tau type]

       ; matches' <- mapM (tcMatch ctxt pat_tys rhs_ty) matches
       ; pat_tys  <- mapM readExpType pat_tys
       ; rhs_ty   <- readExpType rhs_ty
       ; return (MG { mg_alts = L l matches'
                    , mg_arg_tys = pat_tys
                    , mg_res_ty = rhs_ty
                    , mg_origin = origin }) }

-------------
tcMatch :: (Outputable (body GhcRn)) => TcMatchCtxt body
        -> [ExpSigmaType]        -- Expected pattern types
        -> ExpRhoType            -- Expected result-type of the Match.
        -> LMatch GhcRn (Located (body GhcRn))
        -> TcM (LMatch GhcTcId (Located (body GhcTcId)))

tcMatch ctxt pat_tys rhs_ty match
  = wrapLocM (tc_match ctxt pat_tys rhs_ty) match
  where
    tc_match ctxt pat_tys rhs_ty
             match@(Match { m_pats = pats, m_grhss = grhss })
      = add_match_ctxt match $
        do { (pats', grhss') <- tcPats (mc_what ctxt) pats pat_tys $
                                tcGRHSs ctxt grhss rhs_ty
           ; return (Match { m_ctxt = mc_what ctxt, m_pats = pats'
                           , m_grhss = grhss' }) }

        -- For (\x -> e), tcExpr has already said "In the expression \x->e"
        -- so we don't want to add "In the lambda abstraction \x->e"
    add_match_ctxt match thing_inside
        = case mc_what ctxt of
            LambdaExpr -> thing_inside
            _          -> addErrCtxt (pprMatchInCtxt match) thing_inside

-------------
tcGRHSs :: TcMatchCtxt body -> GRHSs GhcRn (Located (body GhcRn)) -> ExpRhoType
        -> TcM (GRHSs GhcTcId (Located (body GhcTcId)))

-- Notice that we pass in the full res_ty, so that we get
-- good inference from simple things like
--      f = \(x::forall a.a->a) -> <stuff>
-- We used to force it to be a monotype when there was more than one guard
-- but we don't need to do that any more

tcGRHSs ctxt (GRHSs grhss (L l binds)) res_ty
  = do  { (binds', grhss')
            <- tcLocalBinds binds $
               mapM (wrapLocM (tcGRHS ctxt res_ty)) grhss

        ; return (GRHSs grhss' (L l binds')) }

-------------
tcGRHS :: TcMatchCtxt body -> ExpRhoType -> GRHS GhcRn (Located (body GhcRn))
       -> TcM (GRHS GhcTcId (Located (body GhcTcId)))

tcGRHS ctxt res_ty (GRHS guards rhs)
  = do  { (guards', rhs')
            <- tcStmtsAndThen stmt_ctxt tcGuardStmt guards res_ty $
               mc_body ctxt rhs
        ; return (GRHS guards' rhs') }
  where
    stmt_ctxt  = PatGuard (mc_what ctxt)

{-
************************************************************************
*                                                                      *
\subsection{@tcDoStmts@ typechecks a {\em list} of do statements}
*                                                                      *
************************************************************************
-}

tcDoStmts :: HsStmtContext Name
          -> Located [LStmt GhcRn (LHsExpr GhcRn)]
          -> ExpRhoType
          -> TcM (HsExpr GhcTcId)          -- Returns a HsDo
tcDoStmts ListComp (L l stmts) res_ty
  = do  { res_ty <- expTypeToType res_ty
        ; (co, elt_ty) <- matchExpectedListTy res_ty
        ; let list_ty = mkListTy elt_ty
        ; stmts' <- tcStmts ListComp (tcLcStmt listTyCon) stmts
                            (mkCheckExpType elt_ty)
        ; return $ mkHsWrapCo co (HsDo ListComp (L l stmts') list_ty) }

tcDoStmts PArrComp (L l stmts) res_ty
  = do  { res_ty <- expTypeToType res_ty
        ; (co, elt_ty) <- matchExpectedPArrTy res_ty
        ; let parr_ty = mkPArrTy elt_ty
        ; stmts' <- tcStmts PArrComp (tcLcStmt parrTyCon) stmts
                            (mkCheckExpType elt_ty)
        ; return $ mkHsWrapCo co (HsDo PArrComp (L l stmts') parr_ty) }

tcDoStmts DoExpr (L l stmts) res_ty
  = do  { stmts' <- tcStmts DoExpr tcDoStmt stmts res_ty
        ; res_ty <- readExpType res_ty
        ; return (HsDo DoExpr (L l stmts') res_ty) }

tcDoStmts MDoExpr (L l stmts) res_ty
  = do  { stmts' <- tcStmts MDoExpr tcDoStmt stmts res_ty
        ; res_ty <- readExpType res_ty
        ; return (HsDo MDoExpr (L l stmts') res_ty) }

tcDoStmts MonadComp (L l stmts) res_ty
  = do  { stmts' <- tcStmts MonadComp tcMcStmt stmts res_ty
        ; res_ty <- readExpType res_ty
        ; return (HsDo MonadComp (L l stmts') res_ty) }

tcDoStmts ctxt _ _ = pprPanic "tcDoStmts" (pprStmtContext ctxt)

tcBody :: LHsExpr GhcRn -> ExpRhoType -> TcM (LHsExpr GhcTcId)
tcBody body res_ty
  = do  { traceTc "tcBody" (ppr res_ty)
        ; tcMonoExpr body res_ty
        }

{-
************************************************************************
*                                                                      *
\subsection{tcStmts}
*                                                                      *
************************************************************************
-}

type TcExprStmtChecker = TcStmtChecker HsExpr ExpRhoType
type TcCmdStmtChecker  = TcStmtChecker HsCmd  TcRhoType

type TcStmtChecker body rho_type
  =  forall thing. HsStmtContext Name
                -> Stmt GhcRn (Located (body GhcRn))
                -> rho_type                 -- Result type for comprehension
                -> (rho_type -> TcM thing)  -- Checker for what follows the stmt
                -> TcM (Stmt GhcTcId (Located (body GhcTcId)), thing)

tcStmts :: (Outputable (body GhcRn)) => HsStmtContext Name
        -> TcStmtChecker body rho_type   -- NB: higher-rank type
        -> [LStmt GhcRn (Located (body GhcRn))]
        -> rho_type
        -> TcM [LStmt GhcTcId (Located (body GhcTcId))]
tcStmts ctxt stmt_chk stmts res_ty
  = do { (stmts', _) <- tcStmtsAndThen ctxt stmt_chk stmts res_ty $
                        const (return ())
       ; return stmts' }

tcStmtsAndThen :: (Outputable (body GhcRn)) => HsStmtContext Name
               -> TcStmtChecker body rho_type    -- NB: higher-rank type
               -> [LStmt GhcRn (Located (body GhcRn))]
               -> rho_type
               -> (rho_type -> TcM thing)
               -> TcM ([LStmt GhcTcId (Located (body GhcTcId))], thing)

-- Note the higher-rank type.  stmt_chk is applied at different
-- types in the equations for tcStmts

tcStmtsAndThen _ _ [] res_ty thing_inside
  = do  { thing <- thing_inside res_ty
        ; return ([], thing) }

-- LetStmts are handled uniformly, regardless of context
tcStmtsAndThen ctxt stmt_chk (L loc (LetStmt (L l binds)) : stmts)
                                                             res_ty thing_inside
  = do  { (binds', (stmts',thing)) <- tcLocalBinds binds $
              tcStmtsAndThen ctxt stmt_chk stmts res_ty thing_inside
        ; return (L loc (LetStmt (L l binds')) : stmts', thing) }

-- Don't set the error context for an ApplicativeStmt.  It ought to be
-- possible to do this with a popErrCtxt in the tcStmt case for
-- ApplicativeStmt, but it did someting strange and broke a test (ado002).
tcStmtsAndThen ctxt stmt_chk (L loc stmt : stmts) res_ty thing_inside
  | ApplicativeStmt{} <- stmt
  = do  { (stmt', (stmts', thing)) <-
             stmt_chk ctxt stmt res_ty $ \ res_ty' ->
               tcStmtsAndThen ctxt stmt_chk stmts res_ty'  $
                 thing_inside
        ; return (L loc stmt' : stmts', thing) }

  -- For the vanilla case, handle the location-setting part
  | otherwise
  = do  { (stmt', (stmts', thing)) <-
                setSrcSpan loc                              $
                addErrCtxt (pprStmtInCtxt ctxt stmt)        $
                stmt_chk ctxt stmt res_ty                   $ \ res_ty' ->
                popErrCtxt                                  $
                tcStmtsAndThen ctxt stmt_chk stmts res_ty'  $
                thing_inside
        ; return (L loc stmt' : stmts', thing) }

---------------------------------------------------
--              Pattern guards
---------------------------------------------------

tcGuardStmt :: TcExprStmtChecker
tcGuardStmt _ (BodyStmt guard _ _ _) res_ty thing_inside
  = do  { guard' <- tcMonoExpr guard (mkCheckExpType boolTy)
        ; thing  <- thing_inside res_ty
        ; return (BodyStmt guard' noSyntaxExpr noSyntaxExpr boolTy, thing) }

tcGuardStmt ctxt (BindStmt pat rhs _ _ _) res_ty thing_inside
  = do  { (rhs', rhs_ty) <- tcInferSigmaNC rhs
                                   -- Stmt has a context already
        ; (pat', thing)  <- tcPat_O (StmtCtxt ctxt) (lexprCtOrigin rhs)
                                    pat (mkCheckExpType rhs_ty) $
                            thing_inside res_ty
        ; return (mkTcBindStmt pat' rhs', thing) }

tcGuardStmt _ stmt _ _
  = pprPanic "tcGuardStmt: unexpected Stmt" (ppr stmt)


---------------------------------------------------
--           List comprehensions and PArrays
--               (no rebindable syntax)
---------------------------------------------------

-- Dealt with separately, rather than by tcMcStmt, because
--   a) PArr isn't (yet) an instance of Monad, so the generality seems overkill
--   b) We have special desugaring rules for list comprehensions,
--      which avoid creating intermediate lists.  They in turn
--      assume that the bind/return operations are the regular
--      polymorphic ones, and in particular don't have any
--      coercion matching stuff in them.  It's hard to avoid the
--      potential for non-trivial coercions in tcMcStmt

tcLcStmt :: TyCon       -- The list/Parray type constructor ([] or PArray)
         -> TcExprStmtChecker

tcLcStmt _ _ (LastStmt body noret _) elt_ty thing_inside
  = do { body' <- tcMonoExprNC body elt_ty
       ; thing <- thing_inside (panic "tcLcStmt: thing_inside")
       ; return (LastStmt body' noret noSyntaxExpr, thing) }

-- A generator, pat <- rhs
tcLcStmt m_tc ctxt (BindStmt pat rhs _ _ _) elt_ty thing_inside
 = do   { pat_ty <- newFlexiTyVarTy liftedTypeKind
        ; rhs'   <- tcMonoExpr rhs (mkCheckExpType $ mkTyConApp m_tc [pat_ty])
        ; (pat', thing)  <- tcPat (StmtCtxt ctxt) pat (mkCheckExpType pat_ty) $
                            thing_inside elt_ty
        ; return (mkTcBindStmt pat' rhs', thing) }

-- A boolean guard
tcLcStmt _ _ (BodyStmt rhs _ _ _) elt_ty thing_inside
  = do  { rhs'  <- tcMonoExpr rhs (mkCheckExpType boolTy)
        ; thing <- thing_inside elt_ty
        ; return (BodyStmt rhs' noSyntaxExpr noSyntaxExpr boolTy, thing) }

-- ParStmt: See notes with tcMcStmt
tcLcStmt m_tc ctxt (ParStmt bndr_stmts_s _ _ _) elt_ty thing_inside
  = do  { (pairs', thing) <- loop bndr_stmts_s
        ; return (ParStmt pairs' noExpr noSyntaxExpr unitTy, thing) }
  where
    -- loop :: [([LStmt GhcRn], [GhcRn])]
    --      -> TcM ([([LStmt GhcTcId], [GhcTcId])], thing)
    loop [] = do { thing <- thing_inside elt_ty
                 ; return ([], thing) }         -- matching in the branches

    loop (ParStmtBlock stmts names _ : pairs)
      = do { (stmts', (ids, pairs', thing))
                <- tcStmtsAndThen ctxt (tcLcStmt m_tc) stmts elt_ty $ \ _elt_ty' ->
                   do { ids <- tcLookupLocalIds names
                      ; (pairs', thing) <- loop pairs
                      ; return (ids, pairs', thing) }
           ; return ( ParStmtBlock stmts' ids noSyntaxExpr : pairs', thing ) }

tcLcStmt m_tc ctxt (TransStmt { trS_form = form, trS_stmts = stmts
                              , trS_bndrs =  bindersMap
                              , trS_by = by, trS_using = using }) elt_ty thing_inside
  = do { let (bndr_names, n_bndr_names) = unzip bindersMap
             unused_ty = pprPanic "tcLcStmt: inner ty" (ppr bindersMap)
             -- The inner 'stmts' lack a LastStmt, so the element type
             --  passed in to tcStmtsAndThen is never looked at
       ; (stmts', (bndr_ids, by'))
            <- tcStmtsAndThen (TransStmtCtxt ctxt) (tcLcStmt m_tc) stmts unused_ty $ \_ -> do
               { by' <- traverse tcInferSigma by
               ; bndr_ids <- tcLookupLocalIds bndr_names
               ; return (bndr_ids, by') }

       ; let m_app ty = mkTyConApp m_tc [ty]

       --------------- Typecheck the 'using' function -------------
       -- using :: ((a,b,c)->t) -> m (a,b,c) -> m (a,b,c)m      (ThenForm)
       --       :: ((a,b,c)->t) -> m (a,b,c) -> m (m (a,b,c)))  (GroupForm)

         -- n_app :: Type -> Type   -- Wraps a 'ty' into '[ty]' for GroupForm
       ; let n_app = case form of
                       ThenForm -> (\ty -> ty)
                       _        -> m_app

             by_arrow :: Type -> Type     -- Wraps 'ty' to '(a->t) -> ty' if the By is present
             by_arrow = case by' of
                          Nothing       -> \ty -> ty
                          Just (_,e_ty) -> \ty -> (alphaTy `mkFunTy` e_ty) `mkFunTy` ty

             tup_ty        = mkBigCoreVarTupTy bndr_ids
             poly_arg_ty   = m_app alphaTy
             poly_res_ty   = m_app (n_app alphaTy)
             using_poly_ty = mkInvForAllTy alphaTyVar $
                             by_arrow $
                             poly_arg_ty `mkFunTy` poly_res_ty

       ; using' <- tcPolyExpr using using_poly_ty
       ; let final_using = fmap (mkHsWrap (WpTyApp tup_ty)) using'

             -- 'stmts' returns a result of type (m1_ty tuple_ty),
             -- typically something like [(Int,Bool,Int)]
             -- We don't know what tuple_ty is yet, so we use a variable
       ; let mk_n_bndr :: Name -> TcId -> TcId
             mk_n_bndr n_bndr_name bndr_id = mkLocalIdOrCoVar n_bndr_name (n_app (idType bndr_id))

             -- Ensure that every old binder of type `b` is linked up with its
             -- new binder which should have type `n b`
             -- See Note [GroupStmt binder map] in HsExpr
             n_bndr_ids  = zipWith mk_n_bndr n_bndr_names bndr_ids
             bindersMap' = bndr_ids `zip` n_bndr_ids

       -- Type check the thing in the environment with
       -- these new binders and return the result
       ; thing <- tcExtendIdEnv n_bndr_ids (thing_inside elt_ty)

       ; return (TransStmt { trS_stmts = stmts', trS_bndrs = bindersMap'
                           , trS_by = fmap fst by', trS_using = final_using
                           , trS_ret = noSyntaxExpr
                           , trS_bind = noSyntaxExpr
                           , trS_fmap = noExpr
                           , trS_bind_arg_ty = unitTy
                           , trS_form = form }, thing) }

tcLcStmt _ _ stmt _ _
  = pprPanic "tcLcStmt: unexpected Stmt" (ppr stmt)


---------------------------------------------------
--           Monad comprehensions
--        (supports rebindable syntax)
---------------------------------------------------

tcMcStmt :: TcExprStmtChecker

tcMcStmt _ (LastStmt body noret return_op) res_ty thing_inside
  = do  { (body', return_op')
            <- tcSyntaxOp MCompOrigin return_op [SynRho] res_ty $
               \ [a_ty] ->
               tcMonoExprNC body (mkCheckExpType a_ty)
        ; thing      <- thing_inside (panic "tcMcStmt: thing_inside")
        ; return (LastStmt body' noret return_op', thing) }

-- Generators for monad comprehensions ( pat <- rhs )
--
--   [ body | q <- gen ]  ->  gen :: m a
--                            q   ::   a
--

tcMcStmt ctxt (BindStmt pat rhs bind_op fail_op _) res_ty thing_inside
           -- (>>=) :: rhs_ty -> (pat_ty -> new_res_ty) -> res_ty
  = do  { ((rhs', pat', thing, new_res_ty), bind_op')
            <- tcSyntaxOp MCompOrigin bind_op
                          [SynRho, SynFun SynAny SynRho] res_ty $
               \ [rhs_ty, pat_ty, new_res_ty] ->
               do { rhs' <- tcMonoExprNC rhs (mkCheckExpType rhs_ty)
                  ; (pat', thing) <- tcPat (StmtCtxt ctxt) pat
                                           (mkCheckExpType pat_ty) $
                                     thing_inside (mkCheckExpType new_res_ty)
                  ; return (rhs', pat', thing, new_res_ty) }

        -- If (but only if) the pattern can fail, typecheck the 'fail' operator
        ; fail_op' <- tcMonadFailOp (MCompPatOrigin pat) pat' fail_op new_res_ty

        ; return (BindStmt pat' rhs' bind_op' fail_op' new_res_ty, thing) }

-- Boolean expressions.
--
--   [ body | stmts, expr ]  ->  expr :: m Bool
--
tcMcStmt _ (BodyStmt rhs then_op guard_op _) res_ty thing_inside
  = do  { -- Deal with rebindable syntax:
          --    guard_op :: test_ty -> rhs_ty
          --    then_op  :: rhs_ty -> new_res_ty -> res_ty
          -- Where test_ty is, for example, Bool
        ; ((thing, rhs', rhs_ty, guard_op'), then_op')
            <- tcSyntaxOp MCompOrigin then_op [SynRho, SynRho] res_ty $
               \ [rhs_ty, new_res_ty] ->
               do { (rhs', guard_op')
                      <- tcSyntaxOp MCompOrigin guard_op [SynAny]
                                    (mkCheckExpType rhs_ty) $
                         \ [test_ty] ->
                         tcMonoExpr rhs (mkCheckExpType test_ty)
                  ; thing <- thing_inside (mkCheckExpType new_res_ty)
                  ; return (thing, rhs', rhs_ty, guard_op') }
        ; return (BodyStmt rhs' then_op' guard_op' rhs_ty, thing) }

-- Grouping statements
--
--   [ body | stmts, then group by e using f ]
--     ->  e :: t
--         f :: forall a. (a -> t) -> m a -> m (m a)
--   [ body | stmts, then group using f ]
--     ->  f :: forall a. m a -> m (m a)

-- We type [ body | (stmts, group by e using f), ... ]
--     f <optional by> [ (a,b,c) | stmts ] >>= \(a,b,c) -> ...body....
--
-- We type the functions as follows:
--     f <optional by> :: m1 (a,b,c) -> m2 (a,b,c)              (ThenForm)
--                     :: m1 (a,b,c) -> m2 (n (a,b,c))          (GroupForm)
--     (>>=) :: m2 (a,b,c)     -> ((a,b,c)   -> res) -> res     (ThenForm)
--           :: m2 (n (a,b,c)) -> (n (a,b,c) -> res) -> res     (GroupForm)
--
tcMcStmt ctxt (TransStmt { trS_stmts = stmts, trS_bndrs = bindersMap
                         , trS_by = by, trS_using = using, trS_form = form
                         , trS_ret = return_op, trS_bind = bind_op
                         , trS_fmap = fmap_op }) res_ty thing_inside
  = do { let star_star_kind = liftedTypeKind `mkFunTy` liftedTypeKind
       ; m1_ty   <- newFlexiTyVarTy star_star_kind
       ; m2_ty   <- newFlexiTyVarTy star_star_kind
       ; tup_ty  <- newFlexiTyVarTy liftedTypeKind
       ; by_e_ty <- newFlexiTyVarTy liftedTypeKind  -- The type of the 'by' expression (if any)

         -- n_app :: Type -> Type   -- Wraps a 'ty' into '(n ty)' for GroupForm
       ; n_app <- case form of
                    ThenForm -> return (\ty -> ty)
                    _        -> do { n_ty <- newFlexiTyVarTy star_star_kind
                                   ; return (n_ty `mkAppTy`) }
       ; let by_arrow :: Type -> Type
             -- (by_arrow res) produces ((alpha->e_ty) -> res)     ('by' present)
             --                          or res                    ('by' absent)
             by_arrow = case by of
                          Nothing -> \res -> res
                          Just {} -> \res -> (alphaTy `mkFunTy` by_e_ty) `mkFunTy` res

             poly_arg_ty  = m1_ty `mkAppTy` alphaTy
             using_arg_ty = m1_ty `mkAppTy` tup_ty
             poly_res_ty  = m2_ty `mkAppTy` n_app alphaTy
             using_res_ty = m2_ty `mkAppTy` n_app tup_ty
             using_poly_ty = mkInvForAllTy alphaTyVar $
                             by_arrow $
                             poly_arg_ty `mkFunTy` poly_res_ty

             -- 'stmts' returns a result of type (m1_ty tuple_ty),
             -- typically something like [(Int,Bool,Int)]
             -- We don't know what tuple_ty is yet, so we use a variable
       ; let (bndr_names, n_bndr_names) = unzip bindersMap
       ; (stmts', (bndr_ids, by', return_op')) <-
            tcStmtsAndThen (TransStmtCtxt ctxt) tcMcStmt stmts
                           (mkCheckExpType using_arg_ty) $ \res_ty' -> do
                { by' <- case by of
                           Nothing -> return Nothing
                           Just e  -> do { e' <- tcMonoExpr e
                                                   (mkCheckExpType by_e_ty)
                                         ; return (Just e') }

                -- Find the Ids (and hence types) of all old binders
                ; bndr_ids <- tcLookupLocalIds bndr_names

                -- 'return' is only used for the binders, so we know its type.
                --   return :: (a,b,c,..) -> m (a,b,c,..)
                ; (_, return_op') <- tcSyntaxOp MCompOrigin return_op
                                       [synKnownType (mkBigCoreVarTupTy bndr_ids)]
                                       res_ty' $ \ _ -> return ()

                ; return (bndr_ids, by', return_op') }

       --------------- Typecheck the 'bind' function -------------
       -- (>>=) :: m2 (n (a,b,c)) -> ( n (a,b,c) -> new_res_ty ) -> res_ty
       ; new_res_ty <- newFlexiTyVarTy liftedTypeKind
       ; (_, bind_op')  <- tcSyntaxOp MCompOrigin bind_op
                             [ synKnownType using_res_ty
                             , synKnownType (n_app tup_ty `mkFunTy` new_res_ty) ]
                             res_ty $ \ _ -> return ()

       --------------- Typecheck the 'fmap' function -------------
       ; fmap_op' <- case form of
                       ThenForm -> return noExpr
                       _ -> fmap unLoc . tcPolyExpr (noLoc fmap_op) $
                            mkInvForAllTy alphaTyVar $
                            mkInvForAllTy betaTyVar  $
                            (alphaTy `mkFunTy` betaTy)
                            `mkFunTy` (n_app alphaTy)
                            `mkFunTy` (n_app betaTy)

       --------------- Typecheck the 'using' function -------------
       -- using :: ((a,b,c)->t) -> m1 (a,b,c) -> m2 (n (a,b,c))

       ; using' <- tcPolyExpr using using_poly_ty
       ; let final_using = fmap (mkHsWrap (WpTyApp tup_ty)) using'

       --------------- Bulding the bindersMap ----------------
       ; let mk_n_bndr :: Name -> TcId -> TcId
             mk_n_bndr n_bndr_name bndr_id = mkLocalIdOrCoVar n_bndr_name (n_app (idType bndr_id))

             -- Ensure that every old binder of type `b` is linked up with its
             -- new binder which should have type `n b`
             -- See Note [GroupStmt binder map] in HsExpr
             n_bndr_ids = zipWith mk_n_bndr n_bndr_names bndr_ids
             bindersMap' = bndr_ids `zip` n_bndr_ids

       -- Type check the thing in the environment with
       -- these new binders and return the result
       ; thing <- tcExtendIdEnv n_bndr_ids $
                  thing_inside (mkCheckExpType new_res_ty)

       ; return (TransStmt { trS_stmts = stmts', trS_bndrs = bindersMap'
                           , trS_by = by', trS_using = final_using
                           , trS_ret = return_op', trS_bind = bind_op'
                           , trS_bind_arg_ty = n_app tup_ty
                           , trS_fmap = fmap_op', trS_form = form }, thing) }

-- A parallel set of comprehensions
--      [ (g x, h x) | ... ; let g v = ...
--                   | ... ; let h v = ... ]
--
-- It's possible that g,h are overloaded, so we need to feed the LIE from the
-- (g x, h x) up through both lots of bindings (so we get the bindLocalMethods).
-- Similarly if we had an existential pattern match:
--
--      data T = forall a. Show a => C a
--
--      [ (show x, show y) | ... ; C x <- ...
--                         | ... ; C y <- ... ]
--
-- Then we need the LIE from (show x, show y) to be simplified against
-- the bindings for x and y.
--
-- It's difficult to do this in parallel, so we rely on the renamer to
-- ensure that g,h and x,y don't duplicate, and simply grow the environment.
-- So the binders of the first parallel group will be in scope in the second
-- group.  But that's fine; there's no shadowing to worry about.
--
-- Note: The `mzip` function will get typechecked via:
--
--   ParStmt [st1::t1, st2::t2, st3::t3]
--
--   mzip :: m st1
--        -> (m st2 -> m st3 -> m (st2, st3))   -- recursive call
--        -> m (st1, (st2, st3))
--
tcMcStmt ctxt (ParStmt bndr_stmts_s mzip_op bind_op _) res_ty thing_inside
  = do { let star_star_kind = liftedTypeKind `mkFunTy` liftedTypeKind
       ; m_ty   <- newFlexiTyVarTy star_star_kind

       ; let mzip_ty  = mkInvForAllTys [alphaTyVar, betaTyVar] $
                        (m_ty `mkAppTy` alphaTy)
                        `mkFunTy`
                        (m_ty `mkAppTy` betaTy)
                        `mkFunTy`
                        (m_ty `mkAppTy` mkBoxedTupleTy [alphaTy, betaTy])
       ; mzip_op' <- unLoc `fmap` tcPolyExpr (noLoc mzip_op) mzip_ty

        -- type dummies since we don't know all binder types yet
       ; id_tys_s <- (mapM . mapM) (const (newFlexiTyVarTy liftedTypeKind))
                       [ names | ParStmtBlock _ names _ <- bndr_stmts_s ]

       -- Typecheck bind:
       ; let tup_tys  = [ mkBigCoreTupTy id_tys | id_tys <- id_tys_s ]
             tuple_ty = mk_tuple_ty tup_tys

       ; (((blocks', thing), inner_res_ty), bind_op')
           <- tcSyntaxOp MCompOrigin bind_op
                         [ synKnownType (m_ty `mkAppTy` tuple_ty)
                         , SynFun (synKnownType tuple_ty) SynRho ] res_ty $
              \ [inner_res_ty] ->
              do { stuff <- loop m_ty (mkCheckExpType inner_res_ty)
                                 tup_tys bndr_stmts_s
                 ; return (stuff, inner_res_ty) }

       ; return (ParStmt blocks' mzip_op' bind_op' inner_res_ty, thing) }

  where
    mk_tuple_ty tys = foldr1 (\tn tm -> mkBoxedTupleTy [tn, tm]) tys

       -- loop :: Type                                  -- m_ty
       --      -> ExpRhoType                            -- inner_res_ty
       --      -> [TcType]                              -- tup_tys
       --      -> [ParStmtBlock Name]
       --      -> TcM ([([LStmt GhcTcId], [GhcTcId])], thing)
    loop _ inner_res_ty [] [] = do { thing <- thing_inside inner_res_ty
                                   ; return ([], thing) }
                                   -- matching in the branches

    loop m_ty inner_res_ty (tup_ty_in : tup_tys_in)
                           (ParStmtBlock stmts names return_op : pairs)
      = do { let m_tup_ty = m_ty `mkAppTy` tup_ty_in
           ; (stmts', (ids, return_op', pairs', thing))
                <- tcStmtsAndThen ctxt tcMcStmt stmts (mkCheckExpType m_tup_ty) $
                   \m_tup_ty' ->
                   do { ids <- tcLookupLocalIds names
                      ; let tup_ty = mkBigCoreVarTupTy ids
                      ; (_, return_op') <-
                          tcSyntaxOp MCompOrigin return_op
                                     [synKnownType tup_ty] m_tup_ty' $
                                     \ _ -> return ()
                      ; (pairs', thing) <- loop m_ty inner_res_ty tup_tys_in pairs
                      ; return (ids, return_op', pairs', thing) }
           ; return (ParStmtBlock stmts' ids return_op' : pairs', thing) }
    loop _ _ _ _ = panic "tcMcStmt.loop"

tcMcStmt _ stmt _ _
  = pprPanic "tcMcStmt: unexpected Stmt" (ppr stmt)


---------------------------------------------------
--           Do-notation
--        (supports rebindable syntax)
---------------------------------------------------

tcDoStmt :: TcExprStmtChecker

tcDoStmt _ (LastStmt body noret _) res_ty thing_inside
  = do { body' <- tcMonoExprNC body res_ty
       ; thing <- thing_inside (panic "tcDoStmt: thing_inside")
       ; return (LastStmt body' noret noSyntaxExpr, thing) }

tcDoStmt ctxt (BindStmt pat rhs bind_op fail_op _) res_ty thing_inside
  = do  {       -- Deal with rebindable syntax:
                --       (>>=) :: rhs_ty -> (pat_ty -> new_res_ty) -> res_ty
                -- This level of generality is needed for using do-notation
                -- in full generality; see Trac #1537

          ((rhs', pat', new_res_ty, thing), bind_op')
            <- tcSyntaxOp DoOrigin bind_op [SynRho, SynFun SynAny SynRho] res_ty $
                \ [rhs_ty, pat_ty, new_res_ty] ->
                do { rhs' <- tcMonoExprNC rhs (mkCheckExpType rhs_ty)
                   ; (pat', thing) <- tcPat (StmtCtxt ctxt) pat
                                            (mkCheckExpType pat_ty) $
                                      thing_inside (mkCheckExpType new_res_ty)
                   ; return (rhs', pat', new_res_ty, thing) }

        -- If (but only if) the pattern can fail, typecheck the 'fail' operator
        ; fail_op' <- tcMonadFailOp (DoPatOrigin pat) pat' fail_op new_res_ty

        ; return (BindStmt pat' rhs' bind_op' fail_op' new_res_ty, thing) }

tcDoStmt ctxt (ApplicativeStmt pairs mb_join _) res_ty thing_inside
  = do  { let tc_app_stmts ty = tcApplicativeStmts ctxt pairs ty $
                                thing_inside . mkCheckExpType
        ; ((pairs', body_ty, thing), mb_join') <- case mb_join of
            Nothing -> (, Nothing) <$> tc_app_stmts res_ty
            Just join_op ->
              second Just <$>
              (tcSyntaxOp DoOrigin join_op [SynRho] res_ty $
               \ [rhs_ty] -> tc_app_stmts (mkCheckExpType rhs_ty))

        ; return (ApplicativeStmt pairs' mb_join' body_ty, thing) }

tcDoStmt _ (BodyStmt rhs then_op _ _) res_ty thing_inside
  = do  {       -- Deal with rebindable syntax;
                --   (>>) :: rhs_ty -> new_res_ty -> res_ty
        ; ((rhs', rhs_ty, thing), then_op')
            <- tcSyntaxOp DoOrigin then_op [SynRho, SynRho] res_ty $
               \ [rhs_ty, new_res_ty] ->
               do { rhs' <- tcMonoExprNC rhs (mkCheckExpType rhs_ty)
                  ; thing <- thing_inside (mkCheckExpType new_res_ty)
                  ; return (rhs', rhs_ty, thing) }
        ; return (BodyStmt rhs' then_op' noSyntaxExpr rhs_ty, thing) }

tcDoStmt ctxt (RecStmt { recS_stmts = stmts, recS_later_ids = later_names
                       , recS_rec_ids = rec_names, recS_ret_fn = ret_op
                       , recS_mfix_fn = mfix_op, recS_bind_fn = bind_op })
         res_ty thing_inside
  = do  { let tup_names = rec_names ++ filterOut (`elem` rec_names) later_names
        ; tup_elt_tys <- newFlexiTyVarTys (length tup_names) liftedTypeKind
        ; let tup_ids = zipWith mkLocalId tup_names tup_elt_tys
              tup_ty  = mkBigCoreTupTy tup_elt_tys

        ; tcExtendIdEnv tup_ids $ do
        { ((stmts', (ret_op', tup_rets)), stmts_ty)
                <- tcInferInst $ \ exp_ty ->
                   tcStmtsAndThen ctxt tcDoStmt stmts exp_ty $ \ inner_res_ty ->
                   do { tup_rets <- zipWithM tcCheckId tup_names
                                      (map mkCheckExpType tup_elt_tys)
                             -- Unify the types of the "final" Ids (which may
                             -- be polymorphic) with those of "knot-tied" Ids
                      ; (_, ret_op')
                          <- tcSyntaxOp DoOrigin ret_op [synKnownType tup_ty]
                                        inner_res_ty $ \_ -> return ()
                      ; return (ret_op', tup_rets) }

        ; ((_, mfix_op'), mfix_res_ty)
            <- tcInferInst $ \ exp_ty ->
               tcSyntaxOp DoOrigin mfix_op
                          [synKnownType (mkFunTy tup_ty stmts_ty)] exp_ty $
               \ _ -> return ()

        ; ((thing, new_res_ty), bind_op')
            <- tcSyntaxOp DoOrigin bind_op
                          [ synKnownType mfix_res_ty
                          , synKnownType tup_ty `SynFun` SynRho ]
                          res_ty $
               \ [new_res_ty] ->
               do { thing <- thing_inside (mkCheckExpType new_res_ty)
                  ; return (thing, new_res_ty) }

        ; let rec_ids = takeList rec_names tup_ids
        ; later_ids <- tcLookupLocalIds later_names
        ; traceTc "tcdo" $ vcat [ppr rec_ids <+> ppr (map idType rec_ids),
                                 ppr later_ids <+> ppr (map idType later_ids)]
        ; return (RecStmt { recS_stmts = stmts', recS_later_ids = later_ids
                          , recS_rec_ids = rec_ids, recS_ret_fn = ret_op'
                          , recS_mfix_fn = mfix_op', recS_bind_fn = bind_op'
                          , recS_bind_ty = new_res_ty
                          , recS_later_rets = [], recS_rec_rets = tup_rets
                          , recS_ret_ty = stmts_ty }, thing)
        }}

tcDoStmt _ stmt _ _
  = pprPanic "tcDoStmt: unexpected Stmt" (ppr stmt)



---------------------------------------------------
-- MonadFail Proposal warnings
---------------------------------------------------

-- The idea behind issuing MonadFail warnings is that we add them whenever a
-- failable pattern is encountered. However, instead of throwing a type error
-- when the constraint cannot be satisfied, we only issue a warning in
-- TcErrors.hs.

tcMonadFailOp :: CtOrigin
              -> LPat GhcTcId
              -> SyntaxExpr GhcRn    -- The fail op
              -> TcType              -- Type of the whole do-expression
              -> TcRn (SyntaxExpr GhcTcId)  -- Typechecked fail op
-- Get a 'fail' operator expression, to use if the pattern
-- match fails. If the pattern is irrefutatable, just return
-- noSyntaxExpr; it won't be used
tcMonadFailOp orig pat fail_op res_ty
  | isIrrefutableHsPat pat
  = return noSyntaxExpr

  | otherwise
  = do { -- Issue MonadFail warnings
         rebindableSyntax <- xoptM LangExt.RebindableSyntax
       ; desugarFlag      <- xoptM LangExt.MonadFailDesugaring
       ; missingWarning   <- woptM Opt_WarnMissingMonadFailInstances
       ; if | rebindableSyntax && (desugarFlag || missingWarning)
              -> warnRebindableClash pat
            | not desugarFlag && missingWarning
              -> emitMonadFailConstraint pat res_ty
            | otherwise
              -> return ()

        -- Get the fail op itself
        ; snd <$> (tcSyntaxOp orig fail_op [synKnownType stringTy]
                             (mkCheckExpType res_ty) $ \_ -> return ()) }

emitMonadFailConstraint :: LPat GhcTcId -> TcType -> TcRn ()
emitMonadFailConstraint pat res_ty
  = do { -- We expect res_ty to be of form (monad_ty arg_ty)
         (_co, (monad_ty, _arg_ty)) <- matchExpectedAppTy res_ty

         -- Emit (MonadFail m), but ignore the evidence; it's
         -- just there to generate a warning
       ; monadFailClass <- tcLookupClass monadFailClassName
       ; _ <- emitWanted (FailablePattern pat)
                         (mkClassPred monadFailClass [monad_ty])
       ; return () }

warnRebindableClash :: LPat GhcTcId -> TcRn ()
warnRebindableClash pattern = addWarnAt
    (Reason Opt_WarnMissingMonadFailInstances)
    (getLoc pattern)
    (text "The failable pattern" <+> quotes (ppr pattern)
     $$
     nest 2 (text "is used together with -XRebindableSyntax."
             <+> text "If this is intentional,"
             $$
             text "compile with -Wno-missing-monadfail-instances."))

{-
Note [Treat rebindable syntax first]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking
        do { bar; ... } :: IO ()
we want to typecheck 'bar' in the knowledge that it should be an IO thing,
pushing info from the context into the RHS.  To do this, we check the
rebindable syntax first, and push that information into (tcMonoExprNC rhs).
Otherwise the error shows up when cheking the rebindable syntax, and
the expected/inferred stuff is back to front (see Trac #3613).

Note [typechecking ApplicativeStmt]

join ((\pat1 ... patn -> body) <$> e1 <*> ... <*> en)

fresh type variables:
   pat_ty_1..pat_ty_n
   exp_ty_1..exp_ty_n
   t_1..t_(n-1)

body  :: body_ty
(\pat1 ... patn -> body) :: pat_ty_1 -> ... -> pat_ty_n -> body_ty
pat_i :: pat_ty_i
e_i   :: exp_ty_i
<$>   :: (pat_ty_1 -> ... -> pat_ty_n -> body_ty) -> exp_ty_1 -> t_1
<*>_i :: t_(i-1) -> exp_ty_i -> t_i
join :: tn -> res_ty
-}

tcApplicativeStmts
  :: HsStmtContext Name
  -> [(SyntaxExpr GhcRn, ApplicativeArg GhcRn GhcRn)]
  -> ExpRhoType                         -- rhs_ty
  -> (TcRhoType -> TcM t)               -- thing_inside
  -> TcM ([(SyntaxExpr GhcTcId, ApplicativeArg GhcTcId GhcTcId)], Type, t)

tcApplicativeStmts ctxt pairs rhs_ty thing_inside
 = do { body_ty <- newFlexiTyVarTy liftedTypeKind
      ; let arity = length pairs
      ; ts <- replicateM (arity-1) $ newInferExpTypeInst
      ; exp_tys <- replicateM arity $ newFlexiTyVarTy liftedTypeKind
      ; pat_tys <- replicateM arity $ newFlexiTyVarTy liftedTypeKind
      ; let fun_ty = mkFunTys pat_tys body_ty

       -- NB. do the <$>,<*> operators first, we don't want type errors here
       --     i.e. goOps before goArgs
       -- See Note [Treat rebindable syntax first]
      ; let (ops, args) = unzip pairs
      ; ops' <- goOps fun_ty (zip3 ops (ts ++ [rhs_ty]) exp_tys)

      -- Typecheck each ApplicativeArg separately
      -- See Note [ApplicativeDo and constraints]
      ; args' <- mapM goArg (zip3 args pat_tys exp_tys)

      -- Bring into scope all the things bound by the args,
      -- and typecheck the thing_inside
      -- See Note [ApplicativeDo and constraints]
      ; res <- tcExtendIdEnv (concatMap get_arg_bndrs args') $
               thing_inside body_ty

      ; return (zip ops' args', body_ty, res) }
  where
    goOps _ [] = return []
    goOps t_left ((op,t_i,exp_ty) : ops)
      = do { (_, op')
               <- tcSyntaxOp DoOrigin op
                             [synKnownType t_left, synKnownType exp_ty] t_i $
                   \ _ -> return ()
           ; t_i <- readExpType t_i
           ; ops' <- goOps t_i ops
           ; return (op' : ops') }

    goArg :: (ApplicativeArg GhcRn GhcRn, Type, Type)
          -> TcM (ApplicativeArg GhcTcId GhcTcId)

    goArg (ApplicativeArgOne pat rhs isBody, pat_ty, exp_ty)
      = setSrcSpan (combineSrcSpans (getLoc pat) (getLoc rhs)) $
        addErrCtxt (pprStmtInCtxt ctxt (mkBindStmt pat rhs))   $
        do { rhs' <- tcMonoExprNC rhs (mkCheckExpType exp_ty)
           ; (pat', _) <- tcPat (StmtCtxt ctxt) pat (mkCheckExpType pat_ty) $
                          return ()
           ; return (ApplicativeArgOne pat' rhs' isBody) }

    goArg (ApplicativeArgMany stmts ret pat, pat_ty, exp_ty)
      = do { (stmts', (ret',pat')) <-
                tcStmtsAndThen ctxt tcDoStmt stmts (mkCheckExpType exp_ty) $
                \res_ty  -> do
                  { L _ ret' <- tcMonoExprNC (noLoc ret) res_ty
                  ; (pat', _) <- tcPat (StmtCtxt ctxt) pat (mkCheckExpType pat_ty) $
                                 return ()
                  ; return (ret', pat')
                  }
           ; return (ApplicativeArgMany stmts' ret' pat') }

    get_arg_bndrs :: ApplicativeArg GhcTcId GhcTcId -> [Id]
    get_arg_bndrs (ApplicativeArgOne pat _ _)  = collectPatBinders pat
    get_arg_bndrs (ApplicativeArgMany _ _ pat) = collectPatBinders pat


{- Note [ApplicativeDo and constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An applicative-do is supposed to take place in parallel, so
constraints bound in one arm can't possibly be available in another
(Trac #13242).  Our current rule is this (more details and discussion
on the ticket). Consider

   ...stmts...
   ApplicativeStmts [arg1, arg2, ... argN]
   ...more stmts...

where argi :: ApplicativeArg. Each 'argi' itself contains one or more Stmts.
Now, we say that:

* Constraints required by the argi can be solved from
  constraint bound by ...stmts...

* Constraints and existentials bound by the argi are not available
  to solve constraints required either by argj (where i /= j),
  or by ...more stmts....

* Within the stmts of each 'argi' individually, however, constraints bound
  by earlier stmts can be used to solve later ones.

To achieve this, we just typecheck each 'argi' separately, bring all
the variables they bind into scope, and typecheck the thing_inside.

************************************************************************
*                                                                      *
\subsection{Errors and contexts}
*                                                                      *
************************************************************************

@sameNoOfArgs@ takes a @[RenamedMatch]@ and decides whether the same
number of args are used in each equation.
-}

checkArgs :: Name -> MatchGroup GhcRn body -> TcM ()
checkArgs _ (MG { mg_alts = L _ [] })
    = return ()
checkArgs fun (MG { mg_alts = L _ (match1:matches) })
    | null bad_matches
    = return ()
    | otherwise
    = failWithTc (vcat [ text "Equations for" <+> quotes (ppr fun) <+>
                         text "have different numbers of arguments"
                       , nest 2 (ppr (getLoc match1))
                       , nest 2 (ppr (getLoc (head bad_matches)))])
  where
    n_args1 = args_in_match match1
    bad_matches = [m | m <- matches, args_in_match m /= n_args1]

    args_in_match :: LMatch GhcRn body -> Int
    args_in_match (L _ (Match { m_pats = pats })) = length pats
