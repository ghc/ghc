{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Typechecking patterns
module GHC.Tc.Gen.Pat
   ( tcLetPat
   , newLetBndr
   , LetBndrSpec(..)
   , tcCheckPat, tcCheckPat_O, tcInferPat
   , tcMatchPats
   , addDataConStupidTheta
   )
where

import GHC.Prelude

import {-# SOURCE #-}   GHC.Tc.Gen.Expr( tcSyntaxOp, tcSyntaxOpGen, tcInferRho )

import GHC.Hs
import GHC.Hs.Syn.Type
import GHC.Rename.Utils
import GHC.Tc.Errors.Types
import GHC.Tc.Gen.Sig( TcPragEnv, lookupPragEnv, addInlinePrags )
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Instantiate
import GHC.Types.FieldLabel
import GHC.Types.Id
import GHC.Types.Var
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Core.Multiplicity
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep_syntactic )
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcMType
import GHC.Tc.Zonk.TcType
import GHC.Core.TyCo.Ppr ( pprTyVars )
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Unify
import GHC.Tc.Gen.HsType
import GHC.Builtin.Types
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Origin
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC.Core.ConLike
import GHC.Builtin.Names
import GHC.Types.Basic hiding (SuccessFlag(..))
import GHC.Driver.DynFlags
import GHC.Types.SrcLoc
import GHC.Types.Var.Set
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import qualified GHC.LanguageExtensions as LangExt
import Control.Arrow  ( second )
import Control.Monad
import GHC.Data.FastString
import qualified Data.List.NonEmpty as NE

import GHC.Data.List.SetOps ( getNth )
import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import Data.List( partition )
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.Class

{-
************************************************************************
*                                                                      *
                External interface
*                                                                      *
************************************************************************
-}

tcLetPat :: (Name -> Maybe TcId)
         -> LetBndrSpec
         -> LPat GhcRn -> Scaled ExpSigmaTypeFRR
         -> TcM a
         -> TcM (LPat GhcTc, a)
tcLetPat sig_fn no_gen pat pat_ty thing_inside
  = do { bind_lvl <- getTcLevel
       ; let ctxt = LetPat { pc_lvl    = bind_lvl
                           , pc_sig_fn = sig_fn
                           , pc_new    = no_gen }
             penv = PE { pe_lazy = True
                       , pe_ctxt = ctxt
                       , pe_orig = PatOrigin }
       ; dflags <- getDynFlags
       ; manyIfLazy dflags pat
       ; tc_lpat pat_ty penv pat thing_inside }
  where
    -- The logic is partly duplicated from decideBangHood in
    -- GHC.HsToCore.Utils. Ugh…
    manyIfLazy dflags lpat
      | xopt LangExt.Strict dflags = xstrict lpat
      | otherwise = not_xstrict lpat
      where
        xstrict p@(L _ (LazyPat _ _)) = checkManyPattern LazyPatternReason p pat_ty
        xstrict (L _ (ParPat _ p)) = xstrict p
        xstrict _ = return ()

        not_xstrict (L _ (BangPat _ _)) = return ()
        not_xstrict (L _ (VarPat _ _)) = return ()
        not_xstrict (L _ (ParPat _ p)) = not_xstrict p
        not_xstrict p = checkManyPattern LazyPatternReason p pat_ty

-----------------
tcMatchPats :: forall a.
               HsMatchContextRn
            -> [LPat GhcRn]          -- ^ patterns
            -> [ExpPatType]             -- ^ types of the patterns
            -> TcM a                    -- ^ checker for the body
            -> TcM ([LPat GhcTc], a)
-- See Note [tcMatchPats]
--
-- PRECONDITION:
--    number of visible pats::[LPat GhcRn]   (p is visible, @p is invisible)
--      ==
--    number of visible pat_tys::[ExpPatType]   (ExpFunPatTy is visible,
--                                               ExpForAllPatTy b is visible iff b is Required)
--
-- POSTCONDITION:
--   Returns only the /value/ patterns; see Note [tcMatchPats]

tcMatchPats match_ctxt pats pat_tys thing_inside
  = assertPpr (count isVisibleExpPatType pat_tys == count (isVisArgPat . unLoc) pats)
              (ppr pats $$ ppr pat_tys) $
       -- Check PRECONDITION
       -- When we get @patterns the (length pats) will change
    do { err_ctxt <- getErrCtxt
       ; let loop :: [LPat GhcRn] -> [ExpPatType] -> TcM ([LPat GhcTc], a)

             -- No more patterns.  Discard excess pat_tys;
             -- they should all be invisible.  Example:
             --    f :: Int -> forall a b. blah
             --    f x @p = rhs
             -- We will call tcMatchPats with
             --   pats = [x, @p]
             --   pat_tys = [Int, @a, @b]
             loop [] pat_tys
               = assertPpr (not (any isVisibleExpPatType pat_tys)) (ppr pats $$ ppr pat_tys) $
                 do { res <- setErrCtxt err_ctxt thing_inside
                    ; return ([], res) }

             -- ExpForAllPat: expecting a type pattern
             loop all_pats@(pat : pats) (ExpForAllPatTy (Bndr tv vis) : pat_tys)
               | isVisibleForAllTyFlag vis
               = do { (_p, (ps, res)) <- tc_forall_lpat tv penv pat $
                                         loop pats pat_tys

                    ; return (ps, res) }
                    -- This VisPat is Erased.
                    -- See Note [Invisible binders in functions] in GHC.Hs.Pat

               -- Invisible (Specified) forall in type, and an @a type pattern
               -- E.g.    f :: forall a. Bool -> a -> blah
               --         f @b True  x = rhs1  -- b is bound to skolem a
               --         f @c False y = rhs2  -- c is bound to skolem a
               -- Also handles invisible (Inferred) case originating from type
               -- class deriving; see Note [Inferred invisible patterns]
               | L _ (InvisPat pat_spec tp) <- pat
               , Invisible spec <- vis
               , pat_spec == spec
               = do { (_p, (ps, res)) <- tc_ty_pat tp tv $
                                         loop pats pat_tys
                    ; return (ps, res) }

               | otherwise  -- Discard invisible pat_ty
               = loop all_pats pat_tys

             -- This case handles the user error when an InvisPat is used
             -- without a corresponding invisible (Specified) forall in the type
             -- E.g. 1.  f :: Int
             --          f @a = ...   -- loop (InvisPat{} : _) []
             --      2.  f :: Int -> Int
             --          f @a x = ... -- loop (InvisPat{} : _) (ExpFunPatTy{} : _)
             --      3.  f :: forall a -> Int
             --          f @a t = ... -- loop (InvisPat{} : _) (ExpForAllPatTy (Bndr _ Required) : _)
             --      4.  f :: forall {a}. Int
             --          f @a t = ... -- loop (InvisPat{} : _) (ExpForAllPatTy (Bndr _ Inferred) : _)
             loop (L loc (InvisPat _ tp) : _) _ =
                failAt (locA loc) (TcRnInvisPatWithNoForAll tp)

             -- ExpFunPatTy: expecting a value pattern
             -- tc_lpat will error if it sees a @t type pattern
             loop (pat : pats) (ExpFunPatTy pat_ty : pat_tys)
               = do { (p, (ps, res)) <- tc_lpat pat_ty penv pat $
                                        loop pats pat_tys
                    ; return (p : ps, res) }
                    -- This VisPat is Retained.
                    -- See Note [Invisible binders in functions] in GHC.Hs.Pat

             loop pats@(_:_) [] = pprPanic "tcMatchPats" (ppr pats)
                    -- Failure of PRECONDITION

       ; loop pats pat_tys }
  where
    penv = PE { pe_lazy = False, pe_ctxt = LamPat match_ctxt, pe_orig = PatOrigin }


tcInferPat :: FixedRuntimeRepContext
           -> HsMatchContextRn
           -> LPat GhcRn
           -> TcM a
           -> TcM ((LPat GhcTc, a), TcSigmaTypeFRR)
tcInferPat frr_orig ctxt pat thing_inside
  = tcInferFRR frr_orig $ \ exp_ty ->
    tc_lpat (unrestricted exp_ty) penv pat thing_inside
 where
    penv = PE { pe_lazy = False, pe_ctxt = LamPat ctxt, pe_orig = PatOrigin }

tcCheckPat :: HsMatchContextRn
           -> LPat GhcRn -> Scaled TcSigmaTypeFRR
           -> TcM a                     -- Checker for body
           -> TcM (LPat GhcTc, a)
tcCheckPat ctxt = tcCheckPat_O ctxt PatOrigin

-- | A variant of 'tcPat' that takes a custom origin
tcCheckPat_O :: HsMatchContextRn
             -> CtOrigin              -- ^ origin to use if the type needs inst'ing
             -> LPat GhcRn -> Scaled TcSigmaTypeFRR
             -> TcM a                 -- Checker for body
             -> TcM (LPat GhcTc, a)
tcCheckPat_O ctxt orig pat (Scaled pat_mult pat_ty) thing_inside
  = tc_lpat (Scaled pat_mult (mkCheckExpType pat_ty)) penv pat thing_inside
  where
    penv = PE { pe_lazy = False, pe_ctxt = LamPat ctxt, pe_orig = orig }


{- Note [tcMatchPats]
~~~~~~~~~~~~~~~~~~~~~
tcMatchPats is the externally-callable wrapper function for
  function definitions  f p1 .. pn = rhs
  lambdas               \p1 .. pn -> body
Typecheck the patterns, extend the environment to bind the variables, do the
thing inside, use any existentially-bound dictionaries to discharge parts of
the returning LIE, and deal with pattern type signatures

It takes the list of patterns writen by the user, but it returns only the
/value/ patterns.  For example:
     f :: forall a. forall b -> a -> Mabye b -> blah
     f @a w x (Just y) = ....
tcMatchPats returns only the /value/ patterns [x, Just y].  Why?  The
desugarer expects only value patterns.  (We could change that, but we would
have to do so carefullly.)  However, distinguishing value patterns from type
patterns is a bit tricky; e.g. the `w` in this example.  So it's very
convenient to filter them out right here.


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
      HsMatchContextRn

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

tcPatBndr :: PatEnv -> Name -> Scaled ExpSigmaTypeFRR -> TcM (HsWrapper, TcId)
-- (coi, xp) = tcPatBndr penv x pat_ty
-- Then coi : pat_ty ~ typeof(xp)
--
tcPatBndr penv@(PE { pe_ctxt = LetPat { pc_lvl    = bind_lvl
                                      , pc_sig_fn = sig_fn
                                      , pc_new    = no_gen } })
          bndr_name exp_pat_ty
  -- For the LetPat cases, see
  -- Note [Typechecking pattern bindings] in GHC.Tc.Gen.Bind

  | Just bndr_id <- sig_fn bndr_name   -- There is a signature
  = do { wrap <- tc_sub_type penv (scaledThing exp_pat_ty) (idType bndr_id)
           -- See Note [Subsumption check at pattern variables]
       ; traceTc "tcPatBndr(sig)" (ppr bndr_id $$ ppr (idType bndr_id) $$ ppr exp_pat_ty)
       ; return (wrap, bndr_id) }

  | otherwise                          -- No signature
  = do { (co, bndr_ty) <- case scaledThing exp_pat_ty of
             Check pat_ty    -> promoteTcType bind_lvl pat_ty
             Infer infer_res -> assert (bind_lvl `sameDepthAs` ir_lvl infer_res) $
                                -- If we were under a constructor that bumped the
                                -- level, we'd be in checking mode (see tcConArg)
                                -- hence this assertion
                                do { bndr_ty <- inferResultToType infer_res
                                   ; return (mkNomReflCo bndr_ty, bndr_ty) }
       ; let bndr_mult = scaledMult exp_pat_ty
       ; bndr_id <- newLetBndr no_gen bndr_name bndr_mult bndr_ty
       ; traceTc "tcPatBndr(nosig)" (vcat [ ppr bind_lvl
                                          , ppr exp_pat_ty, ppr bndr_ty, ppr co
                                          , ppr bndr_id ])
       ; return (mkWpCastN co, bndr_id) }

tcPatBndr _ bndr_name pat_ty
  = do { let pat_mult = scaledMult pat_ty
       ; pat_ty <- expTypeToType (scaledThing pat_ty)
       ; traceTc "tcPatBndr(not let)" (ppr bndr_name $$ ppr pat_ty)
       ; return (idHsWrapper, mkLocalIdOrCoVar bndr_name pat_mult pat_ty) }
               -- We should not have "OrCoVar" here, this is a bug (#17545)
               -- Whether or not there is a sig is irrelevant,
               -- as this is local

newLetBndr :: LetBndrSpec -> Name -> Mult -> TcType -> TcM TcId
-- Make up a suitable Id for the pattern-binder.
-- See Note [Typechecking pattern bindings], item (4) in GHC.Tc.Gen.Bind
--
-- In the polymorphic case when we are going to generalise
--    (plan InferGen, no_gen = LetLclBndr), generate a "monomorphic version"
--    of the Id; the original name will be bound to the polymorphic version
--    by the AbsBinds
-- In the monomorphic case when we are not going to generalise
--    (plan NoGen, no_gen = LetGblBndr) there is no AbsBinds,
--    and we use the original name directly
newLetBndr LetLclBndr name w ty
  = do { mono_name <- cloneLocalName name
       ; return (mkLocalId mono_name w ty) }
newLetBndr (LetGblBndr prags) name w ty
  = addInlinePrags (mkLocalId name w ty) (lookupPragEnv prags name)

tc_sub_type :: PatEnv -> ExpSigmaType -> TcSigmaType -> TcM HsWrapper
-- tcSubTypeET with the UserTypeCtxt specialised to GenSigCtxt
-- Used during typechecking patterns
tc_sub_type penv t1 t2 = tcSubTypePat (pe_orig penv) GenSigCtxt t1 t2

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
                          PatEnv
                       -> inp
                       -> TcM r      -- Thing inside
                       -> TcM ( out
                              , r    -- Result of thing inside
                              )

tcMultiple_ :: Checker inp () -> PatEnv -> [inp] -> TcM r -> TcM r
tcMultiple_ tc_pat penv args thing_inside
  = do { (_, res) <- tcMultiple tc_pat penv args thing_inside
       ; return res }

tcMultiple :: Checker inp out -> Checker [inp] [out]
tcMultiple tc_pat penv args thing_inside
  = do  { err_ctxt <- getErrCtxt
        ; let loop []
                = do { res <- thing_inside
                     ; return ([], res) }

              loop (arg:args)
                = do { (p', (ps', res))
                                <- tc_pat penv arg $
                                   setErrCtxt err_ctxt $
                                   loop args
                -- setErrCtxt: restore context before doing the next pattern
                -- See Note [Nesting] above

                     ; return (p':ps', res) }

        ; loop args }

--------------------
tc_lpat :: Scaled ExpSigmaTypeFRR
        -> Checker (LPat GhcRn) (LPat GhcTc)
tc_lpat pat_ty penv (L span pat) thing_inside
  = setSrcSpanA span $
    do  { (pat', res) <- maybeWrapPatCtxt pat (tc_pat pat_ty penv pat)
                                          thing_inside
        ; return (L span pat', res) }

tc_lpats :: [Scaled ExpSigmaTypeFRR]
         -> Checker [LPat GhcRn] [LPat GhcTc]
tc_lpats tys penv pats
  = assertPpr (equalLength pats tys) (ppr pats $$ ppr tys) $
    tcMultiple (\ penv' (p,t) -> tc_lpat t penv' p)
               penv
               (zipEqual "tc_lpats" pats tys)

--------------------
checkManyPattern :: NonLinearPatternReason -> LPat GhcRn -> Scaled a -> TcM ()
checkManyPattern reason pat pat_ty = tcSubMult (NonLinearPatternOrigin reason pat) ManyTy (scaledMult pat_ty)


tc_forall_lpat :: TcTyVar -> Checker (LPat GhcRn) (LPat GhcTc)
tc_forall_lpat tv penv (L span pat) thing_inside
  = setSrcSpanA span $
    do  { (pat', res) <- maybeWrapPatCtxt pat (tc_forall_pat tv penv pat)
                                          thing_inside
        ; return (L span pat', res) }

tc_forall_pat :: TcTyVar -> Checker (Pat GhcRn) (Pat GhcTc)
tc_forall_pat tv penv (ParPat x lpat) thing_inside
  = do { (lpat', res) <- tc_forall_lpat tv penv lpat thing_inside
       ; return (ParPat x lpat', res) }

tc_forall_pat tv _ (EmbTyPat _ tp) thing_inside
  -- The entire type pattern is guarded with the `type` herald:
  --    f (type t) (x :: t) = ...
  -- This special case is not necessary for correctness but avoids
  -- a redundant `ExpansionPat` node.
  = do { (arg_ty, result) <- tc_ty_pat tp tv thing_inside
       ; return (EmbTyPat arg_ty tp, result) }

tc_forall_pat tv _ pat thing_inside
  -- The type pattern is not guarded with the `type` herald, or perhaps
  -- only parts of it are, e.g.
  --    f (t :: Type)        (x :: t) = ...    -- no `type` herald
  --    f ((type t) :: Type) (x :: t) = ...    -- nested `type` herald
  -- Apply a recursive T2T transformation.
  = do { tp <- pat_to_type_pat pat
       ; (arg_ty, result) <- tc_ty_pat tp tv thing_inside
       ; let pat' = XPat $ ExpansionPat pat (EmbTyPat arg_ty tp)
       ; return (pat', result) }


-- Convert a Pat into the equivalent HsTyPat.
-- See `expr_to_type` (GHC.Tc.Gen.App) for the HsExpr counterpart.
-- The `TcM` monad is only used to fail on ill-formed type patterns.
pat_to_type_pat :: Pat GhcRn -> TcM (HsTyPat GhcRn)
pat_to_type_pat pat = do
  (ty, x) <- runWriterT (pat_to_type pat)
  pure (HsTP (buildHsTyPatRn x) ty)

pat_to_type :: Pat GhcRn -> WriterT HsTyPatRnBuilder TcM (LHsType GhcRn)
pat_to_type (EmbTyPat _ (HsTP x t)) =
  do { tell (builderFromHsTyPatRn x)
     ; return t }
pat_to_type (VarPat _ lname)  =
  do { tell (tpBuilderExplicitTV (unLoc lname))
     ; return b }
  where b = noLocA (HsTyVar noAnn NotPromoted lname)
pat_to_type (WildPat _) = return b
  where b = noLocA (HsWildCardTy noExtField)
pat_to_type (SigPat _ pat sig_ty)
  = do { t <- pat_to_type (unLoc pat)
       ; let { !(HsPS x_hsps k) = sig_ty
             ; b = noLocA (HsKindSig noAnn t k) }
       ; tell (tpBuilderPatSig x_hsps)
       ; return b }
pat_to_type (ParPat _ pat)
  = do { t <- pat_to_type (unLoc pat)
       ; return (noLocA (HsParTy noAnn t)) }
pat_to_type (SplicePat (HsUntypedSpliceTop mod_finalizers pat) splice) = do
      { t <- pat_to_type pat
      ; return (noLocA (HsSpliceTy (HsUntypedSpliceTop mod_finalizers t) splice)) }

pat_to_type (TuplePat _ pats Boxed)
  = do { tys <- traverse (pat_to_type . unLoc) pats
       ; let t = noLocA (HsExplicitTupleTy noExtField tys)
       ; pure t }
pat_to_type (ListPat _ pats)
  = do { tys <- traverse (pat_to_type . unLoc) pats
       ; let t = noLocA (HsExplicitListTy NoExtField NotPromoted tys)
       ; pure t }

pat_to_type (LitPat _ lit)
  | Just ty_lit <- tyLitFromLit lit
  = do { let t = noLocA (HsTyLit noExtField ty_lit)
      ; pure t }
pat_to_type (NPat _ (L _ lit) _ _)
  | Just ty_lit <- tyLitFromOverloadedLit (ol_val lit)
  = do { let t = noLocA (HsTyLit noExtField ty_lit)
       ; pure t}

pat_to_type (ConPat _ lname (InfixCon left right))
  = do { lty <- pat_to_type (unLoc left)
       ; rty <- pat_to_type (unLoc right)
       ; let { t = noLocA (HsOpTy noExtField NotPromoted lty lname rty)}
       ; pure t }
pat_to_type (ConPat _ lname (PrefixCon invis_args vis_args))
  = do { let { appHead = noLocA (HsTyVar noAnn NotPromoted lname)}
       ; ty_invis <- foldM apply_invis_arg appHead invis_args
       ; tys_vis <- traverse (pat_to_type . unLoc) vis_args
       ; let t = foldl' mkHsAppTy ty_invis tys_vis
       ; pure t }
      where
        apply_invis_arg :: LHsType GhcRn -> HsConPatTyArg GhcRn -> WriterT HsTyPatRnBuilder TcM (LHsType GhcRn)
        apply_invis_arg !t (HsConPatTyArg _ (HsTP argx arg))
          = do { tell (builderFromHsTyPatRn argx)
               ; pure (mkHsAppKindTy noExtField t arg)}

pat_to_type pat = lift $
  failWith $ TcRnIllformedTypePattern pat
  -- This failure is the only use of the TcM monad in `pat_to_type_pat`

{-
Note [Pattern to type (P2T) conversion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

  data T a b where
    MkT :: forall a. forall b -> a -> b -> T a b
    -- NB: `a` is invisible, but `b` is required

  f (MkT @[Int] (Maybe Bool) x y) = ...

The second type argument of `MkT` is Required, so we write it without
an `@` sign in the pattern match.  So the (Maybe Bool) will be
  * parsed and renamed as a term pattern
  * converted to a type when typechecking the pattern-match: the P2T conversion

This is the only place we have P2T. In type-lambdas, the "pattern" is always a
type variable:

   f :: forall a -> a -> blah
   f b (x::b) = ...

The `b` argument must be a simple variable; we can't pattern-match on types.

The function `pat_to_type` does the P2T conversion:
   pat_to_type :: Pat GhcRn -> WriterT HsTyPatRnBuilder TcM (LHsType GhcRn)

It is arranged as a writer monad, where the `HsTyPatRnBuilder` accumulates the
binders bound by the type.  (We could discover these binders by a subsequent
traversal, that would mean writing another traversal.)
-}

tc_ty_pat :: HsTyPat GhcRn -> TcTyVar -> TcM r -> TcM (TcType, r)
tc_ty_pat tp tv thing_inside
  = do { (sig_wcs, sig_ibs, arg_ty) <- tcHsTyPat tp (varType tv)
       ; _ <- unifyType Nothing arg_ty (mkTyVarTy tv)
       ; result <- tcExtendNameTyVarEnv sig_wcs $
                   tcExtendNameTyVarEnv sig_ibs $
                   thing_inside
       ; return (arg_ty, result) }

tc_pat  :: Scaled ExpSigmaTypeFRR
        -- ^ Fully refined result type
        -> Checker (Pat GhcRn) (Pat GhcTc)
        -- ^ Translated pattern

tc_pat pat_ty penv ps_pat thing_inside = case ps_pat of

  VarPat x (L l name) -> do
        { (wrap, id) <- tcPatBndr penv name pat_ty
        ; res <- tcCheckUsage name (scaledMult pat_ty) $
                              tcExtendIdEnv1 name id thing_inside
        ; pat_ty <- readExpType (scaledThing pat_ty)
        ; return (mkHsWrapPat wrap (VarPat x (L l id)) pat_ty, res) }

  ParPat x pat -> do
        { (pat', res) <- tc_lpat pat_ty penv pat thing_inside
        ; return (ParPat x pat', res) }

  BangPat x pat -> do
        { (pat', res) <- tc_lpat pat_ty penv pat thing_inside
        ; return (BangPat x pat', res) }

  OrPat _ pats -> do -- See Note [Implementation of OrPatterns], Typechecker (1)
    { let pats_list = NE.toList pats
    ; (pats_list', (res, pat_ct)) <- tc_lpats (map (const pat_ty) pats_list) penv pats_list (captureConstraints thing_inside)
    ; let pats' = NE.fromList pats_list' -- tc_lpats preserves non-emptiness
    ; emitConstraints pat_ct
        -- captureConstraints/extendConstraints:
        --   like in Note [Hopping the LIE in lazy patterns]
    ; pat_ty <- expTypeToType (scaledThing pat_ty)
    ; return (OrPat pat_ty pats', res) }

  LazyPat x pat -> do
        { checkManyPattern LazyPatternReason (noLocA ps_pat) pat_ty
        ; (pat', (res, pat_ct))
                <- tc_lpat pat_ty (makeLazy penv) pat $
                   captureConstraints thing_inside
                -- Ignore refined penv', revert to penv

        ; emitConstraints pat_ct
        -- captureConstraints/extendConstraints:
        --   see Note [Hopping the LIE in lazy patterns]

        -- Check that the expected pattern type is itself lifted
        ; pat_ty <- readExpType (scaledThing pat_ty)
        ; _ <- unifyType Nothing (typeKind pat_ty) liftedTypeKind

        ; return ((LazyPat x pat'), res) }

  WildPat _ -> do
        { checkManyPattern OtherPatternReason (noLocA ps_pat) pat_ty
        ; res <- thing_inside
        ; pat_ty <- expTypeToType (scaledThing pat_ty)
        ; return (WildPat pat_ty, res) }

  AsPat x (L nm_loc name) pat -> do
        { checkManyPattern OtherPatternReason (noLocA ps_pat) pat_ty
        ; (wrap, bndr_id) <- setSrcSpanA nm_loc (tcPatBndr penv name pat_ty)
        ; (pat', res) <- tcExtendIdEnv1 name bndr_id $
                         tc_lpat (pat_ty `scaledSet`(mkCheckExpType $ idType bndr_id))
                                 penv pat thing_inside
            -- NB: if we do inference on:
            --          \ (y@(x::forall a. a->a)) = e
            -- we'll fail.  The as-pattern infers a monotype for 'y', which then
            -- fails to unify with the polymorphic type for 'x'.  This could
            -- perhaps be fixed, but only with a bit more work.
            --
            -- If you fix it, don't forget the bindInstsOfPatIds!
        ; pat_ty <- readExpType (scaledThing pat_ty)
        ; return (mkHsWrapPat wrap (AsPat x (L nm_loc bndr_id) pat') pat_ty, res) }

  ViewPat _ expr pat -> do
        { checkManyPattern ViewPatternReason (noLocA ps_pat) pat_ty
         --
         -- It should be possible to have view patterns at linear (or otherwise
         -- non-Many) multiplicity. But it is not clear at the moment what
         -- restriction need to be put in place, if any, for linear view
         -- patterns to desugar to type-correct Core.

        ; (expr',expr_ty) <- tcInferRho expr
               -- Note [View patterns and polymorphism]

         -- Expression must be a function
        ; let herald = ExpectedFunTyViewPat $ unLoc expr
        ; (expr_wrap1, Scaled _mult inf_arg_ty, inf_res_sigma)
            <- matchActualFunTy herald (Just . HsExprRnThing $ unLoc expr) (1,expr_ty) expr_ty
               -- See Note [View patterns and polymorphism]
               -- expr_wrap1 :: expr_ty "->" (inf_arg_ty -> inf_res_sigma)

         -- Check that overall pattern is more polymorphic than arg type
        ; expr_wrap2 <- tc_sub_type penv (scaledThing pat_ty) inf_arg_ty
            -- expr_wrap2 :: pat_ty "->" inf_arg_ty

         -- Pattern must have inf_res_sigma
        ; (pat', res) <- tc_lpat (pat_ty `scaledSet` mkCheckExpType inf_res_sigma) penv pat thing_inside

        ; let Scaled w h_pat_ty = pat_ty
        ; pat_ty <- readExpType h_pat_ty
        ; let expr_wrap2' = mkWpFun expr_wrap2 idHsWrapper
                              (Scaled w pat_ty) inf_res_sigma
          -- expr_wrap2' :: (inf_arg_ty -> inf_res_sigma) "->"
          --                (pat_ty -> inf_res_sigma)
          -- NB: pat_ty comes from matchActualFunTy, so it has a
          -- fixed RuntimeRep, as needed to call mkWpFun.
        ; let
              expr_wrap = expr_wrap2' <.> expr_wrap1

        ; return $ (ViewPat pat_ty (mkLHsWrap expr_wrap expr') pat', res) }

  ModifiedPat _ mods pat -> do
          -- We don't do anything with modifiers, but we do need to make sure
          -- they type check.
        { _ <- tcModifiers mods (const False)
        ; (pat', res) <- tc_lpat pat_ty penv pat thing_inside
        ; return (ModifiedPat noExtField [] pat', res)
        }

{- Note [View patterns and polymorphism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this exotic example:
   pair :: forall a. Bool -> a -> forall b. b -> (a,b)

   f :: Int -> blah
   f (pair True -> x) = ...here (x :: forall b. b -> (Int,b))

The expression (pair True) should have type
    pair True :: Int -> forall b. b -> (Int,b)
so that it is ready to consume the incoming Int. It should be an
arrow type (t1 -> t2); hence using (tcInferRho expr).

Then, when taking that arrow apart we want to get a *sigma* type
(forall b. b->(Int,b)), because that's what we want to bind 'x' to.
Fortunately that's what matchActualFunTy returns anyway.
-}

-- Type signatures in patterns
-- See Note [Pattern coercions] below
  SigPat _ pat sig_ty -> do
        { (inner_ty, tv_binds, wcs, wrap) <- tcPatSig (inPatBind penv)
                                                            sig_ty (scaledThing pat_ty)
                -- Using tcExtendNameTyVarEnv is appropriate here
                -- because we're not really bringing fresh tyvars into scope.
                -- We're *naming* existing tyvars. Note that it is OK for a tyvar
                -- from an outer scope to mention one of these tyvars in its kind.
        ; (pat', res) <- tcExtendNameTyVarEnv wcs      $
                         tcExtendNameTyVarEnv tv_binds $
                         tc_lpat (pat_ty `scaledSet` mkCheckExpType inner_ty) penv pat thing_inside
        ; pat_ty <- readExpType (scaledThing pat_ty)
        ; return (mkHsWrapPat wrap (SigPat inner_ty pat' sig_ty) pat_ty, res) }

------------------------
-- Lists, tuples, arrays

  -- Necessarily a built-in list pattern, not an overloaded list pattern.
  -- See Note [Desugaring overloaded list patterns].
  ListPat _ pats -> do
        { (coi, elt_ty) <- matchExpectedPatTy matchExpectedListTy penv (scaledThing pat_ty)
        ; (pats', res) <- tcMultiple (tc_lpat (pat_ty `scaledSet` mkCheckExpType elt_ty))
                                     penv pats thing_inside
        ; pat_ty <- readExpType (scaledThing pat_ty)
        ; return (mkHsWrapPat coi
                         (ListPat elt_ty pats') pat_ty, res)
}

  TuplePat _ pats boxity -> do
        { let arity = length pats
              tc = tupleTyCon boxity arity
              -- NB: tupleTyCon does not flatten 1-tuples
              -- See Note [Don't flatten tuples from HsSyn] in GHC.Core.Make
        ; checkTupSize arity
        ; (coi, arg_tys) <- matchExpectedPatTy (matchExpectedTyConApp tc)
                                               penv (scaledThing pat_ty)
                     -- Unboxed tuples have RuntimeRep vars, which we discard:
                     -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
        ; let con_arg_tys = case boxity of Unboxed -> drop arity arg_tys
                                           Boxed   -> arg_tys
        ; (pats', res) <- tc_lpats (map (scaledSet pat_ty . mkCheckExpType) con_arg_tys)
                                   penv pats thing_inside

        ; dflags <- getDynFlags

        -- Under flag control turn a pattern (x,y,z) into ~(x,y,z)
        -- so that we can experiment with lazy tuple-matching.
        -- This is a pretty odd place to make the switch, but
        -- it was easy to do.
        ; let
              unmangled_result = TuplePat con_arg_tys pats' boxity
                                 -- pat_ty /= pat_ty iff coi /= IdCo
              possibly_mangled_result
                | gopt Opt_IrrefutableTuples dflags &&
                  isBoxed boxity   = LazyPat noExtField (noLocA unmangled_result)
                | otherwise        = unmangled_result

        ; pat_ty <- readExpType (scaledThing pat_ty)
        ; massert (con_arg_tys `equalLength` pats) -- Syntactically enforced
        ; return (mkHsWrapPat coi possibly_mangled_result pat_ty, res)
        }

  SumPat _ pat alt arity  -> do
        { let tc = sumTyCon arity
        ; (coi, arg_tys) <- matchExpectedPatTy (matchExpectedTyConApp tc)
                                               penv (scaledThing pat_ty)
        ; -- Drop levity vars, we don't care about them here
          let con_arg_tys = drop arity arg_tys
        ; (pat', res) <- tc_lpat (pat_ty `scaledSet` mkCheckExpType (con_arg_tys `getNth` (alt - 1)))
                                 penv pat thing_inside
        ; pat_ty <- readExpType (scaledThing pat_ty)
        ; return (mkHsWrapPat coi (SumPat con_arg_tys pat' alt arity) pat_ty
                 , res)
        }

------------------------
-- Data constructors
  ConPat _ con arg_pats ->
    tcConPat penv con pat_ty arg_pats thing_inside

------------------------
-- Literal patterns
  LitPat x simple_lit -> do
        { let lit_ty = hsLitType simple_lit
        ; wrap   <- tc_sub_type penv (scaledThing pat_ty) lit_ty
        ; res    <- thing_inside
        ; pat_ty <- readExpType (scaledThing pat_ty)
        ; return ( mkHsWrapPat wrap (LitPat x (convertLit simple_lit)) pat_ty
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
  NPat _ (L l over_lit) mb_neg eq -> do
        { checkManyPattern OtherPatternReason (noLocA ps_pat) pat_ty
          -- It may be possible to refine linear pattern so that they work in
          -- linear environments. But it is not clear how useful this is.
        ; let orig = LiteralOrigin over_lit
        ; ((lit', mb_neg'), eq')
            <- tcSyntaxOp orig eq [SynType (scaledThing pat_ty), SynAny]
                          (mkCheckExpType boolTy) $
               \ [neg_lit_ty] _ ->
               let new_over_lit lit_ty = newOverloadedLit over_lit
                                           (mkCheckExpType lit_ty)
               in case mb_neg of
                 Nothing  -> (, Nothing) <$> new_over_lit neg_lit_ty
                 Just neg -> -- Negative literal
                             -- The 'negate' is re-mappable syntax
                   second Just <$>
                   (tcSyntaxOp orig neg [SynRho] (mkCheckExpType neg_lit_ty) $
                    \ [lit_ty] _ -> new_over_lit lit_ty)
                     -- applied to a closed literal: linearity doesn't matter as
                     -- literals are typed in an empty environment, hence have
                     -- all multiplicities.

        ; res <- thing_inside
        ; pat_ty <- readExpType (scaledThing pat_ty)
        ; return (NPat pat_ty (L l lit') mb_neg' eq', res) }

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

Note that we need to type-check the literal twice, because it is used
twice, and may be used at different types. The second HsOverLit stored in the
AST is used for the subtraction operation.
-}

-- See Note [NPlusK patterns]
  NPlusKPat _ (L nm_loc name)
               (L loc lit) _ ge minus -> do
        { checkManyPattern OtherPatternReason (noLocA ps_pat) pat_ty
        ; let pat_exp_ty = scaledThing pat_ty
              orig = LiteralOrigin lit
        ; (lit1', ge')
            <- tcSyntaxOp orig ge [SynType pat_exp_ty, SynRho]
                                  (mkCheckExpType boolTy) $
               \ [lit1_ty] _ ->
               newOverloadedLit lit (mkCheckExpType lit1_ty)
        ; ((lit2', minus_wrap, bndr_id), minus')
            <- tcSyntaxOpGen orig minus [SynType pat_exp_ty, SynRho] SynAny $
               \ [lit2_ty, var_ty] _ ->
               do { lit2' <- newOverloadedLit lit (mkCheckExpType lit2_ty)
                  ; (wrap, bndr_id) <- setSrcSpanA nm_loc $
                                     tcPatBndr penv name (unrestricted $ mkCheckExpType var_ty)
                           -- co :: var_ty ~ idType bndr_id

                           -- minus_wrap is applicable to minus'
                  ; return (lit2', wrap, bndr_id) }

        ; pat_ty <- readExpType pat_exp_ty

        -- The Report says that n+k patterns must be in Integral
        -- but it's silly to insist on this in the RebindableSyntax case
        ; unlessM (xoptM LangExt.RebindableSyntax) $
          do { icls <- tcLookupClass integralClassName
             ; instStupidTheta orig [mkClassPred icls [pat_ty]] }

        ; res <- tcExtendIdEnv1 name bndr_id thing_inside

        ; let minus'' = case minus' of
                          NoSyntaxExprTc -> pprPanic "tc_pat NoSyntaxExprTc" (ppr minus')
                                   -- this should be statically avoidable
                                   -- Case (3) from Note [NoSyntaxExpr] in "GHC.Hs.Expr"
                          SyntaxExprTc { syn_expr = minus'_expr
                                       , syn_arg_wraps = minus'_arg_wraps
                                       , syn_res_wrap = minus'_res_wrap }
                            -> SyntaxExprTc { syn_expr = minus'_expr
                                            , syn_arg_wraps = minus'_arg_wraps
                                            , syn_res_wrap = minus_wrap <.> minus'_res_wrap }
                             -- Oy. This should really be a record update, but
                             -- we get warnings if we try. #17783
              pat' = NPlusKPat pat_ty (L nm_loc bndr_id) (L loc lit1') lit2'
                               ge' minus''
        ; return (pat', res) }

-- Here we get rid of it and add the finalizers to the global environment.
-- See Note [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice.
  SplicePat (HsUntypedSpliceTop mod_finalizers pat) _ -> do
      { addModFinalizersWithLclEnv mod_finalizers
      ; tc_pat pat_ty penv pat thing_inside }

  SplicePat (HsUntypedSpliceNested _) _ -> panic "tc_pat: nested splice in splice pat"

  EmbTyPat _ _ -> failWith TcRnIllegalTypePattern

  InvisPat _ _ -> panic "tc_pat: invisible pattern appears recursively in the pattern"

  XPat (HsPatExpanded lpat rpat) -> do
    { (rpat', res) <- tc_pat pat_ty penv rpat thing_inside
    ; return (XPat $ ExpansionPat lpat rpat', res) }

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
            Pattern signatures   (pat :: type)
*                                                                      *
************************************************************************
-}

tcPatSig :: Bool                    -- True <=> pattern binding
         -> HsPatSigType GhcRn
         -> ExpSigmaType
         -> TcM (TcType,            -- The type to use for "inside" the signature
                 [(Name,TcTyVar)],  -- The new bit of type environment, binding
                                    -- the scoped type variables
                 [(Name,TcTyVar)],  -- The wildcards
                 HsWrapper)         -- Coercion due to unification with actual ty
                                    -- Of shape:  res_ty ~ sig_ty
tcPatSig in_pat_bind sig res_ty
 = do  { (sig_wcs, sig_tvs, sig_ty) <- tcHsPatSigType PatSigCtxt HM_Sig sig OpenKind
        -- sig_tvs are the type variables free in 'sig',
        -- and not already in scope. These are the ones
        -- that should be brought into scope

        ; case NE.nonEmpty sig_tvs of
            Nothing -> do {
                -- Just do the subsumption check and return
                  wrap <- addErrCtxtM (mk_msg sig_ty) $
                          tcSubTypePat PatSigOrigin PatSigCtxt res_ty sig_ty
                ; return (sig_ty, [], sig_wcs, wrap)
                }
            Just sig_tvs_ne -> do
                -- Type signature binds at least one scoped type variable

                -- A pattern binding cannot bind scoped type variables
                -- It is more convenient to make the test here
                -- than in the renamer
              when in_pat_bind
                (addErr (TcRnCannotBindScopedTyVarInPatSig sig_tvs_ne))

              -- Now do a subsumption check of the pattern signature against res_ty
              wrap <- addErrCtxtM (mk_msg sig_ty) $
                      tcSubTypePat PatSigOrigin PatSigCtxt res_ty sig_ty

              -- Phew!
              return (sig_ty, sig_tvs, sig_wcs, wrap)
       }
  where
    mk_msg sig_ty tidy_env
       = do { (tidy_env, sig_ty) <- zonkTidyTcType tidy_env sig_ty
            ; res_ty <- readExpType res_ty   -- should be filled in by now
            ; (tidy_env, res_ty) <- zonkTidyTcType tidy_env res_ty
            ; let msg = vcat [ hang (text "When checking that the pattern signature:")
                                  4 (ppr sig_ty)
                             , nest 2 (hang (text "fits the type of its context:")
                                          2 (ppr res_ty)) ]
            ; return (tidy_env, msg) }


{- *********************************************************************
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

As explained by [Wrappers for data instance tycons] in GHC.Types.Id.Make, the
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

Note [Freshen existentials]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is essential that these existentials are freshened.
Otherwise, if we have something like
  case (a :: Ex, b :: Ex) of (MkEx ..., MkEx ...) -> ...
we'll give both unpacked existential variables the
same name, leading to shadowing.

-}

--      Running example:
-- MkT :: forall a b c. (a~[b]) => b -> c -> T a
--       with scrutinee of type (T ty)

tcConPat :: PatEnv -> LocatedN Name
         -> Scaled ExpSigmaTypeFRR    -- Type of the pattern
         -> HsConPatDetails GhcRn -> TcM a
         -> TcM (Pat GhcTc, a)
tcConPat penv con_lname@(L _ con_name) pat_ty arg_pats thing_inside
  = do  { con_like <- tcLookupConLike con_name
        ; case con_like of
            RealDataCon data_con -> tcDataConPat con_lname data_con pat_ty
                                                 penv arg_pats thing_inside
            PatSynCon pat_syn -> tcPatSynPat con_lname pat_syn pat_ty
                                             penv arg_pats thing_inside
        }

-- Warn when pattern matching on a GADT or a pattern synonym
-- when MonoLocalBinds is off.
warnMonoLocalBinds :: TcM ()
warnMonoLocalBinds
  = do { mono_local_binds <- xoptM LangExt.MonoLocalBinds
       ; unless mono_local_binds $
           addDiagnostic TcRnGADTMonoLocalBinds
           -- We used to require the GADTs or TypeFamilies extension
           -- to pattern match on a GADT (#2905, #7156)
           --
           -- In #20485 this was made into a warning.
       }

tcDataConPat :: LocatedN Name -> DataCon
             -> Scaled ExpSigmaTypeFRR        -- Type of the pattern
             -> Checker (HsConPatDetails GhcRn) (Pat GhcTc)
tcDataConPat (L con_span con_name) data_con pat_ty_scaled
             penv arg_pats thing_inside
  = do  { let tycon = dataConTyCon data_con
                  -- For data families this is the representation tycon
              (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _)
                = dataConFullSig data_con
              header = L con_span (RealDataCon data_con)

          -- Instantiate the constructor type variables [a->ty]
          -- This may involve doing a family-instance coercion,
          -- and building a wrapper
        ; (wrap, ctxt_res_tys) <- matchExpectedConTy penv tycon pat_ty_scaled
        ; pat_ty <- readExpType (scaledThing pat_ty_scaled)

          -- Add the stupid theta
        ; setSrcSpanA con_span $ addDataConStupidTheta data_con ctxt_res_tys

        -- Check that this isn't a GADT pattern match
        -- in situations in which that isn't allowed.
        ; let all_arg_tys = eqSpecPreds eq_spec ++ theta ++ (map scaledThing arg_tys)
        ; checkGADT (RealDataCon data_con) ex_tvs all_arg_tys penv

        ; tenv1 <- instTyVarsWith PatOrigin univ_tvs ctxt_res_tys
                  -- NB: Do not use zipTvSubst!  See #14154
                  -- We want to create a well-kinded substitution, so
                  -- that the instantiated type is well-kinded

        ; let mc = case pe_ctxt penv of
                     LamPat mc -> mc
                     LetPat {} -> PatBindRhs
        ; skol_info <- mkSkolemInfo (PatSkol (RealDataCon data_con) mc)
        ; (tenv, ex_tvs') <- tcInstSuperSkolTyVarsX skol_info tenv1 ex_tvs
                     -- Get location from monad, not from ex_tvs
                     -- This freshens: See Note [Freshen existentials]
                     -- Why "super"? See Note [Binding when looking up instances]
                     -- in GHC.Core.InstEnv.

        ; let arg_tys'       = substScaledTys tenv arg_tys
              pat_mult       = scaledMult pat_ty_scaled
              arg_tys_scaled = map (scaleScaled pat_mult) arg_tys'
              con_like       = RealDataCon data_con

        -- This check is necessary to uphold the invariant that 'tcConArgs'
        -- is given argument types with a fixed runtime representation.
        -- See test case T20363.
        ; checkFixedRuntimeRep data_con arg_tys'

        ; traceTc "tcConPat" (vcat [ text "con_name:" <+> ppr con_name
                                   , text "univ_tvs:" <+> pprTyVars univ_tvs
                                   , text "ex_tvs:" <+> pprTyVars ex_tvs
                                   , text "eq_spec:" <+> ppr eq_spec
                                   , text "theta:" <+> ppr theta
                                   , text "ex_tvs':" <+> pprTyVars ex_tvs'
                                   , text "ctxt_res_tys:" <+> ppr ctxt_res_tys
                                   , text "pat_ty:" <+> ppr pat_ty
                                   , text "arg_tys':" <+> ppr arg_tys'
                                   , text "arg_pats" <+> ppr arg_pats ])

        ; (univ_ty_args, ex_ty_args) <- splitConTyArgs con_like arg_pats

        ; if null ex_tvs && null eq_spec && null theta
          then do { -- The common case; no class bindings etc
                    -- (see Note [Arrows and patterns])
                    (arg_pats', res) <- tcConTyArgs tenv penv univ_ty_args $
                                        tcConValArgs con_like arg_tys_scaled
                                                     penv arg_pats thing_inside
                  ; let res_pat = ConPat { pat_con = header
                                         , pat_args = arg_pats'
                                         , pat_con_ext = ConPatTc
                                           { cpt_tvs = [], cpt_dicts = []
                                           , cpt_binds = emptyTcEvBinds
                                           , cpt_arg_tys = ctxt_res_tys
                                           , cpt_wrap = idHsWrapper
                                           }
                                         }

                  ; return (mkHsWrapPat wrap res_pat pat_ty, res) }

          else do   -- The general case, with existential,
                    -- and local equality constraints
        { let theta'     = substTheta tenv (eqSpecPreds eq_spec ++ theta)
                           -- order is *important* as we generate the list of
                           -- dictionary binders from theta'

        ; when (not (null eq_spec) || any isEqClassPred theta) warnMonoLocalBinds

        ; given <- newEvVars theta'
        ; (ev_binds, (arg_pats', res))
             <- -- See Note [Type applications in patterns] (W4)
                tcConTyArgs tenv penv univ_ty_args                       $
                checkConstraints (getSkolemInfo skol_info) ex_tvs' given $
                tcConTyArgs tenv penv ex_ty_args                         $
                tcConValArgs con_like arg_tys_scaled penv arg_pats thing_inside

        ; let res_pat = ConPat
                { pat_con   = header
                , pat_args  = arg_pats'
                , pat_con_ext = ConPatTc
                  { cpt_tvs   = ex_tvs'
                  , cpt_dicts = given
                  , cpt_binds = ev_binds
                  , cpt_arg_tys = ctxt_res_tys
                  , cpt_wrap  = idHsWrapper
                  }
                }
        ; return (mkHsWrapPat wrap res_pat pat_ty, res)
        } }

tcPatSynPat :: LocatedN Name -> PatSyn
            -> Scaled ExpSigmaType         -- ^ Type of the pattern
            -> Checker (HsConPatDetails GhcRn) (Pat GhcTc)
tcPatSynPat (L con_span con_name) pat_syn pat_ty penv arg_pats thing_inside
  = do  { let (univ_tvs, req_theta, ex_tvs, prov_theta, arg_tys, ty) = patSynSig pat_syn

        ; (subst, univ_tvs') <- newMetaTyVars univ_tvs

        -- Check that we aren't matching on a GADT-like pattern synonym
        -- in situations in which that isn't allowed.
        ; let all_arg_tys = ty : prov_theta ++ (map scaledThing arg_tys)
        ; checkGADT (PatSynCon pat_syn) ex_tvs all_arg_tys penv

        ; skol_info <- case pe_ctxt penv of
                            LamPat mc -> mkSkolemInfo (PatSkol (PatSynCon pat_syn) mc)
                            LetPat {} -> return unkSkol -- Doesn't matter

        ; (tenv, ex_tvs') <- tcInstSuperSkolTyVarsX skol_info subst ex_tvs
           -- This freshens: Note [Freshen existentials]

        ; let ty'         = substTy tenv ty
              arg_tys'    = substScaledTys tenv arg_tys
              pat_mult    = scaledMult pat_ty
              arg_tys_scaled = map (scaleScaled pat_mult) arg_tys'
              prov_theta' = substTheta tenv prov_theta
              req_theta'  = substTheta tenv req_theta
              con_like    = PatSynCon pat_syn

        ; when (any isEqClassPred prov_theta) warnMonoLocalBinds

        ; checkManyPattern PatternSynonymReason nlWildPatName pat_ty

        ; (univ_ty_args, ex_ty_args) <- splitConTyArgs con_like arg_pats

        ; wrap <- tc_sub_type penv (scaledThing pat_ty) ty'

        ; traceTc "tcPatSynPat" $
          vcat [ text "Pat syn:" <+> ppr pat_syn
               , text "Expected type:" <+> ppr pat_ty
               , text "Pat res ty:" <+> ppr ty'
               , text "ex_tvs':" <+> pprTyVars ex_tvs'
               , text "prov_theta':" <+> ppr prov_theta'
               , text "req_theta':" <+> ppr req_theta'
               , text "arg_tys':" <+> ppr arg_tys'
               , text "univ_ty_args:" <+> ppr univ_ty_args
               , text "ex_ty_args:" <+> ppr ex_ty_args ]

        ; req_wrap <- instCall (OccurrenceOf con_name) (mkTyVarTys univ_tvs') req_theta'
                      -- Origin (OccurrenceOf con_name):
                      -- see Note [Call-stack tracing of pattern synonyms]
        ; traceTc "instCall" (ppr req_wrap)

          -- Pattern synonyms can never have representation-polymorphic argument types,
          -- as checked in 'GHC.Tc.Gen.Sig.tcPatSynSig' (see use of 'FixedRuntimeRepPatSynSigArg')
          -- and 'GHC.Tc.TyCl.PatSyn.tcInferPatSynDecl'.
          -- (If you want to lift this restriction, use 'hasFixedRuntimeRep' here, to match
          -- 'tcDataConPat'.)
        ; let
            bad_arg_tys :: [(Int, Scaled Type)]
            bad_arg_tys = filter (\ (_, Scaled _ arg_ty) -> not (typeHasFixedRuntimeRep arg_ty))
                        $ zip [0..] arg_tys'
        ; massertPpr (null bad_arg_tys) $
            vcat [ text "tcPatSynPat: pattern arguments do not have a fixed RuntimeRep"
                 , text "bad_arg_tys:" <+> ppr bad_arg_tys ]

        ; traceTc "checkConstraints {" Outputable.empty
        ; prov_dicts' <- newEvVars prov_theta'
        ; (ev_binds, (arg_pats', res))
             <- -- See Note [Type applications in patterns] (W4)
                tcConTyArgs tenv penv univ_ty_args                             $
                checkConstraints (getSkolemInfo skol_info) ex_tvs' prov_dicts' $
                tcConTyArgs tenv penv ex_ty_args                               $
                tcConValArgs con_like arg_tys_scaled penv arg_pats             $
                thing_inside
        ; traceTc "checkConstraints }" (ppr ev_binds)

        ; let res_pat = ConPat { pat_con   = L con_span $ PatSynCon pat_syn
                               , pat_args  = arg_pats'
                               , pat_con_ext = ConPatTc
                                 { cpt_tvs   = ex_tvs'
                                 , cpt_dicts = prov_dicts'
                                 , cpt_binds = ev_binds
                                 , cpt_arg_tys = mkTyVarTys univ_tvs'
                                 , cpt_wrap  = req_wrap
                                 }
                               }
        ; pat_ty <- readExpType (scaledThing pat_ty)
        ; return (mkHsWrapPat wrap res_pat pat_ty, res) }

checkFixedRuntimeRep :: DataCon -> [Scaled TcSigmaTypeFRR] -> TcM ()
checkFixedRuntimeRep data_con arg_tys
  = zipWithM_ check_one [1..] arg_tys
  where
    check_one i arg_ty = hasFixedRuntimeRep_syntactic
                            (FRRDataConPatArg data_con i)
                            (scaledThing arg_ty)

{- Note [Call-stack tracing of pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: HasCallStack => blah

   pattern Annotated :: HasCallStack => (CallStack, a) -> a
   pattern Annotated x <- (f -> x)

When we pattern-match against `Annotated` we will call `f`, and must
pass a call-stack.  We may want `Annotated` itself to propagate the call
stack, so we give it a HasCallStack constraint too.  But then we expect
to see `Annotated` in the call stack.

This is achieve easily, but a bit trickily.  When we instantiate
Annotated's "required" constraints, in tcPatSynPat, give them a
CtOrigin of (OccurrenceOf "Annotated"). That way the special magic
in GHC.Tc.Solver.Dict.solveCallStack which deals with CallStack
constraints will kick in: that logic only fires on constraints
whose Origin is (OccurrenceOf f).

See also Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
and Note [Solving CallStack constraints] in GHC.Tc.Solver.Types
-}
----------------------------
-- | Convenient wrapper for calling a matchExpectedXXX function
matchExpectedPatTy :: (TcRhoType -> TcM (TcCoercionN, a))
                    -> PatEnv -> ExpSigmaTypeFRR -> TcM (HsWrapper, a)
-- See Note [Matching polytyped patterns]
-- Returns a wrapper : pat_ty ~R inner_ty
matchExpectedPatTy inner_match (PE { pe_orig = orig }) pat_ty
  = do { pat_ty <- expTypeToType pat_ty
       ; (wrap, pat_rho) <- topInstantiate orig pat_ty
       ; (co, res) <- inner_match pat_rho
       ; traceTc "matchExpectedPatTy" (ppr pat_ty $$ ppr wrap)
       ; return (mkWpCastN (mkSymCo co) <.> wrap, res) }

----------------------------
matchExpectedConTy :: PatEnv
                   -> TyCon
                       -- ^ The TyCon that this data constructor actually returns.
                       -- In the case of a data family, this is
                       -- the /representation/ TyCon.
                   -> Scaled ExpSigmaTypeFRR
                       -- ^ The type of the pattern.
                       -- In the case of a data family, this would
                       -- mention the /family/ TyCon
                   -> TcM (HsWrapper, [TcSigmaType])
-- See Note [Matching constructor patterns]
-- Returns a wrapper : pat_ty "->" T ty1 ... tyn
matchExpectedConTy (PE { pe_orig = orig }) data_tc exp_pat_ty
  | Just (fam_tc, fam_args, co_tc) <- tyConFamInstSig_maybe data_tc
         -- Comments refer to Note [Matching constructor patterns]
         -- co_tc :: forall a. T [a] ~ T7 a
  = do { pat_ty <- expTypeToType (scaledThing exp_pat_ty)
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
             co2 = mkUnbranchedAxInstCo Representational co_tc tys' []
             -- co2 : T (ty1,ty2) ~R T7 ty1 ty2

             full_co = mkSubCo (mkSymCo co1) `mkTransCo` co2
             -- full_co :: pat_rho ~R T7 ty1 ty2

       ; return ( mkWpCastR full_co <.> wrap, tys') }

  | otherwise
  = do { pat_ty <- expTypeToType (scaledThing exp_pat_ty)
       ; (wrap, pat_rho) <- topInstantiate orig pat_ty
       ; (coi, tys) <- matchExpectedTyConApp data_tc pat_rho
       ; return (mkWpCastN (mkSymCo coi) <.> wrap, tys) }

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

{- Note [Type applications in patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type applications in patterns are enabled by -XTypeAbstractions.
For example:
   f :: Either (Maybe a) [b] -> blah
   f (Left @x @[y] (v::Maybe x)) = blah

How should we typecheck them?  The basic plan is pretty simple, and is
all done in tcConTyArgs. For each type argument:

* Step 1:
    * bind the newly-in-scope type variables (here `x` or `y`) to
      unification variables, say `x0` or `y0`

    * typecheck the type argument, `@x` or `@[y]` to get the
      types `x0` or `[y0]`.

    This step is done by `tcHsPatSigType`, similar to the way we
    deal with pattern signatures.

* Step 2: Unify those types with the type arguments we expect from
  the context, in this case (Maybe a) and [b].  These unifications
  will (perhaps after the constraint solver has done its work)
  unify   x0 := Maybe a
          y0 := b
  Thus we learn that x stands for (Maybe a) and y for b.

* Step 3: Extend the lexical context to bind `x` to `x0` and
  `y` to `y0`, and typecheck the body of the pattern match.

However there are several quite tricky wrinkles.

(W1) Surprisingly, we can discard the coercions arising from
     these unifications.  The *only* thing the unification does is
     to side-effect those unification variables, so that we know
     what type x and y stand for; and cause an error if the equality
     is not soluble.  It's a bit like a constraint arising
     from a functional dependency, where we don't use the evidence.

(W2) Note that both here and in pattern signatures the unification may
     not even end up unifying the variable.  For example
       type S a b = a
       f :: Maybe a -> Bool
       f (Just @(S a b) x) = True :: b
     In Step 2 we will unify (S a0 b0 ~ a), which succeeds, but has no
     effect on the unification variable b0, to which 'b' is bound.
     Later, in the RHS, we find that b0 must be Bool, and unify it there.
     All is fine.

(W3) The order of the arguments to the /data constructor/ may differ from
     the order of the arguments to the /type constructor/. Example
         data T a b where { MkT :: forall c d. (c,d) -> T d c }
         f :: T Int Bool -> blah
         f (MkT @x @y p) = ...
     The /first/ type argument to `MkT`, namely `@x` corresponds to the
     /second/ argument to `T` in the type `T Int Bool`.  So `x` is bound
     to `Bool` -- not to `Int`!.  That is why splitConTyArgs uses
     conLikeUserTyVarBinders to match up with the user-supplied type arguments
     in the pattern, not dataConUnivTyVars/dataConExTyVars.

(W4) A similar story works for existentials, but it is subtly different
     (#19847).  Consider
         data T a where { MkT :: forall a b. a -> b -> T a }
         f :: T Int -> blah
         f (MkT @x @y v w) = blah
     Here we create a fresh unification variables x0,y0 for x,y and
     unify x0~Int, y0~b, where b is the fresh existential variable bound by
     the pattern. But
       * (x0~Int) must be /outside/ the implication constraint
       * (y0~b)   must be /inside/ it
     (and hence x0 and y0 themselves must have different levels).
     Thus:
         x0[1]~Int,  (forall[2] b. (y0[2]~b, ...constraints-from-blah...))

     We need (x0~Int) /outside/ so that it can influence the type of the
     pattern in an inferred setting, e.g.
         g :: T _ -> blah
         g (MkT @Int @y v w) = blah
     Here we want to infer `g` to have type `T Int -> blah`. If the
     (x0~Int) was inside the implication, and the the constructor bound
     equality constraints, `x0` would be untouchable. This was the root
     cause of #19847.

     We need (y0~b) to be /inside/ the implication, so that `b` is in
     scope.  In fact, we may actually /need/ equalities bound by the
     implication to prove the equality constraint we generate.
     Example   data T a where
                 MkT :: forall p q. T (p,q)
               f :: T (Int,Bool) -> blah
               f (MkT @Int @Bool) = ...
     We get the implication
        forall[2] p q. (p,q)~(Int,Bool) => (p ~ Int, q ~ Bool, ...)
     where the Given comes from the GADT match, while (p~Int, q~Bool)
     comes from matching the type arguments.

     Wow.  That's all quite subtle! See the long discussion on #19847.  We
     must treat universal and existential arguments separately, even though
     they are all mixed up (W3).  The function splitConTyArgs separates the
     universals from existentials; and we build the implication between
     typechecking the two sets:
           tcConTyArgs ... univ_ty_args    $
           checkConstraints ...            $
           tcConTyArgs ... ex_ty_args      $
           ..typecheck body..
     You can see this code shape in tcDataConPat and tcPatSynPat.

     Where pattern synonyms are involved, this two-level split may not be
     enough.  See #22328 for the story.
-}

{- Note [Omitted record fields and linearity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  data T = MkT {a:A, b:B}
  f :: T -> A
  f (MkT{a=a}) = a

The pattern in f is equivalent to

  f (MkT a _) = a

Evidently, the b field isn't used linearly here, it must be typed as a wildcard
pattern. However, this is *the only check* for omitted record fields: if it
weren't for linearity checking, the type checker could ignore b altogether. So
we have a function check_omitted_fields_multiplicity, whose purpose is to do the
linearity checking on the omitted fields.
-}

tcConValArgs :: ConLike
             -> [Scaled TcSigmaTypeFRR]
             -> Checker (HsConPatDetails GhcRn) (HsConPatDetails GhcTc)
tcConValArgs con_like arg_tys penv con_args thing_inside = case con_args of
  PrefixCon type_args arg_pats -> do
        -- NB: type_args already dealt with
        -- See Note [Type applications in patterns]
        { checkTc (con_arity == no_of_args)     -- Check correct arity
                  (TcRnArityMismatch (AConLike con_like) con_arity no_of_args)

        ; let pats_w_tys = zipEqual "tcConArgs" arg_pats arg_tys
        ; (arg_pats', res) <- tcMultiple tcConArg penv pats_w_tys thing_inside

        ; return (PrefixCon type_args arg_pats', res) }
    where
      con_arity  = conLikeArity con_like
      no_of_args = length arg_pats

  InfixCon p1 p2 -> do
        { checkTc (con_arity == 2)      -- Check correct arity
                  (TcRnArityMismatch (AConLike con_like) con_arity 2)
        ; let [arg_ty1,arg_ty2] = arg_tys       -- This can't fail after the arity check
        ; ([p1',p2'], res) <- tcMultiple tcConArg penv [(p1,arg_ty1),(p2,arg_ty2)]
                                                  thing_inside
        ; return (InfixCon p1' p2', res) }
    where
      con_arity  = conLikeArity con_like

  RecCon (HsRecFields x rpats dd) -> do
        { check_omitted_fields_multiplicity
        ; (rpats', res) <- tcMultiple tc_field penv rpats thing_inside
        ; return ((RecCon (HsRecFields x rpats' dd)), res) }
    where
      tc_field :: Checker (LHsRecField GhcRn (LPat GhcRn))
                          (LHsRecField GhcTc (LPat GhcTc))
      tc_field  penv
                (L l (HsFieldBind ann (L loc (FieldOcc rdr (L lr sel))) pat pun))
                thing_inside
        = do { sel'   <- tcLookupId sel
             ; pat_ty <- setSrcSpanA loc $ find_field_ty sel
                                            (occNameFS $ rdrNameOcc rdr)
             ; (pat', res) <- tcConArg penv (pat, pat_ty) thing_inside
             ; return (L l (HsFieldBind ann (L loc (FieldOcc rdr (L lr sel'))) pat'
                                                                        pun), res) }
      -- See Note [Omitted record fields and linearity]
      check_omitted_fields_multiplicity :: TcM ()
      check_omitted_fields_multiplicity = do
        forM_ omitted_field_tys $ \(fl, pat_ty) ->
          tcSubMult (OmittedFieldOrigin fl) ManyTy (scaledMult pat_ty)

      find_field_ty :: Name -> FastString -> TcM (Scaled TcType)
      find_field_ty sel lbl
        = case [ty | (Just fl, ty) <- bound_field_tys, flSelector fl == sel ] of

                -- No matching field; chances are this field label comes from some
                -- other record type (or maybe none).  If this happens, just fail,
                -- otherwise we get crashes later (#8570), and similar:
                --      f (R { foo = (a,b) }) = a+b
                -- If foo isn't one of R's fields, we don't want to crash when
                -- typechecking the "a+b".
           [] -> failWith (badFieldConErr (getName con_like) (FieldLabelString lbl))

                -- The normal case, when the field comes from the right constructor
           (pat_ty : extras) -> do
                traceTc "find_field" (ppr pat_ty <+> ppr extras)
                assert (null extras) (return pat_ty)

      bound_field_tys, omitted_field_tys :: [(Maybe FieldLabel, Scaled TcType)]
      (bound_field_tys, omitted_field_tys) = partition is_bound all_field_tys

      is_bound :: (Maybe FieldLabel, Scaled TcType) -> Bool
      is_bound (Just fl, _) = elem (flSelector fl) (map (\(L _ (HsFieldBind _ (L _ (FieldOcc _ sel )) _ _)) -> unLoc sel) rpats)
      is_bound _ = False

      all_field_tys :: [(Maybe FieldLabel, Scaled TcType)]
      all_field_tys = zip con_field_labels arg_tys
          -- If the constructor isn't really a record, then dataConFieldLabels
          -- will be empty (and each field in the pattern will generate an error
          -- below). We still need those unnamed fields for
          -- linearity-checking. Hence we zip the anonymous fields with Nothing.

      con_field_labels :: [Maybe FieldLabel]
      con_field_labels = (map Just (conLikeFieldLabels con_like)) ++ repeat Nothing


splitConTyArgs :: ConLike -> HsConPatDetails GhcRn
               -> TcM ( [(HsConPatTyArg GhcRn, TyVar)]    -- Universals
                      , [(HsConPatTyArg GhcRn, TyVar)] )  -- Existentials
-- See Note [Type applications in patterns] (W4)
-- This function is monadic only because of the error check
-- for too many type arguments
splitConTyArgs con_like (PrefixCon type_args _)
  = do { checkTc (type_args `leLength` con_spec_bndrs)
                 (TcRnTooManyTyArgsInConPattern con_like
                          (length con_spec_bndrs) (length type_args))
       ; if null ex_tvs  -- Short cut common case
         then return (bndr_ty_arg_prs, [])
         else return (partition is_universal bndr_ty_arg_prs) }
  where
    ex_tvs = conLikeExTyCoVars con_like
    con_spec_bndrs = [ tv | Bndr tv SpecifiedSpec <- conLikeUserTyVarBinders con_like ]
        -- conLikeUserTyVarBinders: see (W3) in
        --    Note [Type applications in patterns]
        -- SpecifiedSpec: forgetting to filter out inferred binders led to #20443

    bndr_ty_arg_prs = type_args `zip` con_spec_bndrs
                      -- The zip truncates to length(type_args)

    is_universal (_, tv) = not (tv `elem` ex_tvs)
         -- See Note [DataCon user type variable binders] in GHC.Core.DataCon
         -- especially INVARIANT(dataConTyVars).

splitConTyArgs _ (RecCon {})   = return ([], []) -- No type args in RecCon
splitConTyArgs _ (InfixCon {}) = return ([], []) -- No type args in InfixCon

tcConTyArgs :: Subst -> PatEnv -> [(HsConPatTyArg GhcRn, TyVar)]
            -> TcM a -> TcM a
tcConTyArgs tenv penv prs thing_inside
  = tcMultiple_ (tcConTyArg tenv) penv prs thing_inside

tcConTyArg :: Subst -> Checker (HsConPatTyArg GhcRn, TyVar) ()
tcConTyArg tenv penv (HsConPatTyArg _ rn_ty, con_tv) thing_inside
  = do { (sig_wcs, sig_ibs, arg_ty) <- tcHsTyPat rn_ty (substTy tenv (varType con_tv))

       ; case NE.nonEmpty sig_ibs of
           Just sig_ibs_ne | inPatBind penv ->
             addErr (TcRnCannotBindTyVarsInPatBind sig_ibs_ne)
           _ -> pure ()

          -- This unification is straight from Figure 7 of
          -- "Type Variables in Patterns", Haskell'18
          -- OK to drop coercions here. These unifications are all about
          -- guiding inference based on a user-written type annotation
          -- See Note [Type applications in patterns] (W1)
       ; _ <- unifyType Nothing arg_ty (substTyVar tenv con_tv)

       ; result <- tcExtendNameTyVarEnv sig_wcs $
                   tcExtendNameTyVarEnv sig_ibs $
                   thing_inside
             -- NB: Because we call tConTyArgs twice, once for universals and
             --     once for existentials; so this brings things into scope
             --     "out of left-right order". But it doesn't matter; the renamer
             --     has dealt with all that.

       ; return ((), result) }

tcConArg :: Checker (LPat GhcRn, Scaled TcSigmaType) (LPat GhcTc)
tcConArg penv (arg_pat, Scaled arg_mult arg_ty)
  = tc_lpat (Scaled arg_mult (mkCheckExpType arg_ty)) penv arg_pat

addDataConStupidTheta :: DataCon -> [TcType] -> TcM ()
-- Instantiate the "stupid theta" of the data con, and throw
-- the constraints into the constraint set.
-- See Note [The stupid context] in GHC.Core.DataCon.
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
GADT constraints in a 'proc', which we do by disallowing any
non-vanilla pattern match (i.e. one that introduces existential
variables or provided constraints), in tcDataConPat and tcPatSynPat.

We also short-cut the constraint simplification for such vanilla patterns,
so that we bind no constraints. Hence the 'fast path' in tcDataConPat;
which applies more generally (not just within 'proc'), as it's a good
plan in general to bypass the constraint simplification step entirely
when it's not needed.

Note [Pattern coercions]
~~~~~~~~~~~~~~~~~~~~~~~~
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

A SigPat is a type coercion and must be handled one at a time.  We can't
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

See also Note [Typechecking pattern bindings] in GHC.Tc.Gen.Bind
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

-- | Check that a pattern isn't a GADT, or doesn't have existential variables,
-- in a situation in which that is not permitted (inside a lazy pattern, or
-- in arrow notation).
checkGADT :: ConLike
          -> [TyVar] -- ^ existentials
          -> [Type]  -- ^ argument types
          -> PatEnv
          -> TcM ()
checkGADT conlike ex_tvs arg_tys = \case
  PE { pe_ctxt = LetPat {} }
    -> return ()
  PE { pe_ctxt = LamPat (ArrowMatchCtxt {}) }
    | not $ isVanillaConLike conlike
    -- See Note [Arrows and patterns]
    -> failWithTc TcRnArrowProcGADTPattern
  PE { pe_lazy = True }
    | has_existentials
    -- See Note [Existential check]
    -> failWithTc TcRnLazyGADTPattern
  _ -> return ()
  where
    has_existentials :: Bool
    has_existentials = any (`elemVarSet` tyCoVarsOfTypes arg_tys) ex_tvs
