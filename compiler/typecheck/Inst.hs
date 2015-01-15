{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The @Inst@ type: dictionaries or method instances
-}

{-# LANGUAGE CPP #-}

module Inst (
       deeplySkolemise,
       deeplyInstantiate, instCall, instStupidTheta,
       emitWanted, emitWanteds,

       newOverloadedLit, mkOverLit,

       newClsInst,
       tcGetInsts, tcGetInstEnvs, getOverlapFlag,
       tcExtendLocalInstEnv, instCallConstraints, newMethodFromName,
       tcSyntaxName,

       -- Simple functions over evidence variables
       tyVarsOfWC, tyVarsOfBag,
       tyVarsOfCt, tyVarsOfCts,
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}   TcExpr( tcPolyExpr, tcSyntaxOp )
import {-# SOURCE #-}   TcUnify( unifyType )

import FastString
import HsSyn
import TcHsSyn
import TcRnMonad
import TcEnv
import TcEvidence
import InstEnv
import FunDeps
import TcMType
import Type
import Coercion ( Role(..) )
import TcType
import HscTypes
import Class( Class )
import MkId( mkDictFunId )
import Id
import Name
import Var      ( EvVar )
import VarEnv
import VarSet
import PrelNames
import SrcLoc
import DynFlags
import Bag
import Util
import Outputable
import Control.Monad( unless )
import Data.Maybe( isJust )

{-
************************************************************************
*                                                                      *
                Emitting constraints
*                                                                      *
************************************************************************
-}

emitWanteds :: CtOrigin -> TcThetaType -> TcM [EvVar]
emitWanteds origin theta = mapM (emitWanted origin) theta

emitWanted :: CtOrigin -> TcPredType -> TcM EvVar
emitWanted origin pred
  = do { loc <- getCtLoc origin
       ; ev  <- newEvVar pred
       ; emitSimple $ mkNonCanonical $
             CtWanted { ctev_pred = pred, ctev_evar = ev, ctev_loc = loc }
       ; return ev }

newMethodFromName :: CtOrigin -> Name -> TcRhoType -> TcM (HsExpr TcId)
-- Used when Name is the wired-in name for a wired-in class method,
-- so the caller knows its type for sure, which should be of form
--    forall a. C a => <blah>
-- newMethodFromName is supposed to instantiate just the outer
-- type variable and constraint

newMethodFromName origin name inst_ty
  = do { id <- tcLookupId name
              -- Use tcLookupId not tcLookupGlobalId; the method is almost
              -- always a class op, but with -XRebindableSyntax GHC is
              -- meant to find whatever thing is in scope, and that may
              -- be an ordinary function.

       ; let (tvs, theta, _caller_knows_this) = tcSplitSigmaTy (idType id)
             (the_tv:rest) = tvs
             subst = zipOpenTvSubst [the_tv] [inst_ty]

       ; wrap <- ASSERT( null rest && isSingleton theta )
                 instCall origin [inst_ty] (substTheta subst theta)
       ; return (mkHsWrap wrap (HsVar id)) }

{-
************************************************************************
*                                                                      *
        Deep instantiation and skolemisation
*                                                                      *
************************************************************************

Note [Deep skolemisation]
~~~~~~~~~~~~~~~~~~~~~~~~~
deeplySkolemise decomposes and skolemises a type, returning a type
with all its arrows visible (ie not buried under foralls)

Examples:

  deeplySkolemise (Int -> forall a. Ord a => blah)
    =  ( wp, [a], [d:Ord a], Int -> blah )
    where wp = \x:Int. /\a. \(d:Ord a). <hole> x

  deeplySkolemise  (forall a. Ord a => Maybe a -> forall b. Eq b => blah)
    =  ( wp, [a,b], [d1:Ord a,d2:Eq b], Maybe a -> blah )
    where wp = /\a.\(d1:Ord a).\(x:Maybe a)./\b.\(d2:Ord b). <hole> x

In general,
  if      deeplySkolemise ty = (wrap, tvs, evs, rho)
    and   e :: rho
  then    wrap e :: ty
    and   'wrap' binds tvs, evs

ToDo: this eta-abstraction plays fast and loose with termination,
      because it can introduce extra lambdas.  Maybe add a `seq` to
      fix this
-}

deeplySkolemise
  :: TcSigmaType
  -> TcM (HsWrapper, [TyVar], [EvVar], TcRhoType)

deeplySkolemise ty
  | Just (arg_tys, tvs, theta, ty') <- tcDeepSplitSigmaTy_maybe ty
  = do { ids1 <- newSysLocalIds (fsLit "dk") arg_tys
       ; (subst, tvs1) <- tcInstSkolTyVars tvs
       ; ev_vars1 <- newEvVars (substTheta subst theta)
       ; (wrap, tvs2, ev_vars2, rho) <- deeplySkolemise (substTy subst ty')
       ; return ( mkWpLams ids1
                   <.> mkWpTyLams tvs1
                   <.> mkWpLams ev_vars1
                   <.> wrap
                   <.> mkWpEvVarApps ids1
                , tvs1     ++ tvs2
                , ev_vars1 ++ ev_vars2
                , mkFunTys arg_tys rho ) }

  | otherwise
  = return (idHsWrapper, [], [], ty)

deeplyInstantiate :: CtOrigin -> TcSigmaType -> TcM (HsWrapper, TcRhoType)
--   Int -> forall a. a -> a  ==>  (\x:Int. [] x alpha) :: Int -> alpha
-- In general if
-- if    deeplyInstantiate ty = (wrap, rho)
-- and   e :: ty
-- then  wrap e :: rho

deeplyInstantiate orig ty
  | Just (arg_tys, tvs, theta, rho) <- tcDeepSplitSigmaTy_maybe ty
  = do { (subst, tvs') <- tcInstTyVars tvs
       ; ids1  <- newSysLocalIds (fsLit "di") (substTys subst arg_tys)
       ; let theta' = substTheta subst theta
       ; wrap1 <- instCall orig (mkTyVarTys tvs') theta'
       ; traceTc "Instantiating (deeply)" (vcat [ text "origin" <+> pprCtOrigin orig
                                                , text "type" <+> ppr ty
                                                , text "with" <+> ppr tvs'
                                                , text "args:" <+> ppr ids1
                                                , text "theta:" <+>  ppr theta' ])
       ; (wrap2, rho2) <- deeplyInstantiate orig (substTy subst rho)
       ; return (mkWpLams ids1
                    <.> wrap2
                    <.> wrap1
                    <.> mkWpEvVarApps ids1,
                 mkFunTys arg_tys rho2) }

  | otherwise = return (idHsWrapper, ty)

{-
************************************************************************
*                                                                      *
            Instantiating a call
*                                                                      *
************************************************************************
-}

----------------
instCall :: CtOrigin -> [TcType] -> TcThetaType -> TcM HsWrapper
-- Instantiate the constraints of a call
--      (instCall o tys theta)
-- (a) Makes fresh dictionaries as necessary for the constraints (theta)
-- (b) Throws these dictionaries into the LIE
-- (c) Returns an HsWrapper ([.] tys dicts)

instCall orig tys theta
  = do  { dict_app <- instCallConstraints orig theta
        ; return (dict_app <.> mkWpTyApps tys) }

----------------
instCallConstraints :: CtOrigin -> TcThetaType -> TcM HsWrapper
-- Instantiates the TcTheta, puts all constraints thereby generated
-- into the LIE, and returns a HsWrapper to enclose the call site.

instCallConstraints orig preds
  | null preds
  = return idHsWrapper
  | otherwise
  = do { evs <- mapM go preds
       ; traceTc "instCallConstraints" (ppr evs)
       ; return (mkWpEvApps evs) }
  where
    go pred
     | Just (Nominal, ty1, ty2) <- getEqPredTys_maybe pred -- Try short-cut
     = do  { co <- unifyType ty1 ty2
           ; return (EvCoercion co) }
     | otherwise
     = do { ev_var <- emitWanted modified_orig pred
          ; return (EvId ev_var) }
      where
        -- Coercible constraints appear as normal class constraints, but
        -- are aggressively canonicalized and manipulated during solving.
        -- The final equality to solve may barely resemble the initial
        -- constraint. Here, we remember the initial constraint in a
        -- CtOrigin for better error messages. It's perhaps worthwhile
        -- considering making this approach general, for other class
        -- constraints, too.
        modified_orig
          | Just (Representational, ty1, ty2) <- getEqPredTys_maybe pred
          = CoercibleOrigin ty1 ty2
          | otherwise
          = orig

----------------
instStupidTheta :: CtOrigin -> TcThetaType -> TcM ()
-- Similar to instCall, but only emit the constraints in the LIE
-- Used exclusively for the 'stupid theta' of a data constructor
instStupidTheta orig theta
  = do  { _co <- instCallConstraints orig theta -- Discard the coercion
        ; return () }

{-
************************************************************************
*                                                                      *
                Literals
*                                                                      *
************************************************************************

In newOverloadedLit we convert directly to an Int or Integer if we
know that's what we want.  This may save some time, by not
temporarily generating overloaded literals, but it won't catch all
cases (the rest are caught in lookupInst).
-}

newOverloadedLit :: CtOrigin
                 -> HsOverLit Name
                 -> TcRhoType
                 -> TcM (HsOverLit TcId)
newOverloadedLit orig lit res_ty
    = do dflags <- getDynFlags
         newOverloadedLit' dflags orig lit res_ty

newOverloadedLit' :: DynFlags
                  -> CtOrigin
                  -> HsOverLit Name
                  -> TcRhoType
                  -> TcM (HsOverLit TcId)
newOverloadedLit' dflags orig
  lit@(OverLit { ol_val = val, ol_rebindable = rebindable
               , ol_witness = meth_name }) res_ty

  | not rebindable
  , Just expr <- shortCutLit dflags val res_ty
        -- Do not generate a LitInst for rebindable syntax.
        -- Reason: If we do, tcSimplify will call lookupInst, which
        --         will call tcSyntaxName, which does unification,
        --         which tcSimplify doesn't like
  = return (lit { ol_witness = expr, ol_type = res_ty
                , ol_rebindable = rebindable })

  | otherwise
  = do  { hs_lit <- mkOverLit val
        ; let lit_ty = hsLitType hs_lit
        ; fi' <- tcSyntaxOp orig meth_name (mkFunTy lit_ty res_ty)
                -- Overloaded literals must have liftedTypeKind, because
                -- we're instantiating an overloaded function here,
                -- whereas res_ty might be openTypeKind. This was a bug in 6.2.2
                -- However this'll be picked up by tcSyntaxOp if necessary
        ; let witness = HsApp (noLoc fi') (noLoc (HsLit hs_lit))
        ; return (lit { ol_witness = witness, ol_type = res_ty
                      , ol_rebindable = rebindable }) }

------------
mkOverLit :: OverLitVal -> TcM HsLit
mkOverLit (HsIntegral src i)
  = do  { integer_ty <- tcMetaTy integerTyConName
        ; return (HsInteger src i integer_ty) }

mkOverLit (HsFractional r)
  = do  { rat_ty <- tcMetaTy rationalTyConName
        ; return (HsRat r rat_ty) }

mkOverLit (HsIsString src s) = return (HsString src s)

{-
************************************************************************
*                                                                      *
                Re-mappable syntax

     Used only for arrow syntax -- find a way to nuke this
*                                                                      *
************************************************************************

Suppose we are doing the -XRebindableSyntax thing, and we encounter
a do-expression.  We have to find (>>) in the current environment, which is
done by the rename. Then we have to check that it has the same type as
Control.Monad.(>>).  Or, more precisely, a compatible type. One 'customer' had
this:

  (>>) :: HB m n mn => m a -> n b -> mn b

So the idea is to generate a local binding for (>>), thus:

        let then72 :: forall a b. m a -> m b -> m b
            then72 = ...something involving the user's (>>)...
        in
        ...the do-expression...

Now the do-expression can proceed using then72, which has exactly
the expected type.

In fact tcSyntaxName just generates the RHS for then72, because we only
want an actual binding in the do-expression case. For literals, we can
just use the expression inline.
-}

tcSyntaxName :: CtOrigin
             -> TcType                  -- Type to instantiate it at
             -> (Name, HsExpr Name)     -- (Standard name, user name)
             -> TcM (Name, HsExpr TcId) -- (Standard name, suitable expression)
-- USED ONLY FOR CmdTop (sigh) ***
-- See Note [CmdSyntaxTable] in HsExpr

tcSyntaxName orig ty (std_nm, HsVar user_nm)
  | std_nm == user_nm
  = do rhs <- newMethodFromName orig std_nm ty
       return (std_nm, rhs)

tcSyntaxName orig ty (std_nm, user_nm_expr) = do
    std_id <- tcLookupId std_nm
    let
        -- C.f. newMethodAtLoc
        ([tv], _, tau)  = tcSplitSigmaTy (idType std_id)
        sigma1          = substTyWith [tv] [ty] tau
        -- Actually, the "tau-type" might be a sigma-type in the
        -- case of locally-polymorphic methods.

    addErrCtxtM (syntaxNameCtxt user_nm_expr orig sigma1) $ do

        -- Check that the user-supplied thing has the
        -- same type as the standard one.
        -- Tiresome jiggling because tcCheckSigma takes a located expression
     span <- getSrcSpanM
     expr <- tcPolyExpr (L span user_nm_expr) sigma1
     return (std_nm, unLoc expr)

syntaxNameCtxt :: HsExpr Name -> CtOrigin -> Type -> TidyEnv
               -> TcRn (TidyEnv, SDoc)
syntaxNameCtxt name orig ty tidy_env
  = do { inst_loc <- getCtLoc orig
       ; let msg = vcat [ ptext (sLit "When checking that") <+> quotes (ppr name)
                          <+> ptext (sLit "(needed by a syntactic construct)")
                        , nest 2 (ptext (sLit "has the required type:")
                                  <+> ppr (tidyType tidy_env ty))
                        , nest 2 (pprArisingAt inst_loc) ]
       ; return (tidy_env, msg) }

{-
************************************************************************
*                                                                      *
                Instances
*                                                                      *
************************************************************************
-}

getOverlapFlag :: Maybe OverlapMode -> TcM OverlapFlag
getOverlapFlag overlap_mode
  = do  { dflags <- getDynFlags
        ; let overlap_ok    = xopt Opt_OverlappingInstances dflags
              incoherent_ok = xopt Opt_IncoherentInstances  dflags
              use x = OverlapFlag { isSafeOverlap = safeLanguageOn dflags
                                  , overlapMode   = x }
              default_oflag | incoherent_ok = use (Incoherent "")
                            | overlap_ok    = use (Overlaps "")
                            | otherwise     = use (NoOverlap "")

              final_oflag = setOverlapModeMaybe default_oflag overlap_mode
        ; return final_oflag }

tcGetInsts :: TcM [ClsInst]
-- Gets the local class instances.
tcGetInsts = fmap tcg_insts getGblEnv

newClsInst :: Maybe OverlapMode -> Name -> [TyVar] -> ThetaType
           -> Class -> [Type] -> TcM ClsInst
newClsInst overlap_mode dfun_name tvs theta clas tys
  = do { (subst, tvs') <- freshenTyVarBndrs tvs
             -- Be sure to freshen those type variables,
             -- so they are sure not to appear in any lookup
       ; let tys'   = substTys subst tys
             theta' = substTheta subst theta
             dfun   = mkDictFunId dfun_name tvs' theta' clas tys'
             -- Substituting in the DFun type just makes sure that
             -- we are using TyVars rather than TcTyVars
             -- Not sure if this is really the right place to do so,
             -- but it'll do fine
       ; oflag <- getOverlapFlag overlap_mode
       ; return (mkLocalInstance dfun oflag tvs' clas tys') }

tcExtendLocalInstEnv :: [ClsInst] -> TcM a -> TcM a
  -- Add new locally-defined instances
tcExtendLocalInstEnv dfuns thing_inside
 = do { traceDFuns dfuns
      ; env <- getGblEnv
      ; (inst_env', cls_insts') <- foldlM addLocalInst
                                          (tcg_inst_env env, tcg_insts env)
                                          dfuns
      ; let env' = env { tcg_insts    = cls_insts'
                       , tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }

addLocalInst :: (InstEnv, [ClsInst]) -> ClsInst -> TcM (InstEnv, [ClsInst])
-- Check that the proposed new instance is OK,
-- and then add it to the home inst env
-- If overwrite_inst, then we can overwrite a direct match
addLocalInst (home_ie, my_insts) ispec
   = do {
         -- Instantiate the dfun type so that we extend the instance
         -- envt with completely fresh template variables
         -- This is important because the template variables must
         -- not overlap with anything in the things being looked up
         -- (since we do unification).
             --
             -- We use tcInstSkolType because we don't want to allocate fresh
             --  *meta* type variables.
             --
             -- We use UnkSkol --- and *not* InstSkol or PatSkol --- because
             -- these variables must be bindable by tcUnifyTys.  See
             -- the call to tcUnifyTys in InstEnv, and the special
             -- treatment that instanceBindFun gives to isOverlappableTyVar
             -- This is absurdly delicate.

             -- Load imported instances, so that we report
             -- duplicates correctly

             -- 'matches'  are existing instance declarations that are less
             --            specific than the new one
             -- 'dups'     are those 'matches' that are equal to the new one
         ; isGHCi <- getIsGHCi
         ; eps    <- getEps
         ; tcg_env <- getGblEnv

           -- In GHCi, we *override* any identical instances
           -- that are also defined in the interactive context
           -- See Note [Override identical instances in GHCi]
         ; let home_ie'
                 | isGHCi    = deleteFromInstEnv home_ie ispec
                 | otherwise = home_ie

               (_tvs, cls, tys) = instanceHead ispec
               -- If we're compiling sig-of and there's an external duplicate
               -- instance, silently ignore it (that's the instance we're
               -- implementing!)  NB: we still count local duplicate instances
               -- as errors.
               -- See Note [Signature files and type class instances]
               global_ie
                    | isJust (tcg_sig_of tcg_env) = emptyInstEnv
                    | otherwise = eps_inst_env eps
               inst_envs       = InstEnvs { ie_global  = global_ie
                                          , ie_local   = home_ie'
                                          , ie_visible = tcg_visible_orphan_mods tcg_env }
               (matches, _, _) = lookupInstEnv inst_envs cls tys
               dups            = filter (identicalClsInstHead ispec) (map fst matches)

             -- Check functional dependencies
         ; case checkFunDeps inst_envs ispec of
             Just specs -> funDepErr ispec specs
             Nothing    -> return ()

             -- Check for duplicate instance decls.
         ; unless (null dups) $
           dupInstErr ispec (head dups)

         ; return (extendInstEnv home_ie' ispec, ispec : my_insts) }

{-
Note [Signature files and type class instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instances in signature files do not have an effect when compiling:
when you compile a signature against an implementation, you will
see the instances WHETHER OR NOT the instance is declared in
the file (this is because the signatures go in the EPS and we
can't filter them out easily.)  This is also why we cannot
place the instance in the hi file: it would show up as a duplicate,
and we don't have instance reexports anyway.

However, you might find them useful when typechecking against
a signature: the instance is a way of indicating to GHC that
some instance exists, in case downstream code uses it.

Implementing this is a little tricky.  Consider the following
situation (sigof03):

 module A where
     instance C T where ...

 module ASig where
     instance C T

When compiling ASig, A.hi is loaded, which brings its instances
into the EPS.  When we process the instance declaration in ASig,
we should ignore it for the purpose of doing a duplicate check,
since it's not actually a duplicate. But don't skip the check
entirely, we still want this to fail (tcfail221):

 module ASig where
     instance C T
     instance C T

Note that in some situations, the interface containing the type
class instances may not have been loaded yet at all.  The usual
situation when A imports another module which provides the
instances (sigof02m):

 module A(module B) where
     import B

See also Note [Signature lazy interface loading].  We can't
rely on this, however, since sometimes we'll have spurious
type class instances in the EPS, see #9422 (sigof02dm)

************************************************************************
*                                                                      *
        Errors and tracing
*                                                                      *
************************************************************************
-}

traceDFuns :: [ClsInst] -> TcRn ()
traceDFuns ispecs
  = traceTc "Adding instances:" (vcat (map pp ispecs))
  where
    pp ispec = hang (ppr (instanceDFunId ispec) <+> colon)
                  2 (ppr ispec)
        -- Print the dfun name itself too

funDepErr :: ClsInst -> [ClsInst] -> TcRn ()
funDepErr ispec ispecs
  = addClsInstsErr (ptext (sLit "Functional dependencies conflict between instance declarations:"))
                    (ispec : ispecs)

dupInstErr :: ClsInst -> ClsInst -> TcRn ()
dupInstErr ispec dup_ispec
  = addClsInstsErr (ptext (sLit "Duplicate instance declarations:"))
                    [ispec, dup_ispec]

addClsInstsErr :: SDoc -> [ClsInst] -> TcRn ()
addClsInstsErr herald ispecs
  = setSrcSpan (getSrcSpan (head sorted)) $
    addErr (hang herald 2 (pprInstances sorted))
 where
   sorted = sortWith getSrcLoc ispecs
   -- The sortWith just arranges that instances are dislayed in order
   -- of source location, which reduced wobbling in error messages,
   -- and is better for users

{-
************************************************************************
*                                                                      *
        Simple functions over evidence variables
*                                                                      *
************************************************************************
-}

---------------- Getting free tyvars -------------------------
tyVarsOfCt :: Ct -> TcTyVarSet
tyVarsOfCt (CTyEqCan { cc_tyvar = tv, cc_rhs = xi })     = extendVarSet (tyVarsOfType xi) tv
tyVarsOfCt (CFunEqCan { cc_tyargs = tys, cc_fsk = fsk }) = extendVarSet (tyVarsOfTypes tys) fsk
tyVarsOfCt (CDictCan { cc_tyargs = tys })                = tyVarsOfTypes tys
tyVarsOfCt (CIrredEvCan { cc_ev = ev })                  = tyVarsOfType (ctEvPred ev)
tyVarsOfCt (CHoleCan { cc_ev = ev })                     = tyVarsOfType (ctEvPred ev)
tyVarsOfCt (CNonCanonical { cc_ev = ev })                = tyVarsOfType (ctEvPred ev)

tyVarsOfCts :: Cts -> TcTyVarSet
tyVarsOfCts = foldrBag (unionVarSet . tyVarsOfCt) emptyVarSet

tyVarsOfWC :: WantedConstraints -> TyVarSet
-- Only called on *zonked* things, hence no need to worry about flatten-skolems
tyVarsOfWC (WC { wc_simple = simple, wc_impl = implic, wc_insol = insol })
  = tyVarsOfCts simple `unionVarSet`
    tyVarsOfBag tyVarsOfImplic implic `unionVarSet`
    tyVarsOfCts insol

tyVarsOfImplic :: Implication -> TyVarSet
-- Only called on *zonked* things, hence no need to worry about flatten-skolems
tyVarsOfImplic (Implic { ic_skols = skols
                       , ic_given = givens, ic_wanted = wanted })
  = (tyVarsOfWC wanted `unionVarSet` tyVarsOfTypes (map evVarPred givens))
    `delVarSetList` skols

tyVarsOfBag :: (a -> TyVarSet) -> Bag a -> TyVarSet
tyVarsOfBag tvs_of = foldrBag (unionVarSet . tvs_of) emptyVarSet
