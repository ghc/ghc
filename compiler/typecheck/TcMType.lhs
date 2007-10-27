%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Monadic type operations

This module contains monadic operations over types that contain
mutable type variables

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module TcMType (
  TcTyVar, TcKind, TcType, TcTauType, TcThetaType, TcTyVarSet,

  --------------------------------
  -- Creating new mutable type variables
  newFlexiTyVar,
  newFlexiTyVarTy,		-- Kind -> TcM TcType
  newFlexiTyVarTys,		-- Int -> Kind -> TcM [TcType]
  newKindVar, newKindVars, 
  lookupTcTyVar, LookupTyVarResult(..),

  newMetaTyVar, readMetaTyVar, writeMetaTyVar, isFilledMetaTyVar,

  --------------------------------
  -- Boxy type variables
  newBoxyTyVar, newBoxyTyVars, newBoxyTyVarTys, readFilledBox, 

  --------------------------------
  -- Creating new coercion variables
  newCoVars, newMetaCoVar,

  --------------------------------
  -- Instantiation
  tcInstTyVar, tcInstType, tcInstTyVars, tcInstBoxyTyVar,
  tcInstSigTyVars,
  tcInstSkolTyVar, tcInstSkolTyVars, tcInstSkolType, 
  tcSkolSigType, tcSkolSigTyVars, occurCheckErr,

  --------------------------------
  -- Checking type validity
  Rank, UserTypeCtxt(..), checkValidType, 
  SourceTyCtxt(..), checkValidTheta, checkFreeness,
  checkValidInstHead, checkValidInstance, 
  checkInstTermination, checkValidTypeInst, checkTyFamFreeness,
  checkUpdateMeta, updateMeta, checkTauTvUpdate, fillBoxWithTau, unifyKindCtxt,
  unifyKindMisMatch, validDerivPred, arityErr, notMonoType, notMonoArgs,

  --------------------------------
  -- Zonking
  zonkType, zonkTcPredType, 
  zonkTcTyVar, zonkTcTyVars, zonkTcTyVarsAndFV, zonkSigTyVar,
  zonkQuantifiedTyVar, zonkQuantifiedTyVars,
  zonkTcType, zonkTcTypes, zonkTcClassConstraints, zonkTcThetaType,
  zonkTcKindToKind, zonkTcKind, zonkTopTyVar,

  readKindVar, writeKindVar
  ) where

#include "HsVersions.h"

-- friends:
import TypeRep
import TcType
import Type
import Coercion
import Class
import TyCon
import Var

-- others:
import TcRnMonad          -- TcType, amongst others
import FunDeps
import Name
import VarSet
import ErrUtils
import DynFlags
import Util
import Maybes
import ListSetOps
import UniqSupply
import SrcLoc
import Outputable

import Control.Monad	( when, unless )
import Data.List	( (\\) )
\end{code}


%************************************************************************
%*									*
	Instantiation in general
%*									*
%************************************************************************

\begin{code}
tcInstType :: ([TyVar] -> TcM [TcTyVar]) 		-- How to instantiate the type variables
	   -> TcType 					-- Type to instantiate
	   -> TcM ([TcTyVar], TcThetaType, TcType)	-- Result
tcInstType inst_tyvars ty
  = case tcSplitForAllTys ty of
	([],     rho) -> let	-- There may be overloading despite no type variables;
				-- 	(?x :: Int) => Int -> Int
			   (theta, tau) = tcSplitPhiTy rho
			 in
			 return ([], theta, tau)

	(tyvars, rho) -> do { tyvars' <- inst_tyvars tyvars

			    ; let  tenv = zipTopTvSubst tyvars (mkTyVarTys tyvars')
				-- Either the tyvars are freshly made, by inst_tyvars,
				-- or (in the call from tcSkolSigType) any nested foralls
				-- have different binders.  Either way, zipTopTvSubst is ok

			    ; let  (theta, tau) = tcSplitPhiTy (substTy tenv rho)
			    ; return (tyvars', theta, tau) }
\end{code}


%************************************************************************
%*									*
	Updating tau types
%*									*
%************************************************************************

Can't be in TcUnify, as we also need it in TcTyFuns.

\begin{code}
type SwapFlag = Bool
	-- False <=> the two args are (actual, expected) respectively
	-- True  <=> the two args are (expected, actual) respectively

checkUpdateMeta :: SwapFlag
	        -> TcTyVar -> IORef MetaDetails -> TcType -> TcM ()
-- Update tv1, which is flexi; occurs check is alrady done
-- The 'check' version does a kind check too
-- We do a sub-kind check here: we might unify (a b) with (c d) 
--	where b::*->* and d::*; this should fail

checkUpdateMeta swapped tv1 ref1 ty2
  = do	{ checkKinds swapped tv1 ty2
	; updateMeta tv1 ref1 ty2 }

updateMeta :: TcTyVar -> IORef MetaDetails -> TcType -> TcM ()
updateMeta tv1 ref1 ty2
  = ASSERT( isMetaTyVar tv1 )
    ASSERT( isBoxyTyVar tv1 || isTauTy ty2 )
    do	{ ASSERTM2( do { details <- readMetaTyVar tv1; return (isFlexi details) }, ppr tv1 )
	; traceTc (text "updateMeta" <+> ppr tv1 <+> text ":=" <+> ppr ty2)
	; writeMutVar ref1 (Indirect ty2) 
	}

----------------
checkKinds swapped tv1 ty2
-- We're about to unify a type variable tv1 with a non-tyvar-type ty2.
-- ty2 has been zonked at this stage, which ensures that
-- its kind has as much boxity information visible as possible.
  | tk2 `isSubKind` tk1 = returnM ()

  | otherwise
	-- Either the kinds aren't compatible
	--	(can happen if we unify (a b) with (c d))
	-- or we are unifying a lifted type variable with an
	-- 	unlifted type: e.g.  (id 3#) is illegal
  = addErrCtxtM (unifyKindCtxt swapped tv1 ty2)	$
    unifyKindMisMatch k1 k2
  where
    (k1,k2) | swapped   = (tk2,tk1)
	    | otherwise = (tk1,tk2)
    tk1 = tyVarKind tv1
    tk2 = typeKind ty2

----------------
checkTauTvUpdate :: TcTyVar -> TcType -> TcM (Maybe TcType)
--    (checkTauTvUpdate tv ty)
-- We are about to update the TauTv tv with ty.
-- Check (a) that tv doesn't occur in ty (occurs check)
--	 (b) that ty is a monotype
-- Furthermore, in the interest of (b), if you find an
-- empty box (BoxTv that is Flexi), fill it in with a TauTv
-- 
-- We have three possible outcomes:
-- (1) Return the (non-boxy) type to update the type variable with, 
--     [we know the update is ok!]
-- (2) return Nothing, or 
--     [we cannot tell whether the update is ok right now]
-- (3) fails.
--     [the update is definitely invalid]
-- We return Nothing in case the tv occurs in ty *under* a type family
-- application.  In this case, we must not update tv (to avoid a cyclic type
-- term), but we also cannot fail claiming an infinite type.  Given
--   type family F a
--   type instance F Int = Int
-- consider
--   a ~ F a
-- This is perfectly reasonable, if we later get a ~ Int.

checkTauTvUpdate orig_tv orig_ty
  = do { result <- go orig_ty
       ; case result of 
           Right ty    -> return $ Just ty
           Left  True  -> return $ Nothing
           Left  False -> occurCheckErr (mkTyVarTy orig_tv) orig_ty
       }
  where
    go :: TcType -> TcM (Either Bool TcType)
    -- go returns
    --   Right ty    if everything is fine
    --   Left True   if orig_tv occurs in orig_ty, but under a type family app
    --   Left False  if orig_tv occurs in orig_ty (with no type family app)
    -- It fails if it encounters a forall type, except as an argument for a
    -- closed type synonym that expands to a tau type.
    go (TyConApp tc tys)
	| isSynTyCon tc  = go_syn tc tys
	| otherwise	 = do { tys' <- mappM go tys
                              ; return $ occurs (TyConApp tc) tys' }
    go (NoteTy _ ty2) 	 = go ty2	-- Discard free-tyvar annotations
    go (PredTy p)	 = do { p' <- go_pred p
                              ; return $ occurs1 PredTy p' }
    go (FunTy arg res)   = do { arg' <- go arg
                              ; res' <- go res
                              ; return $ occurs2 FunTy arg' res' }
    go (AppTy fun arg)	 = do { fun' <- go fun
                              ; arg' <- go arg
                              ; return $ occurs2 mkAppTy fun' arg' }
		-- NB the mkAppTy; we might have instantiated a
		-- type variable to a type constructor, so we need
		-- to pull the TyConApp to the top.
    go (ForAllTy tv ty) = notMonoType orig_ty		-- (b)

    go (TyVarTy tv)
	| orig_tv == tv = return $ Left False           -- (a)
	| isTcTyVar tv  = go_tyvar tv (tcTyVarDetails tv)
	| otherwise     = return $ Right (TyVarTy tv)
		 -- Ordinary (non Tc) tyvars
		 -- occur inside quantified types

    go_pred (ClassP c tys) = do { tys' <- mapM go tys
                                ; return $ occurs (ClassP c) tys' }
    go_pred (IParam n ty)  = do { ty' <- go ty
                                ; return $ occurs1 (IParam n) ty' }
    go_pred (EqPred t1 t2) = do { t1' <- go t1
                                ; t2' <- go t2
                                ; return $ occurs2 EqPred t1' t2' }

    go_tyvar tv (SkolemTv _) = return $ Right (TyVarTy tv)
    go_tyvar tv (MetaTv box ref)
	= do { cts <- readMutVar ref
	     ; case cts of
		  Indirect ty -> go ty 
		  Flexi -> case box of
				BoxTv -> do { ty <- fillBoxWithTau tv ref
                                            ; return $ Right ty }
				other -> return $ Right (TyVarTy tv)
	     }

	-- go_syn is called for synonyms only
	-- See Note [Type synonyms and the occur check]
    go_syn tc tys
	| not (isTauTyCon tc)
	= notMonoType orig_ty	-- (b) again
	| otherwise
	= do { (msgs, mb_tys') <- tryTc (mapM go tys)
	     ; case mb_tys' of

                -- we had a type error => forall in type parameters
		Nothing 
                  | isOpenTyCon tc -> notMonoArgs (TyConApp tc tys)
		        -- Synonym families must have monotype args
		  | otherwise      -> go (expectJust "checkTauTvUpdate(1)" 
					    (tcView (TyConApp tc tys)))
		        -- Try again, expanding the synonym

                -- no type error, but need to test whether occurs check happend
		Just tys' -> 
                  case occurs id tys' of
                    Left _ 
                      | isOpenTyCon tc -> return $ Left True
                        -- Variable occured under type family application
                      | otherwise      -> go (expectJust "checkTauTvUpdate(2)" 
					       (tcView (TyConApp tc tys)))
		        -- Try again, expanding the synonym
                    Right raw_tys'     -> return $ Right (TyConApp tc raw_tys')
		        -- Retain the synonym (the common case)
	     }

    -- Left results (= occurrence of orig_ty) dominate and
    -- (Left False) (= fatal occurrence) dominates over (Left True)
    occurs :: ([a] -> b) -> [Either Bool a] -> Either Bool b
    occurs c = either Left (Right . c) . foldr combine (Right [])
      where
        combine (Left famInst1) (Left famInst2) = Left (famInst1 && famInst2)
        combine (Right _      ) (Left famInst)  = Left famInst
        combine (Left famInst)  (Right _)       = Left famInst
        combine (Right arg)     (Right args)    = Right (arg:args)

    occurs1 c x   = occurs (\[x']     -> c x')    [x]
    occurs2 c x y = occurs (\[x', y'] -> c x' y') [x, y]

fillBoxWithTau :: BoxyTyVar -> IORef MetaDetails -> TcM TcType
-- (fillBoxWithTau tv ref) fills ref with a freshly allocated 
--  tau-type meta-variable, whose print-name is the same as tv
-- Choosing the same name is good: when we instantiate a function
-- we allocate boxy tyvars with the same print-name as the quantified
-- tyvar; and then we often fill the box with a tau-tyvar, and again
-- we want to choose the same name.
fillBoxWithTau tv ref 
  = do	{ tv' <- tcInstTyVar tv		-- Do not gratuitously forget
	; let tau = mkTyVarTy tv'	-- name of the type variable
	; writeMutVar ref (Indirect tau)
	; return tau }
\end{code}

Note [Type synonyms and the occur check]
~~~~~~~~~~~~~~~~~~~~
Basically we want to update     tv1 := ps_ty2
because ps_ty2 has type-synonym info, which improves later error messages

But consider 
	type A a = ()

	f :: (A a -> a -> ()) -> ()
	f = \ _ -> ()

	x :: ()
	x = f (\ x p -> p x)

In the application (p x), we try to match "t" with "A t".  If we go
ahead and bind t to A t (= ps_ty2), we'll lead the type checker into 
an infinite loop later.
But we should not reject the program, because A t = ().
Rather, we should bind t to () (= non_var_ty2).

--------------

Error mesages in case of kind mismatch.

\begin{code}
unifyKindMisMatch ty1 ty2
  = zonkTcKind ty1	`thenM` \ ty1' ->
    zonkTcKind ty2	`thenM` \ ty2' ->
    let
	msg = hang (ptext SLIT("Couldn't match kind"))
		   2 (sep [quotes (ppr ty1'), 
			   ptext SLIT("against"), 
			   quotes (ppr ty2')])
    in
    failWithTc msg

unifyKindCtxt swapped tv1 ty2 tidy_env	-- not swapped => tv1 expected, ty2 inferred
	-- tv1 and ty2 are zonked already
  = returnM msg
  where
    msg = (env2, ptext SLIT("When matching the kinds of") <+> 
		 sep [quotes pp_expected <+> ptext SLIT("and"), quotes pp_actual])

    (pp_expected, pp_actual) | swapped   = (pp2, pp1)
		             | otherwise = (pp1, pp2)
    (env1, tv1') = tidyOpenTyVar tidy_env tv1
    (env2, ty2') = tidyOpenType  env1 ty2
    pp1 = ppr tv1' <+> dcolon <+> ppr (tyVarKind tv1)
    pp2 = ppr ty2' <+> dcolon <+> ppr (typeKind ty2)
\end{code}

Error message for failure due to an occurs check.

\begin{code}
occurCheckErr :: TcType -> TcType -> TcM a
occurCheckErr ty containingTy
  = do	{ env0 <- tcInitTidyEnv
	; ty'           <- zonkTcType ty
	; containingTy' <- zonkTcType containingTy
	; let (env1, tidy_ty1) = tidyOpenType env0 ty'
	      (env2, tidy_ty2) = tidyOpenType env1 containingTy'
	      extra = sep [ppr tidy_ty1, char '=', ppr tidy_ty2]
	; failWithTcM (env2, hang msg 2 extra) }
  where
    msg = ptext SLIT("Occurs check: cannot construct the infinite type:")
\end{code}

%************************************************************************
%*									*
	Kind variables
%*									*
%************************************************************************

\begin{code}
newCoVars :: [(TcType,TcType)] -> TcM [CoVar]
newCoVars spec
  = do	{ us <- newUniqueSupply 
	; return [ mkCoVar (mkSysTvName uniq FSLIT("co"))
			   (mkCoKind ty1 ty2)
		 | ((ty1,ty2), uniq) <- spec `zip` uniqsFromSupply us] }

newMetaCoVar :: TcType -> TcType -> TcM TcTyVar
newMetaCoVar ty1 ty2 = newMetaTyVar TauTv (mkCoKind ty1 ty2)

newKindVar :: TcM TcKind
newKindVar = do	{ uniq <- newUnique
		; ref <- newMutVar Flexi
		; return (mkTyVarTy (mkKindVar uniq ref)) }

newKindVars :: Int -> TcM [TcKind]
newKindVars n = mappM (\ _ -> newKindVar) (nOfThem n ())
\end{code}


%************************************************************************
%*									*
	SkolemTvs (immutable)
%*									*
%************************************************************************

\begin{code}
mkSkolTyVar :: Name -> Kind -> SkolemInfo -> TcTyVar
mkSkolTyVar name kind info = mkTcTyVar name kind (SkolemTv info)

tcSkolSigType :: SkolemInfo -> Type -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type signature with skolem constants, but 
-- do *not* give them fresh names, because we want the name to
-- be in the type environment -- it is lexically scoped.
tcSkolSigType info ty = tcInstType (\tvs -> return (tcSkolSigTyVars info tvs)) ty

tcSkolSigTyVars :: SkolemInfo -> [TyVar] -> [TcTyVar]
-- Make skolem constants, but do *not* give them new names, as above
tcSkolSigTyVars info tyvars = [ mkSkolTyVar (tyVarName tv) (tyVarKind tv) info
			      | tv <- tyvars ]

tcInstSkolTyVar :: SkolemInfo -> Maybe SrcSpan -> TyVar -> TcM TcTyVar
-- Instantiate the tyvar, using 
--	* the occ-name and kind of the supplied tyvar, 
--	* the unique from the monad,
--	* the location either from the tyvar (mb_loc = Nothing)
--	  or from mb_loc (Just loc)
tcInstSkolTyVar info mb_loc tyvar
  = do	{ uniq <- newUnique
	; let old_name = tyVarName tyvar
	      kind     = tyVarKind tyvar
	      loc      = mb_loc `orElse` getSrcSpan old_name
	      new_name = mkInternalName uniq (nameOccName old_name) loc
	; return (mkSkolTyVar new_name kind info) }

tcInstSkolTyVars :: SkolemInfo -> [TyVar] -> TcM [TcTyVar]
-- Get the location from the monad
tcInstSkolTyVars info tyvars 
  = do 	{ span <- getSrcSpanM
	; mapM (tcInstSkolTyVar info (Just span)) tyvars }

tcInstSkolType :: SkolemInfo -> TcType -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type with fresh skolem constants
-- Binding location comes from the monad
tcInstSkolType info ty = tcInstType (tcInstSkolTyVars info) ty
\end{code}


%************************************************************************
%*									*
	MetaTvs (meta type variables; mutable)
%*									*
%************************************************************************

\begin{code}
newMetaTyVar :: BoxInfo -> Kind -> TcM TcTyVar
-- Make a new meta tyvar out of thin air
newMetaTyVar box_info kind
  = do	{ uniq <- newUnique
 	; ref <- newMutVar Flexi
	; let name = mkSysTvName uniq fs 
	      fs = case box_info of
			BoxTv   -> FSLIT("t")
			TauTv   -> FSLIT("t")
			SigTv _ -> FSLIT("a")
		-- We give BoxTv and TauTv the same string, because
		-- otherwise we get user-visible differences in error
		-- messages, which are confusing.  If you want to see
		-- the box_info of each tyvar, use -dppr-debug
	; return (mkTcTyVar name kind (MetaTv box_info ref)) }

instMetaTyVar :: BoxInfo -> TyVar -> TcM TcTyVar
-- Make a new meta tyvar whose Name and Kind 
-- come from an existing TyVar
instMetaTyVar box_info tyvar
  = do	{ uniq <- newUnique
 	; ref <- newMutVar Flexi
	; let name = setNameUnique (tyVarName tyvar) uniq
	      kind = tyVarKind tyvar
	; return (mkTcTyVar name kind (MetaTv box_info ref)) }

readMetaTyVar :: TyVar -> TcM MetaDetails
readMetaTyVar tyvar = ASSERT2( isMetaTyVar tyvar, ppr tyvar )
		      readMutVar (metaTvRef tyvar)

isFilledMetaTyVar :: TyVar -> TcM Bool
-- True of a filled-in (Indirect) meta type variable
isFilledMetaTyVar tv
  | not (isTcTyVar tv) = return False
  | MetaTv _ ref <- tcTyVarDetails tv
  = do 	{ details <- readMutVar ref
	; return (isIndirect details) }
  | otherwise = return False

writeMetaTyVar :: TcTyVar -> TcType -> TcM ()
#ifndef DEBUG
writeMetaTyVar tyvar ty = writeMutVar (metaTvRef tyvar) (Indirect ty)
#else
writeMetaTyVar tyvar ty
  | not (isMetaTyVar tyvar)
  = pprTrace "writeMetaTyVar" (ppr tyvar) $
    returnM ()

  | otherwise
  = ASSERT( isMetaTyVar tyvar )
    -- TOM: It should also work for coercions
    -- ASSERT2( k2 `isSubKind` k1, (ppr tyvar <+> ppr k1) $$ (ppr ty <+> ppr k2) )
    do	{ ASSERTM2( do { details <- readMetaTyVar tyvar; return (isFlexi details) }, ppr tyvar )
	; writeMutVar (metaTvRef tyvar) (Indirect ty) }
  where
    k1 = tyVarKind tyvar
    k2 = typeKind ty
#endif
\end{code}


%************************************************************************
%*									*
	MetaTvs: TauTvs
%*									*
%************************************************************************

\begin{code}
newFlexiTyVar :: Kind -> TcM TcTyVar
newFlexiTyVar kind = newMetaTyVar TauTv kind

newFlexiTyVarTy  :: Kind -> TcM TcType
newFlexiTyVarTy kind
  = newFlexiTyVar kind	`thenM` \ tc_tyvar ->
    returnM (TyVarTy tc_tyvar)

newFlexiTyVarTys :: Int -> Kind -> TcM [TcType]
newFlexiTyVarTys n kind = mappM newFlexiTyVarTy (nOfThem n kind)

tcInstTyVar :: TyVar -> TcM TcTyVar
-- Instantiate with a META type variable
tcInstTyVar tyvar = instMetaTyVar TauTv tyvar

tcInstTyVars :: [TyVar] -> TcM ([TcTyVar], [TcType], TvSubst)
-- Instantiate with META type variables
tcInstTyVars tyvars
  = do	{ tc_tvs <- mapM tcInstTyVar tyvars
	; let tys = mkTyVarTys tc_tvs
	; returnM (tc_tvs, tys, zipTopTvSubst tyvars tys) }
		-- Since the tyvars are freshly made,
		-- they cannot possibly be captured by
		-- any existing for-alls.  Hence zipTopTvSubst
\end{code}


%************************************************************************
%*									*
	MetaTvs: SigTvs
%*									*
%************************************************************************

\begin{code}
tcInstSigTyVars :: Bool -> SkolemInfo -> [TyVar] -> TcM [TcTyVar]
-- Instantiate with skolems or meta SigTvs; depending on use_skols
-- Always take location info from the supplied tyvars
tcInstSigTyVars use_skols skol_info tyvars 
  | use_skols
  = mapM (tcInstSkolTyVar skol_info Nothing) tyvars

  | otherwise
  = mapM (instMetaTyVar (SigTv skol_info)) tyvars

zonkSigTyVar :: TcTyVar -> TcM TcTyVar
zonkSigTyVar sig_tv 
  | isSkolemTyVar sig_tv 
  = return sig_tv	-- Happens in the call in TcBinds.checkDistinctTyVars
  | otherwise
  = ASSERT( isSigTyVar sig_tv )
    do { ty <- zonkTcTyVar sig_tv
       ; return (tcGetTyVar "zonkSigTyVar" ty) }
	-- 'ty' is bound to be a type variable, because SigTvs
	-- can only be unified with type variables
\end{code}


%************************************************************************
%*									*
	MetaTvs: BoxTvs
%*									*
%************************************************************************

\begin{code}
newBoxyTyVar :: Kind -> TcM BoxyTyVar
newBoxyTyVar kind = newMetaTyVar BoxTv kind

newBoxyTyVars :: [Kind] -> TcM [BoxyTyVar]
newBoxyTyVars kinds = mapM newBoxyTyVar kinds

newBoxyTyVarTys :: [Kind] -> TcM [BoxyType]
newBoxyTyVarTys kinds = do { tvs <- mapM newBoxyTyVar kinds; return (mkTyVarTys tvs) }

readFilledBox :: BoxyTyVar -> TcM TcType
-- Read the contents of the box, which should be filled in by now
readFilledBox box_tv = ASSERT( isBoxyTyVar box_tv )
		       do { cts <- readMetaTyVar box_tv
		 	  ; case cts of
				Flexi -> pprPanic "readFilledBox" (ppr box_tv)
				Indirect ty -> return ty }

tcInstBoxyTyVar :: TyVar -> TcM BoxyTyVar
-- Instantiate with a BOXY type variable
tcInstBoxyTyVar tyvar = instMetaTyVar BoxTv tyvar
\end{code}


%************************************************************************
%*									*
\subsection{Putting and getting  mutable type variables}
%*									*
%************************************************************************

But it's more fun to short out indirections on the way: If this
version returns a TyVar, then that TyVar is unbound.  If it returns
any other type, then there might be bound TyVars embedded inside it.

We return Nothing iff the original box was unbound.

\begin{code}
data LookupTyVarResult	-- The result of a lookupTcTyVar call
  = DoneTv TcTyVarDetails	-- SkolemTv or virgin MetaTv
  | IndirectTv TcType

lookupTcTyVar :: TcTyVar -> TcM LookupTyVarResult
lookupTcTyVar tyvar 
  = ASSERT2( isTcTyVar tyvar, ppr tyvar )
    case details of
      SkolemTv _   -> return (DoneTv details)
      MetaTv _ ref -> do { meta_details <- readMutVar ref
			 ; case meta_details of
			    Indirect ty -> return (IndirectTv ty)
			    Flexi -> return (DoneTv details) }
  where
    details =  tcTyVarDetails tyvar

{- 
-- gaw 2004 We aren't shorting anything out anymore, at least for now
getTcTyVar tyvar
  | not (isTcTyVar tyvar)
  = pprTrace "getTcTyVar" (ppr tyvar) $
    returnM (Just (mkTyVarTy tyvar))

  | otherwise
  = ASSERT2( isTcTyVar tyvar, ppr tyvar )
    readMetaTyVar tyvar				`thenM` \ maybe_ty ->
    case maybe_ty of
	Just ty -> short_out ty				`thenM` \ ty' ->
		   writeMetaTyVar tyvar (Just ty')	`thenM_`
		   returnM (Just ty')

	Nothing	   -> returnM Nothing

short_out :: TcType -> TcM TcType
short_out ty@(TyVarTy tyvar)
  | not (isTcTyVar tyvar)
  = returnM ty

  | otherwise
  = readMetaTyVar tyvar	`thenM` \ maybe_ty ->
    case maybe_ty of
	Just ty' -> short_out ty' 			`thenM` \ ty' ->
		    writeMetaTyVar tyvar (Just ty')	`thenM_`
		    returnM ty'

	other    -> returnM ty

short_out other_ty = returnM other_ty
-}
\end{code}


%************************************************************************
%*									*
\subsection{Zonking -- the exernal interfaces}
%*									*
%************************************************************************

-----------------  Type variables

\begin{code}
zonkTcTyVars :: [TcTyVar] -> TcM [TcType]
zonkTcTyVars tyvars = mappM zonkTcTyVar tyvars

zonkTcTyVarsAndFV :: [TcTyVar] -> TcM TcTyVarSet
zonkTcTyVarsAndFV tyvars = mappM zonkTcTyVar tyvars	`thenM` \ tys ->
			   returnM (tyVarsOfTypes tys)

zonkTcTyVar :: TcTyVar -> TcM TcType
zonkTcTyVar tyvar = ASSERT2( isTcTyVar tyvar, ppr tyvar)
		    zonk_tc_tyvar (\ tv -> returnM (TyVarTy tv)) tyvar
\end{code}

-----------------  Types

\begin{code}
zonkTcType :: TcType -> TcM TcType
zonkTcType ty = zonkType (\ tv -> returnM (TyVarTy tv)) ty

zonkTcTypes :: [TcType] -> TcM [TcType]
zonkTcTypes tys = mappM zonkTcType tys

zonkTcClassConstraints cts = mappM zonk cts
    where zonk (clas, tys)
	    = zonkTcTypes tys	`thenM` \ new_tys ->
	      returnM (clas, new_tys)

zonkTcThetaType :: TcThetaType -> TcM TcThetaType
zonkTcThetaType theta = mappM zonkTcPredType theta

zonkTcPredType :: TcPredType -> TcM TcPredType
zonkTcPredType (ClassP c ts)
  = zonkTcTypes ts	`thenM` \ new_ts ->
    returnM (ClassP c new_ts)
zonkTcPredType (IParam n t)
  = zonkTcType t	`thenM` \ new_t ->
    returnM (IParam n new_t)
zonkTcPredType (EqPred t1 t2)
  = zonkTcType t1	`thenM` \ new_t1 ->
    zonkTcType t2	`thenM` \ new_t2 ->
    returnM (EqPred new_t1 new_t2)
\end{code}

-------------------  These ...ToType, ...ToKind versions
		     are used at the end of type checking

\begin{code}
zonkTopTyVar :: TcTyVar -> TcM TcTyVar
-- zonkTopTyVar is used, at the top level, on any un-instantiated meta type variables
-- to default the kind of ? and ?? etc to *.  This is important to ensure that
-- instance declarations match.  For example consider
--	instance Show (a->b)
--	foo x = show (\_ -> True)
-- Then we'll get a constraint (Show (p ->q)) where p has argTypeKind (printed ??), 
-- and that won't match the typeKind (*) in the instance decl.
--
-- Because we are at top level, no further constraints are going to affect these
-- type variables, so it's time to do it by hand.  However we aren't ready
-- to default them fully to () or whatever, because the type-class defaulting
-- rules have yet to run.

zonkTopTyVar tv
  | k `eqKind` default_k = return tv
  | otherwise
  = do	{ tv' <- newFlexiTyVar default_k
	; writeMetaTyVar tv (mkTyVarTy tv') 
	; return tv' }
  where
    k = tyVarKind tv
    default_k = defaultKind k

zonkQuantifiedTyVars :: [TcTyVar] -> TcM [TcTyVar]
zonkQuantifiedTyVars = mappM zonkQuantifiedTyVar

zonkQuantifiedTyVar :: TcTyVar -> TcM TcTyVar
-- zonkQuantifiedTyVar is applied to the a TcTyVar when quantifying over it.
--
-- The quantified type variables often include meta type variables
-- we want to freeze them into ordinary type variables, and
-- default their kind (e.g. from OpenTypeKind to TypeKind)
-- 			-- see notes with Kind.defaultKind
-- The meta tyvar is updated to point to the new skolem TyVar.  Now any 
-- bound occurences of the original type variable will get zonked to 
-- the immutable version.
--
-- We leave skolem TyVars alone; they are immutable.
zonkQuantifiedTyVar tv
  | ASSERT( isTcTyVar tv ) 
    isSkolemTyVar tv = return tv
	-- It might be a skolem type variable, 
	-- for example from a user type signature

  | otherwise	-- It's a meta-type-variable
  = do	{ details <- readMetaTyVar tv

	-- Create the new, frozen, skolem type variable
        -- We zonk to a skolem, not to a regular TcVar
        -- See Note [Zonking to Skolem]
	; let final_kind = defaultKind (tyVarKind tv)
	      final_tv   = mkSkolTyVar (tyVarName tv) final_kind UnkSkol

	-- Bind the meta tyvar to the new tyvar
	; case details of
	    Indirect ty -> WARN( True, ppr tv $$ ppr ty ) 
			   return ()
		-- [Sept 04] I don't think this should happen
		-- See note [Silly Type Synonym]

	    Flexi -> writeMetaTyVar tv (mkTyVarTy final_tv)

	-- Return the new tyvar
	; return final_tv }
\end{code}

Note [Silly Type Synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
	type C u a = u	-- Note 'a' unused

	foo :: (forall a. C u a -> C u a) -> u
	foo x = ...

	bar :: Num u => u
	bar = foo (\t -> t + t)

* From the (\t -> t+t) we get type  {Num d} =>  d -> d
  where d is fresh.

* Now unify with type of foo's arg, and we get:
	{Num (C d a)} =>  C d a -> C d a
  where a is fresh.

* Now abstract over the 'a', but float out the Num (C d a) constraint
  because it does not 'really' mention a.  (see exactTyVarsOfType)
  The arg to foo becomes
	/\a -> \t -> t+t

* So we get a dict binding for Num (C d a), which is zonked to give
	a = ()
  [Note Sept 04: now that we are zonking quantified type variables
  on construction, the 'a' will be frozen as a regular tyvar on
  quantification, so the floated dict will still have type (C d a).
  Which renders this whole note moot; happily!]

* Then the /\a abstraction has a zonked 'a' in it.

All very silly.   I think its harmless to ignore the problem.  We'll end up with
a /\a in the final result but all the occurrences of a will be zonked to ()

Note [Zonking to Skolem]
~~~~~~~~~~~~~~~~~~~~~~~~
We used to zonk quantified type variables to regular TyVars.  However, this
leads to problems.  Consider this program from the regression test suite:

  eval :: Int -> String -> String -> String
  eval 0 root actual = evalRHS 0 root actual

  evalRHS :: Int -> a
  evalRHS 0 root actual = eval 0 root actual

It leads to the deferral of an equality

  (String -> String -> String) ~ a

which is propagated up to the toplevel (see TcSimplify.tcSimplifyInferCheck).
In the meantime `a' is zonked and quantified to form `evalRHS's signature.
This has the *side effect* of also zonking the `a' in the deferred equality
(which at this point is being handed around wrapped in an implication
constraint).

Finally, the equality (with the zonked `a') will be handed back to the
simplifier by TcRnDriver.tcRnSrcDecls calling TcSimplify.tcSimplifyTop.
If we zonk `a' with a regular type variable, we will have this regular type
variable now floating around in the simplifier, which in many places assumes to
only see proper TcTyVars.

We can avoid this problem by zonking with a skolem.  The skolem is rigid
(which we requirefor a quantified variable), but is still a TcTyVar that the
simplifier knows how to deal with.


%************************************************************************
%*									*
\subsection{Zonking -- the main work-horses: zonkType, zonkTyVar}
%*									*
%*		For internal use only!					*
%*									*
%************************************************************************

\begin{code}
-- For unbound, mutable tyvars, zonkType uses the function given to it
-- For tyvars bound at a for-all, zonkType zonks them to an immutable
--	type variable and zonks the kind too

zonkType :: (TcTyVar -> TcM Type) 	-- What to do with unbound mutable type variables
					-- see zonkTcType, and zonkTcTypeToType
         -> TcType
	 -> TcM Type
zonkType unbound_var_fn ty
  = go ty
  where
    go (NoteTy _ ty2) 	 = go ty2	-- Discard free-tyvar annotations
			 
    go (TyConApp tc tys) = mappM go tys	`thenM` \ tys' ->
			   returnM (TyConApp tc tys')
			    
    go (PredTy p)	 = go_pred p		`thenM` \ p' ->
			   returnM (PredTy p')
			 
    go (FunTy arg res)   = go arg		`thenM` \ arg' ->
			   go res		`thenM` \ res' ->
			   returnM (FunTy arg' res')
 			 
    go (AppTy fun arg)	 = go fun		`thenM` \ fun' ->
			   go arg		`thenM` \ arg' ->
			   returnM (mkAppTy fun' arg')
		-- NB the mkAppTy; we might have instantiated a
		-- type variable to a type constructor, so we need
		-- to pull the TyConApp to the top.

	-- The two interesting cases!
    go (TyVarTy tyvar) | isTcTyVar tyvar = zonk_tc_tyvar unbound_var_fn tyvar
		       | otherwise	 = return (TyVarTy tyvar)
		-- Ordinary (non Tc) tyvars occur inside quantified types

    go (ForAllTy tyvar ty) = ASSERT( isImmutableTyVar tyvar )
			     go ty		`thenM` \ ty' ->
			     returnM (ForAllTy tyvar ty')

    go_pred (ClassP c tys)   = mappM go tys	`thenM` \ tys' ->
			       returnM (ClassP c tys')
    go_pred (IParam n ty)    = go ty		`thenM` \ ty' ->
			       returnM (IParam n ty')
    go_pred (EqPred ty1 ty2) = go ty1		`thenM` \ ty1' ->
			       go ty2		`thenM` \ ty2' ->
			       returnM (EqPred ty1' ty2')

zonk_tc_tyvar :: (TcTyVar -> TcM Type)		-- What to do for an unbound mutable variable
 	      -> TcTyVar -> TcM TcType
zonk_tc_tyvar unbound_var_fn tyvar 
  | not (isMetaTyVar tyvar)	-- Skolems
  = returnM (TyVarTy tyvar)

  | otherwise			-- Mutables
  = do	{ cts <- readMetaTyVar tyvar
	; case cts of
	    Flexi       -> unbound_var_fn tyvar    -- Unbound meta type variable
	    Indirect ty -> zonkType unbound_var_fn ty  }
\end{code}



%************************************************************************
%*									*
			Zonking kinds
%*									*
%************************************************************************

\begin{code}
readKindVar  :: KindVar -> TcM (MetaDetails)
writeKindVar :: KindVar -> TcKind -> TcM ()
readKindVar  kv = readMutVar (kindVarRef kv)
writeKindVar kv val = writeMutVar (kindVarRef kv) (Indirect val)

-------------
zonkTcKind :: TcKind -> TcM TcKind
zonkTcKind k = zonkTcType k

-------------
zonkTcKindToKind :: TcKind -> TcM Kind
-- When zonking a TcKind to a kind, we need to instantiate kind variables,
-- Haskell specifies that * is to be used, so we follow that.
zonkTcKindToKind k = zonkType (\ _ -> return liftedTypeKind) k
\end{code}
			
%************************************************************************
%*									*
\subsection{Checking a user type}
%*									*
%************************************************************************

When dealing with a user-written type, we first translate it from an HsType
to a Type, performing kind checking, and then check various things that should 
be true about it.  We don't want to perform these checks at the same time
as the initial translation because (a) they are unnecessary for interface-file
types and (b) when checking a mutually recursive group of type and class decls,
we can't "look" at the tycons/classes yet.  Also, the checks are are rather
diverse, and used to really mess up the other code.

One thing we check for is 'rank'.  

	Rank 0: 	monotypes (no foralls)
	Rank 1:		foralls at the front only, Rank 0 inside
	Rank 2:		foralls at the front, Rank 1 on left of fn arrow,

	basic ::= tyvar | T basic ... basic

	r2  ::= forall tvs. cxt => r2a
	r2a ::= r1 -> r2a | basic
	r1  ::= forall tvs. cxt => r0
	r0  ::= r0 -> r0 | basic
	
Another thing is to check that type synonyms are saturated. 
This might not necessarily show up in kind checking.
	type A i = i
	data T k = MkT (k Int)
	f :: T A	-- BAD!

	
\begin{code}
checkValidType :: UserTypeCtxt -> Type -> TcM ()
-- Checks that the type is valid for the given context
checkValidType ctxt ty
  = traceTc (text "checkValidType" <+> ppr ty)	`thenM_`
    doptM Opt_UnboxedTuples `thenM` \ unboxed ->
    doptM Opt_Rank2Types	`thenM` \ rank2 ->
    doptM Opt_RankNTypes	`thenM` \ rankn ->
    doptM Opt_PolymorphicComponents	`thenM` \ polycomp ->
    let 
	rank | rankn = Arbitrary
	     | rank2 = Rank 2
	     | otherwise
	     = case ctxt of	-- Haskell 98
		 GenPatCtxt	-> Rank 0
		 LamPatSigCtxt	-> Rank 0
		 BindPatSigCtxt	-> Rank 0
		 DefaultDeclCtxt-> Rank 0
		 ResSigCtxt	-> Rank 0
		 TySynCtxt _    -> Rank 0
		 ExprSigCtxt 	-> Rank 1
		 FunSigCtxt _   -> Rank 1
		 ConArgCtxt _   -> if polycomp
                           then Rank 2
                                -- We are given the type of the entire
                                -- constructor, hence rank 1
                           else Rank 1
		 ForSigCtxt _	-> Rank 1
		 SpecInstCtxt   -> Rank 1

	actual_kind = typeKind ty

	kind_ok = case ctxt of
			TySynCtxt _  -> True	-- Any kind will do
			ResSigCtxt   -> isSubOpenTypeKind 	 actual_kind
			ExprSigCtxt  -> isSubOpenTypeKind 	 actual_kind
			GenPatCtxt   -> isLiftedTypeKind actual_kind
			ForSigCtxt _ -> isLiftedTypeKind actual_kind
			other	     -> isSubArgTypeKind    actual_kind
	
	ubx_tup = case ctxt of
	              TySynCtxt _ | unboxed -> UT_Ok
	              ExprSigCtxt | unboxed -> UT_Ok
	              _                     -> UT_NotOk
    in
	-- Check that the thing has kind Type, and is lifted if necessary
    checkTc kind_ok (kindErr actual_kind)	`thenM_`

	-- Check the internal validity of the type itself
    check_poly_type rank ubx_tup ty		`thenM_`

    traceTc (text "checkValidType done" <+> ppr ty)
\end{code}


\begin{code}
data Rank = Rank Int | Arbitrary

decRank :: Rank -> Rank
decRank Arbitrary = Arbitrary
decRank (Rank n)  = Rank (n-1)

----------------------------------------
data UbxTupFlag = UT_Ok	| UT_NotOk
	-- The "Ok" version means "ok if -fglasgow-exts is on"

----------------------------------------
check_poly_type :: Rank -> UbxTupFlag -> Type -> TcM ()
check_poly_type (Rank 0) ubx_tup ty 
  = check_tau_type (Rank 0) ubx_tup ty

check_poly_type rank ubx_tup ty 
  | null tvs && null theta
  = check_tau_type rank ubx_tup ty
  | otherwise
  = do	{ check_valid_theta SigmaCtxt theta
	; check_poly_type rank ubx_tup tau	-- Allow foralls to right of arrow
	; checkFreeness tvs theta
	; checkAmbiguity tvs theta (tyVarsOfType tau) }
  where
    (tvs, theta, tau) = tcSplitSigmaTy ty
   
----------------------------------------
check_arg_type :: Type -> TcM ()
-- The sort of type that can instantiate a type variable,
-- or be the argument of a type constructor.
-- Not an unboxed tuple, but now *can* be a forall (since impredicativity)
-- Other unboxed types are very occasionally allowed as type
-- arguments depending on the kind of the type constructor
-- 
-- For example, we want to reject things like:
--
--	instance Ord a => Ord (forall s. T s a)
-- and
--	g :: T s (forall b.b)
--
-- NB: unboxed tuples can have polymorphic or unboxed args.
--     This happens in the workers for functions returning
--     product types with polymorphic components.
--     But not in user code.
-- Anyway, they are dealt with by a special case in check_tau_type

check_arg_type ty 
  = check_poly_type Arbitrary UT_NotOk ty	`thenM_` 
    checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty)

----------------------------------------
check_tau_type :: Rank -> UbxTupFlag -> Type -> TcM ()
-- Rank is allowed rank for function args
-- No foralls otherwise

check_tau_type rank ubx_tup ty@(ForAllTy _ _)       = failWithTc (forAllTyErr ty)
check_tau_type rank ubx_tup ty@(FunTy (PredTy _) _) = failWithTc (forAllTyErr ty)
	-- Reject e.g. (Maybe (?x::Int => Int)), with a decent error message

-- Naked PredTys don't usually show up, but they can as a result of
--	{-# SPECIALISE instance Ord Char #-}
-- The Right Thing would be to fix the way that SPECIALISE instance pragmas
-- are handled, but the quick thing is just to permit PredTys here.
check_tau_type rank ubx_tup (PredTy sty) = getDOpts		`thenM` \ dflags ->
					   check_pred_ty dflags TypeCtxt sty

check_tau_type rank ubx_tup (TyVarTy _)       = returnM ()
check_tau_type rank ubx_tup ty@(FunTy arg_ty res_ty)
  = check_poly_type (decRank rank) UT_NotOk arg_ty	`thenM_`
    check_poly_type rank 	   UT_Ok    res_ty

check_tau_type rank ubx_tup (AppTy ty1 ty2)
  = check_arg_type ty1 `thenM_` check_arg_type ty2

check_tau_type rank ubx_tup (NoteTy other_note ty)
  = check_tau_type rank ubx_tup ty

check_tau_type rank ubx_tup ty@(TyConApp tc tys)
  | isSynTyCon tc
  = do	{ 	-- Check that the synonym has enough args
		-- This applies equally to open and closed synonyms
	 	-- It's OK to have an *over-applied* type synonym
		--	data Tree a b = ...
		--	type Foo a = Tree [a]
		--	f :: Foo a b -> ...
 	  checkTc (tyConArity tc <= length tys) arity_msg

	-- See Note [Liberal type synonyms]
	; liberal <- doptM Opt_LiberalTypeSynonyms
	; if not liberal || isOpenSynTyCon tc then
		-- For H98 and synonym families, do check the type args
		mappM_ check_arg_type tys

	  else	-- In the liberal case (only for closed syns), expand then check
	  case tcView ty of   
	     Just ty' -> check_tau_type rank ubx_tup ty' 
	     Nothing  -> pprPanic "check_tau_type" (ppr ty)
    }
    
  | isUnboxedTupleTyCon tc
  = doptM Opt_UnboxedTuples `thenM` \ ub_tuples_allowed ->
    checkTc (ubx_tup_ok ub_tuples_allowed) ubx_tup_msg	`thenM_`
    mappM_ (check_tau_type (Rank 0) UT_Ok) tys	
		-- Args are allowed to be unlifted, or
		-- more unboxed tuples, so can't use check_arg_ty

  | otherwise
  = mappM_ check_arg_type tys

  where
    ubx_tup_ok ub_tuples_allowed = case ubx_tup of { UT_Ok -> ub_tuples_allowed; other -> False }

    n_args    = length tys
    tc_arity  = tyConArity tc

    arity_msg   = arityErr "Type synonym" (tyConName tc) tc_arity n_args
    ubx_tup_msg = ubxArgTyErr ty

----------------------------------------
forAllTyErr     ty = ptext SLIT("Illegal polymorphic or qualified type:") <+> ppr ty
unliftedArgErr  ty = ptext SLIT("Illegal unlifted type argument:") <+> ppr ty
ubxArgTyErr     ty = ptext SLIT("Illegal unboxed tuple type as function argument:") <+> ppr ty
kindErr kind       = ptext SLIT("Expecting an ordinary type, but found a type of kind") <+> ppr kind
\end{code}

Note [Liberal type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If -XLiberalTypeSynonyms is on, expand closed type synonyms *before*
doing validity checking.  This allows us to instantiate a synonym defn
with a for-all type, or with a partially-applied type synonym.
	e.g.   type T a b = a
	       type S m   = m ()
	       f :: S (T Int)
Here, T is partially applied, so it's illegal in H98.  But if you
expand S first, then T we get just
	       f :: Int
which is fine.

IMPORTANT: suppose T is a type synonym.  Then we must do validity
checking on an appliation (T ty1 ty2)

	*either* before expansion (i.e. check ty1, ty2)
	*or* after expansion (i.e. expand T ty1 ty2, and then check)
	BUT NOT BOTH

If we do both, we get exponential behaviour!!

  data TIACons1 i r c = c i ::: r c
  type TIACons2 t x = TIACons1 t (TIACons1 t x)
  type TIACons3 t x = TIACons2 t (TIACons1 t x)
  type TIACons4 t x = TIACons2 t (TIACons2 t x)
  type TIACons7 t x = TIACons4 t (TIACons3 t x)


%************************************************************************
%*									*
\subsection{Checking a theta or source type}
%*									*
%************************************************************************

\begin{code}
-- Enumerate the contexts in which a "source type", <S>, can occur
--	Eq a 
-- or 	?x::Int
-- or 	r <: {x::Int}
-- or 	(N a) where N is a newtype

data SourceTyCtxt
  = ClassSCCtxt Name	-- Superclasses of clas
			-- 	class <S> => C a where ...
  | SigmaCtxt		-- Theta part of a normal for-all type
			--	f :: <S> => a -> a
  | DataTyCtxt Name	-- Theta part of a data decl
			--	data <S> => T a = MkT a
  | TypeCtxt 		-- Source type in an ordinary type
			-- 	f :: N a -> N a
  | InstThetaCtxt	-- Context of an instance decl
			--	instance <S> => C [a] where ...
		
pprSourceTyCtxt (ClassSCCtxt c) = ptext SLIT("the super-classes of class") <+> quotes (ppr c)
pprSourceTyCtxt SigmaCtxt       = ptext SLIT("the context of a polymorphic type")
pprSourceTyCtxt (DataTyCtxt tc) = ptext SLIT("the context of the data type declaration for") <+> quotes (ppr tc)
pprSourceTyCtxt InstThetaCtxt   = ptext SLIT("the context of an instance declaration")
pprSourceTyCtxt TypeCtxt        = ptext SLIT("the context of a type")
\end{code}

\begin{code}
checkValidTheta :: SourceTyCtxt -> ThetaType -> TcM ()
checkValidTheta ctxt theta 
  = addErrCtxt (checkThetaCtxt ctxt theta) (check_valid_theta ctxt theta)

-------------------------
check_valid_theta ctxt []
  = returnM ()
check_valid_theta ctxt theta
  = getDOpts					`thenM` \ dflags ->
    warnTc (notNull dups) (dupPredWarn dups)	`thenM_`
    mappM_ (check_pred_ty dflags ctxt) theta
  where
    (_,dups) = removeDups tcCmpPred theta

-------------------------
check_pred_ty :: DynFlags -> SourceTyCtxt -> PredType -> TcM ()
check_pred_ty dflags ctxt pred@(ClassP cls tys)
  = do {	-- Class predicates are valid in all contexts
       ; checkTc (arity == n_tys) arity_err

		-- Check the form of the argument types
       ; mappM_ check_arg_type tys
       ; checkTc (check_class_pred_tys dflags ctxt tys)
		 (predTyVarErr pred $$ how_to_allow)
       }
  where
    class_name = className cls
    arity      = classArity cls
    n_tys      = length tys
    arity_err  = arityErr "Class" class_name arity n_tys
    how_to_allow = parens (ptext SLIT("Use -XFlexibleContexts to permit this"))

check_pred_ty dflags ctxt pred@(EqPred ty1 ty2)
  = do {	-- Equational constraints are valid in all contexts if type
		-- families are permitted
       ; checkTc (dopt Opt_TypeFamilies dflags) (eqPredTyErr pred)

		-- Check the form of the argument types
       ; check_eq_arg_type ty1
       ; check_eq_arg_type ty2
       }
  where 
    check_eq_arg_type = check_poly_type (Rank 0) UT_NotOk

check_pred_ty dflags SigmaCtxt (IParam _ ty) = check_arg_type ty
	-- Implicit parameters only allowed in type
	-- signatures; not in instance decls, superclasses etc
	-- The reason for not allowing implicit params in instances is a bit
	-- subtle.
	-- If we allowed	instance (?x::Int, Eq a) => Foo [a] where ...
	-- then when we saw (e :: (?x::Int) => t) it would be unclear how to 
	-- discharge all the potential usas of the ?x in e.   For example, a
	-- constraint Foo [Int] might come out of e,and applying the
	-- instance decl would show up two uses of ?x.

-- Catch-all
check_pred_ty dflags ctxt sty = failWithTc (badPredTyErr sty)

-------------------------
check_class_pred_tys :: DynFlags -> SourceTyCtxt -> [Type] -> Bool
check_class_pred_tys dflags ctxt tys 
  = case ctxt of
	TypeCtxt      -> True	-- {-# SPECIALISE instance Eq (T Int) #-} is fine
	InstThetaCtxt -> flexible_contexts || undecidable_ok || all tcIsTyVarTy tys
				-- Further checks on head and theta in
				-- checkInstTermination
	other	      -> flexible_contexts || all tyvar_head tys
  where
    flexible_contexts = dopt Opt_FlexibleContexts dflags
    undecidable_ok = dopt Opt_UndecidableInstances dflags

-------------------------
tyvar_head ty			-- Haskell 98 allows predicates of form 
  | tcIsTyVarTy ty = True	-- 	C (a ty1 .. tyn)
  | otherwise			-- where a is a type variable
  = case tcSplitAppTy_maybe ty of
	Just (ty, _) -> tyvar_head ty
	Nothing	     -> False
\end{code}

Check for ambiguity
~~~~~~~~~~~~~~~~~~~
	  forall V. P => tau
is ambiguous if P contains generic variables
(i.e. one of the Vs) that are not mentioned in tau

However, we need to take account of functional dependencies
when we speak of 'mentioned in tau'.  Example:
	class C a b | a -> b where ...
Then the type
	forall x y. (C x y) => x
is not ambiguous because x is mentioned and x determines y

NB; the ambiguity check is only used for *user* types, not for types
coming from inteface files.  The latter can legitimately have
ambiguous types. Example

   class S a where s :: a -> (Int,Int)
   instance S Char where s _ = (1,1)
   f:: S a => [a] -> Int -> (Int,Int)
   f (_::[a]) x = (a*x,b)
	where (a,b) = s (undefined::a)

Here the worker for f gets the type
	fw :: forall a. S a => Int -> (# Int, Int #)

If the list of tv_names is empty, we have a monotype, and then we
don't need to check for ambiguity either, because the test can't fail
(see is_ambig).


\begin{code}
checkAmbiguity :: [TyVar] -> ThetaType -> TyVarSet -> TcM ()
checkAmbiguity forall_tyvars theta tau_tyvars
  = mappM_ complain (filter is_ambig theta)
  where
    complain pred     = addErrTc (ambigErr pred)
    extended_tau_vars = grow theta tau_tyvars

	-- See Note [Implicit parameters and ambiguity] in TcSimplify
    is_ambig pred     = isClassPred  pred &&
			any ambig_var (varSetElems (tyVarsOfPred pred))

    ambig_var ct_var  = (ct_var `elem` forall_tyvars) &&
		        not (ct_var `elemVarSet` extended_tau_vars)

ambigErr pred
  = sep [ptext SLIT("Ambiguous constraint") <+> quotes (pprPred pred),
	 nest 4 (ptext SLIT("At least one of the forall'd type variables mentioned by the constraint") $$
		 ptext SLIT("must be reachable from the type after the '=>'"))]
\end{code}
    
In addition, GHC insists that at least one type variable
in each constraint is in V.  So we disallow a type like
	forall a. Eq b => b -> b
even in a scope where b is in scope.

\begin{code}
checkFreeness forall_tyvars theta
  = do	{ flexible_contexts <- doptM Opt_FlexibleContexts
	; unless flexible_contexts $ mappM_ complain (filter is_free theta) }
  where    
    is_free pred     =  not (isIPPred pred)
		     && not (any bound_var (varSetElems (tyVarsOfPred pred)))
    bound_var ct_var = ct_var `elem` forall_tyvars
    complain pred    = addErrTc (freeErr pred)

freeErr pred
  = sep [ ptext SLIT("All of the type variables in the constraint") <+> 
          quotes (pprPred pred)
	, ptext SLIT("are already in scope") <+>
          ptext SLIT("(at least one must be universally quantified here)")
	, nest 4 $
            ptext SLIT("(Use -XFlexibleContexts to lift this restriction)")
        ]
\end{code}

\begin{code}
checkThetaCtxt ctxt theta
  = vcat [ptext SLIT("In the context:") <+> pprTheta theta,
	  ptext SLIT("While checking") <+> pprSourceTyCtxt ctxt ]

badPredTyErr sty = ptext SLIT("Illegal constraint") <+> pprPred sty
eqPredTyErr  sty = ptext SLIT("Illegal equational constraint") <+> pprPred sty
		   $$
		   parens (ptext SLIT("Use -XTypeFamilies to permit this"))
predTyVarErr pred  = sep [ptext SLIT("Non type-variable argument"),
			  nest 2 (ptext SLIT("in the constraint:") <+> pprPred pred)]
dupPredWarn dups   = ptext SLIT("Duplicate constraint(s):") <+> pprWithCommas pprPred (map head dups)

arityErr kind name n m
  = hsep [ text kind, quotes (ppr name), ptext SLIT("should have"),
	   n_arguments <> comma, text "but has been given", int m]
    where
	n_arguments | n == 0 = ptext SLIT("no arguments")
		    | n == 1 = ptext SLIT("1 argument")
		    | True   = hsep [int n, ptext SLIT("arguments")]

-----------------
notMonoType ty
  = do	{ ty' <- zonkTcType ty
	; env0 <- tcInitTidyEnv
	; let (env1, tidy_ty) = tidyOpenType env0 ty'
	      msg = ptext SLIT("Cannot match a monotype with") <+> quotes (ppr tidy_ty)
	; failWithTcM (env1, msg) }

notMonoArgs ty
  = do	{ ty' <- zonkTcType ty
	; env0 <- tcInitTidyEnv
	; let (env1, tidy_ty) = tidyOpenType env0 ty'
	      msg = ptext SLIT("Arguments of type synonym families must be monotypes") <+> quotes (ppr tidy_ty)
	; failWithTcM (env1, msg) }
\end{code}


%************************************************************************
%*									*
\subsection{Checking for a decent instance head type}
%*									*
%************************************************************************

@checkValidInstHead@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere). In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.

\begin{code}
checkValidInstHead :: Type -> TcM (Class, [TcType])

checkValidInstHead ty	-- Should be a source type
  = case tcSplitPredTy_maybe ty of {
	Nothing -> failWithTc (instTypeErr (ppr ty) empty) ;
	Just pred -> 

    case getClassPredTys_maybe pred of {
	Nothing -> failWithTc (instTypeErr (pprPred pred) empty) ;
        Just (clas,tys) ->

    getDOpts					`thenM` \ dflags ->
    mappM_ check_arg_type tys			`thenM_`
    check_inst_head dflags clas tys		`thenM_`
    returnM (clas, tys)
    }}

check_inst_head dflags clas tys
	-- If GlasgowExts then check at least one isn't a type variable
  = do checkTc (dopt Opt_TypeSynonymInstances dflags ||
                all tcInstHeadTyNotSynonym tys)
               (instTypeErr (pprClassPred clas tys) head_type_synonym_msg)
       checkTc (dopt Opt_FlexibleInstances dflags ||
                all tcInstHeadTyAppAllTyVars tys)
               (instTypeErr (pprClassPred clas tys) head_type_args_tyvars_msg)
       checkTc (dopt Opt_MultiParamTypeClasses dflags ||
                isSingleton tys)
               (instTypeErr (pprClassPred clas tys) head_one_type_msg)
       mapM_ check_one tys
  where
    head_type_synonym_msg = parens (
                text "All instance types must be of the form (T t1 ... tn)" $$
                text "where T is not a synonym." $$
                text "Use -XTypeSynonymInstances if you want to disable this.")

    head_type_args_tyvars_msg = parens (
                text "All instance types must be of the form (T a1 ... an)" $$
                text "where a1 ... an are distinct type *variables*" $$
                text "Use -XFlexibleInstances if you want to disable this.")

    head_one_type_msg = parens (
                text "Only one type can be given in an instance head." $$
                text "Use -XMultiParamTypeClasses if you want to allow more.")

	-- For now, I only allow tau-types (not polytypes) in 
	-- the head of an instance decl.  
	-- 	E.g.  instance C (forall a. a->a) is rejected
	-- One could imagine generalising that, but I'm not sure
	-- what all the consequences might be
    check_one ty = do { check_tau_type (Rank 0) UT_NotOk ty
		      ; checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty) }

instTypeErr pp_ty msg
  = sep [ptext SLIT("Illegal instance declaration for") <+> quotes pp_ty, 
	 nest 4 msg]
\end{code}


%************************************************************************
%*									*
\subsection{Checking instance for termination}
%*									*
%************************************************************************


\begin{code}
checkValidInstance :: [TyVar] -> ThetaType -> Class -> [TcType] -> TcM ()
checkValidInstance tyvars theta clas inst_tys
  = do	{ undecidable_ok <- doptM Opt_UndecidableInstances

	; checkValidTheta InstThetaCtxt theta
	; checkAmbiguity tyvars theta (tyVarsOfTypes inst_tys)

	-- Check that instance inference will terminate (if we care)
	-- For Haskell 98 this will already have been done by checkValidTheta,
        -- but as we may be using other extensions we need to check.
	; unless undecidable_ok $
	  mapM_ addErrTc (checkInstTermination inst_tys theta)
	
	-- The Coverage Condition
	; checkTc (undecidable_ok || checkInstCoverage clas inst_tys)
	  	  (instTypeErr (pprClassPred clas inst_tys) msg)
	}
  where
    msg  = parens (vcat [ptext SLIT("the Coverage Condition fails for one of the functional dependencies;"),
			 undecidableMsg])
\end{code}

Termination test: the so-called "Paterson conditions" (see Section 5 of
"Understanding functionsl dependencies via Constraint Handling Rules, 
JFP Jan 2007).

We check that each assertion in the context satisfies:
 (1) no variable has more occurrences in the assertion than in the head, and
 (2) the assertion has fewer constructors and variables (taken together
     and counting repetitions) than the head.
This is only needed with -fglasgow-exts, as Haskell 98 restrictions
(which have already been checked) guarantee termination. 

The underlying idea is that 

    for any ground substitution, each assertion in the
    context has fewer type constructors than the head.


\begin{code}
checkInstTermination :: [TcType] -> ThetaType -> [Message]
checkInstTermination tys theta
  = mapCatMaybes check theta
  where
   fvs  = fvTypes tys
   size = sizeTypes tys
   check pred 
      | not (null (fvPred pred \\ fvs)) 
      = Just (predUndecErr pred nomoreMsg $$ parens undecidableMsg)
      | sizePred pred >= size
      = Just (predUndecErr pred smallerMsg $$ parens undecidableMsg)
      | otherwise
      = Nothing

predUndecErr pred msg = sep [msg,
			nest 2 (ptext SLIT("in the constraint:") <+> pprPred pred)]

nomoreMsg = ptext SLIT("Variable occurs more often in a constraint than in the instance head")
smallerMsg = ptext SLIT("Constraint is no smaller than the instance head")
undecidableMsg = ptext SLIT("Use -fallow-undecidable-instances to permit this")
\end{code}


%************************************************************************
%*									*
	Checking the context of a derived instance declaration
%*									*
%************************************************************************

Note [Exotic derived instance contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a 'derived' instance declaration, we *infer* the context.  It's a
bit unclear what rules we should apply for this; the Haskell report is
silent.  Obviously, constraints like (Eq a) are fine, but what about
	data T f a = MkT (f a) deriving( Eq )
where we'd get an Eq (f a) constraint.  That's probably fine too.

One could go further: consider
	data T a b c = MkT (Foo a b c) deriving( Eq )
	instance (C Int a, Eq b, Eq c) => Eq (Foo a b c)

Notice that this instance (just) satisfies the Paterson termination 
conditions.  Then we *could* derive an instance decl like this:

	instance (C Int a, Eq b, Eq c) => Eq (T a b c) 

even though there is no instance for (C Int a), because there just
*might* be an instance for, say, (C Int Bool) at a site where we
need the equality instance for T's.  

However, this seems pretty exotic, and it's quite tricky to allow
this, and yet give sensible error messages in the (much more common)
case where we really want that instance decl for C.

So for now we simply require that the derived instance context
should have only type-variable constraints.

Here is another example:
	data Fix f = In (f (Fix f)) deriving( Eq )
Here, if we are prepared to allow -fallow-undecidable-instances we
could derive the instance
	instance Eq (f (Fix f)) => Eq (Fix f)
but this is so delicate that I don't think it should happen inside
'deriving'. If you want this, write it yourself!

NB: if you want to lift this condition, make sure you still meet the
termination conditions!  If not, the deriving mechanism generates
larger and larger constraints.  Example:
  data Succ a = S a
  data Seq a = Cons a (Seq (Succ a)) | Nil deriving Show

Note the lack of a Show instance for Succ.  First we'll generate
  instance (Show (Succ a), Show a) => Show (Seq a)
and then
  instance (Show (Succ (Succ a)), Show (Succ a), Show a) => Show (Seq a)
and so on.  Instead we want to complain of no instance for (Show (Succ a)).

The bottom line
~~~~~~~~~~~~~~~
Allow constraints which consist only of type variables, with no repeats.

\begin{code}
validDerivPred :: PredType -> Bool
validDerivPred (ClassP cls tys) = hasNoDups fvs && sizeTypes tys == length fvs
				where fvs = fvTypes tys
validDerivPred otehr		= False
\end{code}

%************************************************************************
%*									*
	Checking type instance well-formedness and termination
%*									*
%************************************************************************

\begin{code}
-- Check that a "type instance" is well-formed (which includes decidability
-- unless -fallow-undecidable-instances is given).
--
checkValidTypeInst :: [Type] -> Type -> TcM ()
checkValidTypeInst typats rhs
  = do { -- left-hand side contains no type family applications
         -- (vanilla synonyms are fine, though)
       ; mappM_ checkTyFamFreeness typats

         -- the right-hand side is a tau type
       ; checkTc (isTauTy rhs) $ 
	   polyTyErr rhs

         -- we have a decidable instance unless otherwise permitted
       ; undecidable_ok <- doptM Opt_UndecidableInstances
       ; unless undecidable_ok $
	   mapM_ addErrTc (checkFamInst typats (tyFamInsts rhs))
       }

-- Make sure that each type family instance is 
--   (1) strictly smaller than the lhs,
--   (2) mentions no type variable more often than the lhs, and
--   (3) does not contain any further type family instances.
--
checkFamInst :: [Type]                  -- lhs
             -> [(TyCon, [Type])]       -- type family instances
             -> [Message]
checkFamInst lhsTys famInsts
  = mapCatMaybes check famInsts
  where
   size = sizeTypes lhsTys
   fvs  = fvTypes lhsTys
   check (tc, tys)
      | not (all isTyFamFree tys)
      = Just (famInstUndecErr famInst nestedMsg $$ parens undecidableMsg)
      | not (null (fvTypes tys \\ fvs))
      = Just (famInstUndecErr famInst nomoreVarMsg $$ parens undecidableMsg)
      | size <= sizeTypes tys
      = Just (famInstUndecErr famInst smallerAppMsg $$ parens undecidableMsg)
      | otherwise
      = Nothing
      where
        famInst = TyConApp tc tys

-- Ensure that no type family instances occur in a type.
--
checkTyFamFreeness :: Type -> TcM ()
checkTyFamFreeness ty
  = checkTc (isTyFamFree ty) $
      tyFamInstInIndexErr ty

-- Check that a type does not contain any type family applications.
--
isTyFamFree :: Type -> Bool
isTyFamFree = null . tyFamInsts

-- Error messages

tyFamInstInIndexErr ty
  = hang (ptext SLIT("Illegal type family application in type instance") <> 
         colon) 4 $
      ppr ty

polyTyErr ty 
  = hang (ptext SLIT("Illegal polymorphic type in type instance") <> colon) 4 $
      ppr ty

famInstUndecErr ty msg 
  = sep [msg, 
         nest 2 (ptext SLIT("in the type family application:") <+> 
                 pprType ty)]

nestedMsg     = ptext SLIT("Nested type family application")
nomoreVarMsg  = ptext SLIT("Variable occurs more often than in instance head")
smallerAppMsg = ptext SLIT("Application is no smaller than the instance head")
\end{code}


%************************************************************************
%*									*
\subsection{Auxiliary functions}
%*									*
%************************************************************************

\begin{code}
-- Free variables of a type, retaining repetitions, and expanding synonyms
fvType :: Type -> [TyVar]
fvType ty | Just exp_ty <- tcView ty = fvType exp_ty
fvType (TyVarTy tv)        = [tv]
fvType (TyConApp _ tys)    = fvTypes tys
fvType (NoteTy _ ty)       = fvType ty
fvType (PredTy pred)       = fvPred pred
fvType (FunTy arg res)     = fvType arg ++ fvType res
fvType (AppTy fun arg)     = fvType fun ++ fvType arg
fvType (ForAllTy tyvar ty) = filter (/= tyvar) (fvType ty)

fvTypes :: [Type] -> [TyVar]
fvTypes tys                = concat (map fvType tys)

fvPred :: PredType -> [TyVar]
fvPred (ClassP _ tys')     = fvTypes tys'
fvPred (IParam _ ty)       = fvType ty
fvPred (EqPred ty1 ty2)    = fvType ty1 ++ fvType ty2

-- Size of a type: the number of variables and constructors
sizeType :: Type -> Int
sizeType ty | Just exp_ty <- tcView ty = sizeType exp_ty
sizeType (TyVarTy _)       = 1
sizeType (TyConApp _ tys)  = sizeTypes tys + 1
sizeType (NoteTy _ ty)     = sizeType ty
sizeType (PredTy pred)     = sizePred pred
sizeType (FunTy arg res)   = sizeType arg + sizeType res + 1
sizeType (AppTy fun arg)   = sizeType fun + sizeType arg
sizeType (ForAllTy _ ty)   = sizeType ty

sizeTypes :: [Type] -> Int
sizeTypes xs               = sum (map sizeType xs)

sizePred :: PredType -> Int
sizePred (ClassP _ tys')   = sizeTypes tys'
sizePred (IParam _ ty)     = sizeType ty
sizePred (EqPred ty1 ty2)  = sizeType ty1 + sizeType ty2
\end{code}
