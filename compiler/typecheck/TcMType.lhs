%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Monadic type operations

This module contains monadic operations over types that contain
mutable type variables

\begin{code}
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
  tcInstType, tcInstSigType,
  tcInstSkolTyVars, tcInstSkolType, 
  tcSkolSigType, tcSkolSigTyVars, occurCheckErr, execTcTyVarBinds,

  --------------------------------
  -- Checking type validity
  Rank, UserTypeCtxt(..), checkValidType, checkValidMonoType,
  SourceTyCtxt(..), checkValidTheta, checkFreeness,
  checkValidInstHead, checkValidInstance, 
  checkInstTermination, checkValidTypeInst, checkTyFamFreeness, checkKinds,
  checkUpdateMeta, updateMeta, checkTauTvUpdate, fillBoxWithTau, unifyKindCtxt,
  unifyKindMisMatch, validDerivPred, arityErr, notMonoType, notMonoArgs,
  growPredTyVars, growTyVars, growThetaTyVars,

  --------------------------------
  -- Zonking
  zonkType, zonkTcPredType, 
  zonkTcTyVar, zonkTcTyVars, zonkTcTyVarsAndFV, zonkSigTyVar,
  zonkQuantifiedTyVar, zonkQuantifiedTyVars,
  zonkTcType, zonkTcTypes, zonkTcThetaType,
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
import HsSyn		-- HsType
import TcRnMonad        -- TcType, amongst others
import FunDeps
import Name
import VarEnv
import VarSet
import ErrUtils
import DynFlags
import Util
import Bag
import Maybes
import ListSetOps
import UniqSupply
import SrcLoc
import Outputable
import FastString

import Control.Monad
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
		-- (type vars (excl coercion vars), preds (incl equalities), rho)
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
checkKinds :: Bool -> TyVar -> Type -> TcM ()
checkKinds swapped tv1 ty2
-- We're about to unify a type variable tv1 with a non-tyvar-type ty2.
-- ty2 has been zonked at this stage, which ensures that
-- its kind has as much boxity information visible as possible.
  | tk2 `isSubKind` tk1 = return ()

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
--       (b) that ty is a monotype
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
	| otherwise	 = do { tys' <- mapM go tys
                              ; return $ occurs (TyConApp tc) tys' }
    go (PredTy p)	      = do { p' <- go_pred p
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
    go (ForAllTy _ _) = notMonoType orig_ty		-- (b)

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
				_     -> return $ Right (TyVarTy tv)
	     }

	-- go_syn is called for synonyms only
	-- See Note [Type synonyms and the occur check]
    go_syn tc tys
	| not (isTauTyCon tc)
	= notMonoType orig_ty	-- (b) again
	| otherwise
	= do { (_msgs, mb_tys') <- tryTc (mapM go tys)
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

Execute a bag of type variable bindings.

\begin{code}
execTcTyVarBinds :: TcTyVarBinds -> TcM ()
execTcTyVarBinds = mapM_ execTcTyVarBind . bagToList
  where
    execTcTyVarBind (TcTyVarBind tv ty)
      = do { ASSERTM2( do { details <- readMetaTyVar tv
                          ; return (isFlexi details) }, ppr tv )
           ; ty' <- if isCoVar tv 
                    then return ty 
                    else do { maybe_ty <- checkTauTvUpdate tv ty
                            ; case maybe_ty of
                                Nothing -> pprPanic "TcRnMonad.execTcTyBind"
                                             (ppr tv <+> text ":=" <+> ppr ty)
                                Just ty' -> return ty'
                            }
           ; writeMetaTyVar tv ty'
           }
\end{code}

Error mesages in case of kind mismatch.

\begin{code}
unifyKindMisMatch :: TcKind -> TcKind -> TcM ()
unifyKindMisMatch ty1 ty2 = do
    ty1' <- zonkTcKind ty1
    ty2' <- zonkTcKind ty2
    let
	msg = hang (ptext (sLit "Couldn't match kind"))
		   2 (sep [quotes (ppr ty1'), 
			   ptext (sLit "against"), 
			   quotes (ppr ty2')])
    failWithTc msg

unifyKindCtxt :: Bool -> TyVar -> Type -> TidyEnv -> TcM (TidyEnv, SDoc)
unifyKindCtxt swapped tv1 ty2 tidy_env	-- not swapped => tv1 expected, ty2 inferred
	-- tv1 and ty2 are zonked already
  = return msg
  where
    msg = (env2, ptext (sLit "When matching the kinds of") <+> 
		 sep [quotes pp_expected <+> ptext (sLit "and"), quotes pp_actual])

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
    msg = ptext (sLit "Occurs check: cannot construct the infinite type:")
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
	; return [ mkCoVar (mkSysTvName uniq (fsLit "co_kv"))
			   (mkCoKind ty1 ty2)
		 | ((ty1,ty2), uniq) <- spec `zip` uniqsFromSupply us] }

newMetaCoVar :: TcType -> TcType -> TcM TcTyVar
newMetaCoVar ty1 ty2 = newMetaTyVar TauTv (mkCoKind ty1 ty2)

newKindVar :: TcM TcKind
newKindVar = do	{ uniq <- newUnique
		; ref <- newMutVar Flexi
		; return (mkTyVarTy (mkKindVar uniq ref)) }

newKindVars :: Int -> TcM [TcKind]
newKindVars n = mapM (\ _ -> newKindVar) (nOfThem n ())
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

tcInstSkolTyVar :: SkolemInfo -> (Name -> SrcSpan) -> TyVar -> TcM TcTyVar
-- Instantiate the tyvar, using 
--	* the occ-name and kind of the supplied tyvar, 
--	* the unique from the monad,
--	* the location either from the tyvar (mb_loc = Nothing)
--	  or from mb_loc (Just loc)
tcInstSkolTyVar info get_loc tyvar
  = do	{ uniq <- newUnique
	; let old_name = tyVarName tyvar
	      kind     = tyVarKind tyvar
	      loc      = get_loc old_name
	      new_name = mkInternalName uniq (nameOccName old_name) loc
	; return (mkSkolTyVar new_name kind info) }

tcInstSkolTyVars :: SkolemInfo -> [TyVar] -> TcM [TcTyVar]
-- Get the location from the monad
tcInstSkolTyVars info tyvars 
  = do 	{ span <- getSrcSpanM
	; mapM (tcInstSkolTyVar info (const span)) tyvars }

tcInstSkolType :: SkolemInfo -> TcType -> TcM ([TcTyVar], TcThetaType, TcType)
-- Instantiate a type with fresh skolem constants
-- Binding location comes from the monad
tcInstSkolType info ty = tcInstType (tcInstSkolTyVars info) ty

tcInstSigType :: Bool -> SkolemInfo -> TcType -> TcM ([TcTyVar], TcThetaType, TcRhoType)
-- Instantiate with skolems or meta SigTvs; depending on use_skols
-- Always take location info from the supplied tyvars
tcInstSigType use_skols skol_info ty
  = tcInstType (mapM inst_tyvar) ty
  where
    inst_tyvar | use_skols = tcInstSkolTyVar skol_info getSrcSpan
    	       | otherwise = instMetaTyVar (SigTv skol_info)
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
			BoxTv   -> fsLit "t"
			TauTv   -> fsLit "t"
			SigTv _ -> fsLit "a"
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
writeMetaTyVar tyvar ty
  | not debugIsOn = writeMutVar (metaTvRef tyvar) (Indirect ty)
writeMetaTyVar tyvar ty
  | not (isMetaTyVar tyvar)
  = pprTrace "writeMetaTyVar" (ppr tyvar) $
    return ()
  | otherwise
  = ASSERT( isMetaTyVar tyvar )
    ASSERT2( isCoVar tyvar || typeKind ty `isSubKind` tyVarKind tyvar, 
             (ppr tyvar <+> ppr (tyVarKind tyvar)) 
             $$ (ppr ty <+> ppr (typeKind ty)) )
    do	{ if debugIsOn then do { details <- readMetaTyVar tyvar; 
-- FIXME   	     	       	       ; ASSERT2( not (isFlexi details), ppr tyvar )
    	     	       	       ; WARN( not (isFlexi details), ppr tyvar )
			       	 return () }
			else return () 

	; traceTc (text "writeMetaTyVar" <+> ppr tyvar <+> text ":=" <+> ppr ty)
	; writeMutVar (metaTvRef tyvar) (Indirect ty) }
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
newFlexiTyVarTy kind = do
    tc_tyvar <- newFlexiTyVar kind
    return (TyVarTy tc_tyvar)

newFlexiTyVarTys :: Int -> Kind -> TcM [TcType]
newFlexiTyVarTys n kind = mapM newFlexiTyVarTy (nOfThem n kind)

tcInstTyVar :: TyVar -> TcM TcTyVar
-- Instantiate with a META type variable
tcInstTyVar tyvar = instMetaTyVar TauTv tyvar

tcInstTyVars :: [TyVar] -> TcM ([TcTyVar], [TcType], TvSubst)
-- Instantiate with META type variables
tcInstTyVars tyvars
  = do	{ tc_tvs <- mapM tcInstTyVar tyvars
	; let tys = mkTyVarTys tc_tvs
	; return (tc_tvs, tys, zipTopTvSubst tyvars tys) }
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
    return (Just (mkTyVarTy tyvar))

  | otherwise
  = ASSERT2( isTcTyVar tyvar, ppr tyvar ) do
    maybe_ty <- readMetaTyVar tyvar
    case maybe_ty of
        Just ty -> do ty' <- short_out ty
                      writeMetaTyVar tyvar (Just ty')
                      return (Just ty')

	Nothing	   -> return Nothing

short_out :: TcType -> TcM TcType
short_out ty@(TyVarTy tyvar)
  | not (isTcTyVar tyvar)
  = return ty

  | otherwise = do
    maybe_ty <- readMetaTyVar tyvar
    case maybe_ty of
        Just ty' -> do ty' <- short_out ty'
                       writeMetaTyVar tyvar (Just ty')
                       return ty'

	other    -> return ty

short_out other_ty = return other_ty
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
zonkTcTyVars tyvars = mapM zonkTcTyVar tyvars

zonkTcTyVarsAndFV :: [TcTyVar] -> TcM TcTyVarSet
zonkTcTyVarsAndFV tyvars = tyVarsOfTypes <$> mapM zonkTcTyVar tyvars

zonkTcTyVar :: TcTyVar -> TcM TcType
zonkTcTyVar tyvar = ASSERT2( isTcTyVar tyvar, ppr tyvar)
		    zonk_tc_tyvar (\ tv -> return (TyVarTy tv)) tyvar
\end{code}

-----------------  Types

\begin{code}
zonkTcType :: TcType -> TcM TcType
zonkTcType ty = zonkType (\ tv -> return (TyVarTy tv)) ty

zonkTcTypes :: [TcType] -> TcM [TcType]
zonkTcTypes tys = mapM zonkTcType tys

zonkTcThetaType :: TcThetaType -> TcM TcThetaType
zonkTcThetaType theta = mapM zonkTcPredType theta

zonkTcPredType :: TcPredType -> TcM TcPredType
zonkTcPredType (ClassP c ts)  = ClassP c <$> zonkTcTypes ts
zonkTcPredType (IParam n t)   = IParam n <$> zonkTcType t
zonkTcPredType (EqPred t1 t2) = EqPred <$> zonkTcType t1 <*> zonkTcType t2
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
zonkQuantifiedTyVars = mapM zonkQuantifiedTyVar

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
  | ASSERT2( isTcTyVar tv, ppr tv ) 
    isSkolemTyVar tv 
  = do { kind <- zonkTcType (tyVarKind tv)
       ; return $ setTyVarKind tv kind
       }
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
	\/\a -> \t -> t+t

* So we get a dict binding for Num (C d a), which is zonked to give
	a = ()
  [Note Sept 04: now that we are zonking quantified type variables
  on construction, the 'a' will be frozen as a regular tyvar on
  quantification, so the floated dict will still have type (C d a).
  Which renders this whole note moot; happily!]

* Then the \/\a abstraction has a zonked 'a' in it.

All very silly.   I think its harmless to ignore the problem.  We'll end up with
a \/\a in the final result but all the occurrences of a will be zonked to ()

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
    go (TyConApp tc tys) = do tys' <- mapM go tys
                              return (TyConApp tc tys')

    go (PredTy p)        = do p' <- go_pred p
                              return (PredTy p')

    go (FunTy arg res)   = do arg' <- go arg
                              res' <- go res
                              return (FunTy arg' res')

    go (AppTy fun arg)   = do fun' <- go fun
                              arg' <- go arg
                              return (mkAppTy fun' arg')
		-- NB the mkAppTy; we might have instantiated a
		-- type variable to a type constructor, so we need
		-- to pull the TyConApp to the top.

	-- The two interesting cases!
    go (TyVarTy tyvar) | isTcTyVar tyvar = zonk_tc_tyvar unbound_var_fn tyvar
		       | otherwise	 = liftM TyVarTy $ 
                                             zonkTyVar unbound_var_fn tyvar
		-- Ordinary (non Tc) tyvars occur inside quantified types

    go (ForAllTy tyvar ty) = ASSERT( isImmutableTyVar tyvar ) do
                             ty' <- go ty
                             tyvar' <- zonkTyVar unbound_var_fn tyvar
                             return (ForAllTy tyvar' ty')

    go_pred (ClassP c tys)   = do tys' <- mapM go tys
                                  return (ClassP c tys')
    go_pred (IParam n ty)    = do ty' <- go ty
                                  return (IParam n ty')
    go_pred (EqPred ty1 ty2) = do ty1' <- go ty1
                                  ty2' <- go ty2
                                  return (EqPred ty1' ty2')

zonk_tc_tyvar :: (TcTyVar -> TcM Type)	-- What to do for an unbound mutable var
 	      -> TcTyVar -> TcM TcType
zonk_tc_tyvar unbound_var_fn tyvar 
  = ASSERT( isTcTyVar tyvar )
    case tcTyVarDetails tyvar of
      SkolemTv {}  -> return (TyVarTy tyvar)
      FlatSkol ty  -> zonkType unbound_var_fn ty
      MetaTv _ ref -> do { cts <- readMutVar ref
			 ; case cts of    
			     Flexi       -> unbound_var_fn tyvar  
			     Indirect ty -> zonkType unbound_var_fn ty  }

-- Zonk the kind of a non-TC tyvar in case it is a coercion variable (their
-- kind contains types).
--
zonkTyVar :: (TcTyVar -> TcM Type)      -- What to do for an unbound mutable var
 	  -> TyVar -> TcM TyVar
zonkTyVar unbound_var_fn tv 
  | isCoVar tv
  = do { kind <- zonkType unbound_var_fn (tyVarKind tv)
       ; return $ setTyVarKind tv kind
       }
  | otherwise = return tv
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
checkValidType ctxt ty = do
    traceTc (text "checkValidType" <+> ppr ty)
    unboxed  <- doptM Opt_UnboxedTuples
    rank2    <- doptM Opt_Rank2Types
    rankn    <- doptM Opt_RankNTypes
    polycomp <- doptM Opt_PolymorphicComponents
    let 
	gen_rank n | rankn     = ArbitraryRank
	           | rank2     = Rank 2
	           | otherwise = Rank n
	rank
	  = case ctxt of
		 DefaultDeclCtxt-> MustBeMonoType
		 ResSigCtxt	-> MustBeMonoType
		 LamPatSigCtxt	-> gen_rank 0
		 BindPatSigCtxt	-> gen_rank 0
		 TySynCtxt _    -> gen_rank 0
		 GenPatCtxt	-> gen_rank 1
			-- This one is a bit of a hack
			-- See the forall-wrapping in TcClassDcl.mkGenericInstance		

		 ExprSigCtxt 	-> gen_rank 1
		 FunSigCtxt _   -> gen_rank 1
		 ConArgCtxt _   | polycomp -> gen_rank 2
                                -- We are given the type of the entire
                                -- constructor, hence rank 1
 				| otherwise -> gen_rank 1

		 ForSigCtxt _	-> gen_rank 1
		 SpecInstCtxt   -> gen_rank 1
		 ThBrackCtxt    -> gen_rank 1

	actual_kind = typeKind ty

	kind_ok = case ctxt of
			TySynCtxt _  -> True -- Any kind will do
			ThBrackCtxt  -> True -- Any kind will do
			ResSigCtxt   -> isSubOpenTypeKind actual_kind
			ExprSigCtxt  -> isSubOpenTypeKind actual_kind
			GenPatCtxt   -> isLiftedTypeKind actual_kind
			ForSigCtxt _ -> isLiftedTypeKind actual_kind
			_            -> isSubArgTypeKind actual_kind
	
	ubx_tup = case ctxt of
	              TySynCtxt _ | unboxed -> UT_Ok
	              ExprSigCtxt | unboxed -> UT_Ok
	              ThBrackCtxt | unboxed -> UT_Ok
	              _                     -> UT_NotOk

	-- Check the internal validity of the type itself
    check_type rank ubx_tup ty

	-- Check that the thing has kind Type, and is lifted if necessary
	-- Do this second, becuase we can't usefully take the kind of an 
	-- ill-formed type such as (a~Int)
    checkTc kind_ok (kindErr actual_kind)

    traceTc (text "checkValidType done" <+> ppr ty)

checkValidMonoType :: Type -> TcM ()
checkValidMonoType ty = check_mono_type MustBeMonoType ty
\end{code}


\begin{code}
data Rank = ArbitraryRank	  -- Any rank ok
          | MustBeMonoType  	  -- Monotype regardless of flags
	  | TyConArgMonoType	  -- Monotype but could be poly if -XImpredicativeTypes
	  | SynArgMonoType	  -- Monotype but could be poly if -XLiberalTypeSynonyms
          | Rank Int		  -- Rank n, but could be more with -XRankNTypes

decRank :: Rank -> Rank		  -- Function arguments
decRank (Rank 0)   = Rank 0
decRank (Rank n)   = Rank (n-1)
decRank other_rank = other_rank

nonZeroRank :: Rank -> Bool
nonZeroRank ArbitraryRank = True
nonZeroRank (Rank n) 	  = n>0
nonZeroRank _        	  = False

----------------------------------------
data UbxTupFlag = UT_Ok	| UT_NotOk
	-- The "Ok" version means "ok if UnboxedTuples is on"

----------------------------------------
check_mono_type :: Rank -> Type -> TcM ()	-- No foralls anywhere
				      		-- No unlifted types of any kind
check_mono_type rank ty
   = do { check_type rank UT_NotOk ty
	; checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty) }

check_type :: Rank -> UbxTupFlag -> Type -> TcM ()
-- The args say what the *type context* requires, independent
-- of *flag* settings.  You test the flag settings at usage sites.
-- 
-- Rank is allowed rank for function args
-- Rank 0 means no for-alls anywhere

check_type rank ubx_tup ty
  | not (null tvs && null theta)
  = do	{ checkTc (nonZeroRank rank) (forAllTyErr rank ty)
		-- Reject e.g. (Maybe (?x::Int => Int)), 
		-- with a decent error message
	; check_valid_theta SigmaCtxt theta
	; check_type rank ubx_tup tau	-- Allow foralls to right of arrow
	; checkFreeness tvs theta
	; checkAmbiguity tvs theta (tyVarsOfType tau) }
  where
    (tvs, theta, tau) = tcSplitSigmaTy ty
   
-- Naked PredTys should, I think, have been rejected before now
check_type _ _ ty@(PredTy {})
  = failWithTc (text "Predicate used as a type:" <+> ppr ty)

check_type _ _ (TyVarTy _) = return ()

check_type rank _ (FunTy arg_ty res_ty)
  = do	{ check_type (decRank rank) UT_NotOk arg_ty
	; check_type rank 	    UT_Ok    res_ty }

check_type rank _ (AppTy ty1 ty2)
  = do	{ check_arg_type rank ty1
	; check_arg_type rank ty2 }

check_type rank ubx_tup ty@(TyConApp tc tys)
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
		mapM_ (check_mono_type SynArgMonoType) tys

	  else	-- In the liberal case (only for closed syns), expand then check
	  case tcView ty of   
	     Just ty' -> check_type rank ubx_tup ty' 
	     Nothing  -> pprPanic "check_tau_type" (ppr ty)
    }
    
  | isUnboxedTupleTyCon tc
  = do	{ ub_tuples_allowed <- doptM Opt_UnboxedTuples
	; checkTc (ubx_tup_ok ub_tuples_allowed) ubx_tup_msg

	; impred <- doptM Opt_ImpredicativeTypes	
	; let rank' = if impred then ArbitraryRank else TyConArgMonoType
		-- c.f. check_arg_type
		-- However, args are allowed to be unlifted, or
		-- more unboxed tuples, so can't use check_arg_ty
	; mapM_ (check_type rank' UT_Ok) tys }

  | otherwise
  = mapM_ (check_arg_type rank) tys

  where
    ubx_tup_ok ub_tuples_allowed = case ubx_tup of
                                   UT_Ok -> ub_tuples_allowed
                                   _     -> False

    n_args    = length tys
    tc_arity  = tyConArity tc

    arity_msg   = arityErr "Type synonym" (tyConName tc) tc_arity n_args
    ubx_tup_msg = ubxArgTyErr ty

check_type _ _ ty = pprPanic "check_type" (ppr ty)

----------------------------------------
check_arg_type :: Rank -> Type -> TcM ()
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

check_arg_type rank ty 
  = do	{ impred <- doptM Opt_ImpredicativeTypes
	; let rank' = case rank of 	    -- Predictive => must be monotype
	      	        MustBeMonoType     -> MustBeMonoType  -- Monotype, regardless
			_other | impred    -> ArbitraryRank
			       | otherwise -> TyConArgMonoType
			-- Make sure that MustBeMonoType is propagated, 
			-- so that we don't suggest -XImpredicativeTypes in
			--    (Ord (forall a.a)) => a -> a
			-- and so that if it Must be a monotype, we check that it is!

	; check_type rank' UT_NotOk ty
	; checkTc (not (isUnLiftedType ty)) (unliftedArgErr ty) }

----------------------------------------
forAllTyErr :: Rank -> Type -> SDoc
forAllTyErr rank ty 
   = vcat [ hang (ptext (sLit "Illegal polymorphic or qualified type:")) 2 (ppr ty)
          , suggestion ]
  where
    suggestion = case rank of
    	       	   Rank _ -> ptext (sLit "Perhaps you intended to use -XRankNTypes or -XRank2Types")
    	       	   TyConArgMonoType -> ptext (sLit "Perhaps you intended to use -XImpredicativeTypes")
    	       	   SynArgMonoType -> ptext (sLit "Perhaps you intended to use -XLiberalTypeSynonyms")
		   _ -> empty      -- Polytype is always illegal

unliftedArgErr, ubxArgTyErr :: Type -> SDoc
unliftedArgErr  ty = sep [ptext (sLit "Illegal unlifted type:"), ppr ty]
ubxArgTyErr     ty = sep [ptext (sLit "Illegal unboxed tuple type as function argument:"), ppr ty]

kindErr :: Kind -> SDoc
kindErr kind       = sep [ptext (sLit "Expecting an ordinary type, but found a type of kind"), ppr kind]
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
		
pprSourceTyCtxt :: SourceTyCtxt -> SDoc
pprSourceTyCtxt (ClassSCCtxt c) = ptext (sLit "the super-classes of class") <+> quotes (ppr c)
pprSourceTyCtxt SigmaCtxt       = ptext (sLit "the context of a polymorphic type")
pprSourceTyCtxt (DataTyCtxt tc) = ptext (sLit "the context of the data type declaration for") <+> quotes (ppr tc)
pprSourceTyCtxt InstThetaCtxt   = ptext (sLit "the context of an instance declaration")
pprSourceTyCtxt TypeCtxt        = ptext (sLit "the context of a type")
\end{code}

\begin{code}
checkValidTheta :: SourceTyCtxt -> ThetaType -> TcM ()
checkValidTheta ctxt theta 
  = addErrCtxt (checkThetaCtxt ctxt theta) (check_valid_theta ctxt theta)

-------------------------
check_valid_theta :: SourceTyCtxt -> [PredType] -> TcM ()
check_valid_theta _ []
  = return ()
check_valid_theta ctxt theta = do
    dflags <- getDOpts
    warnTc (notNull dups) (dupPredWarn dups)
    mapM_ (check_pred_ty dflags ctxt) theta
  where
    (_,dups) = removeDups tcCmpPred theta

-------------------------
check_pred_ty :: DynFlags -> SourceTyCtxt -> PredType -> TcM ()
check_pred_ty dflags ctxt pred@(ClassP cls tys)
  = do {	-- Class predicates are valid in all contexts
       ; checkTc (arity == n_tys) arity_err

		-- Check the form of the argument types
       ; mapM_ checkValidMonoType tys
       ; checkTc (check_class_pred_tys dflags ctxt tys)
		 (predTyVarErr pred $$ how_to_allow)
       }
  where
    class_name = className cls
    arity      = classArity cls
    n_tys      = length tys
    arity_err  = arityErr "Class" class_name arity n_tys
    how_to_allow = parens (ptext (sLit "Use -XFlexibleContexts to permit this"))

check_pred_ty _ (ClassSCCtxt _) (EqPred _ _)
  =   -- We do not yet support superclass equalities.
    failWithTc $
      sep [ ptext (sLit "The current implementation of type families does not")
          , ptext (sLit "support equality constraints in superclass contexts.")
          , ptext (sLit "They are planned for a future release.")
          ]

check_pred_ty dflags _ pred@(EqPred ty1 ty2)
  = do {	-- Equational constraints are valid in all contexts if type
		-- families are permitted
       ; checkTc (dopt Opt_TypeFamilies dflags) (eqPredTyErr pred)

		-- Check the form of the argument types
       ; checkValidMonoType ty1
       ; checkValidMonoType ty2
       }

check_pred_ty _ SigmaCtxt (IParam _ ty) = checkValidMonoType ty
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
check_pred_ty _ _ sty = failWithTc (badPredTyErr sty)

-------------------------
check_class_pred_tys :: DynFlags -> SourceTyCtxt -> [Type] -> Bool
check_class_pred_tys dflags ctxt tys 
  = case ctxt of
	TypeCtxt      -> True	-- {-# SPECIALISE instance Eq (T Int) #-} is fine
	InstThetaCtxt -> flexible_contexts || undecidable_ok || all tcIsTyVarTy tys
				-- Further checks on head and theta in
				-- checkInstTermination
	_             -> flexible_contexts || all tyvar_head tys
  where
    flexible_contexts = dopt Opt_FlexibleContexts dflags
    undecidable_ok = dopt Opt_UndecidableInstances dflags

-------------------------
tyvar_head :: Type -> Bool
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
  = mapM_ complain (filter is_ambig theta)
  where
    complain pred     = addErrTc (ambigErr pred)
    extended_tau_vars = growThetaTyVars theta tau_tyvars

	-- See Note [Implicit parameters and ambiguity] in TcSimplify
    is_ambig pred     = isClassPred  pred &&
			any ambig_var (varSetElems (tyVarsOfPred pred))

    ambig_var ct_var  = (ct_var `elem` forall_tyvars) &&
		        not (ct_var `elemVarSet` extended_tau_vars)

ambigErr :: PredType -> SDoc
ambigErr pred
  = sep [ptext (sLit "Ambiguous constraint") <+> quotes (pprPred pred),
	 nest 4 (ptext (sLit "At least one of the forall'd type variables mentioned by the constraint") $$
		 ptext (sLit "must be reachable from the type after the '=>'"))]

--------------------------
-- For this 'grow' stuff see Note [Growing the tau-tvs using constraints] in Inst

growThetaTyVars :: TcThetaType -> TyVarSet -> TyVarSet
-- Finds a fixpoint
growThetaTyVars theta tvs
  | null theta = tvs
  | otherwise  = fixVarSet mk_next tvs
  where
    mk_next tvs = foldr growPredTyVars tvs theta


growPredTyVars :: TcPredType -> TyVarSet -> TyVarSet
-- Here is where the special case for inplicit parameters happens
growPredTyVars (IParam _ ty) tvs = tvs `unionVarSet` tyVarsOfType ty
growPredTyVars pred          tvs = growTyVars (tyVarsOfPred pred) tvs

growTyVars :: TyVarSet -> TyVarSet -> TyVarSet
growTyVars new_tvs tvs 
  | new_tvs `intersectsVarSet` tvs = tvs `unionVarSet` new_tvs
  | otherwise			   = tvs
\end{code}
    
In addition, GHC insists that at least one type variable
in each constraint is in V.  So we disallow a type like
	forall a. Eq b => b -> b
even in a scope where b is in scope.

\begin{code}
checkFreeness :: [Var] -> [PredType] -> TcM ()
checkFreeness forall_tyvars theta
  = do	{ flexible_contexts <- doptM Opt_FlexibleContexts
	; unless flexible_contexts $ mapM_ complain (filter is_free theta) }
  where    
    is_free pred     =  not (isIPPred pred)
		     && not (any bound_var (varSetElems (tyVarsOfPred pred)))
    bound_var ct_var = ct_var `elem` forall_tyvars
    complain pred    = addErrTc (freeErr pred)

freeErr :: PredType -> SDoc
freeErr pred
  = sep [ ptext (sLit "All of the type variables in the constraint") <+> 
          quotes (pprPred pred)
	, ptext (sLit "are already in scope") <+>
          ptext (sLit "(at least one must be universally quantified here)")
	, nest 4 $
            ptext (sLit "(Use -XFlexibleContexts to lift this restriction)")
        ]
\end{code}

\begin{code}
checkThetaCtxt :: SourceTyCtxt -> ThetaType -> SDoc
checkThetaCtxt ctxt theta
  = vcat [ptext (sLit "In the context:") <+> pprTheta theta,
	  ptext (sLit "While checking") <+> pprSourceTyCtxt ctxt ]

badPredTyErr, eqPredTyErr, predTyVarErr :: PredType -> SDoc
badPredTyErr sty = ptext (sLit "Illegal constraint") <+> pprPred sty
eqPredTyErr  sty = ptext (sLit "Illegal equational constraint") <+> pprPred sty
		   $$
		   parens (ptext (sLit "Use -XTypeFamilies to permit this"))
predTyVarErr pred  = sep [ptext (sLit "Non type-variable argument"),
			  nest 2 (ptext (sLit "in the constraint:") <+> pprPred pred)]
dupPredWarn :: [[PredType]] -> SDoc
dupPredWarn dups   = ptext (sLit "Duplicate constraint(s):") <+> pprWithCommas pprPred (map head dups)

arityErr :: Outputable a => String -> a -> Int -> Int -> SDoc
arityErr kind name n m
  = hsep [ text kind, quotes (ppr name), ptext (sLit "should have"),
	   n_arguments <> comma, text "but has been given", 
           if m==0 then text "none" else int m]
    where
	n_arguments | n == 0 = ptext (sLit "no arguments")
		    | n == 1 = ptext (sLit "1 argument")
		    | True   = hsep [int n, ptext (sLit "arguments")]

-----------------
notMonoType :: TcType -> TcM a
notMonoType ty
  = do	{ ty' <- zonkTcType ty
	; env0 <- tcInitTidyEnv
	; let (env1, tidy_ty) = tidyOpenType env0 ty'
	      msg = ptext (sLit "Cannot match a monotype with") <+> quotes (ppr tidy_ty)
	; failWithTcM (env1, msg) }

notMonoArgs :: TcType -> TcM a
notMonoArgs ty
  = do	{ ty' <- zonkTcType ty
	; env0 <- tcInitTidyEnv
	; let (env1, tidy_ty) = tidyOpenType env0 ty'
	      msg = ptext (sLit "Arguments of type synonym families must be monotypes") <+> quotes (ppr tidy_ty)
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
        Just (clas,tys) -> do

    dflags <- getDOpts
    check_inst_head dflags clas tys
    return (clas, tys)
    }}

check_inst_head :: DynFlags -> Class -> [Type] -> TcM ()
check_inst_head dflags clas tys
  = do { -- If GlasgowExts then check at least one isn't a type variable
       ; checkTc (dopt Opt_TypeSynonymInstances dflags ||
                  all tcInstHeadTyNotSynonym tys)
                 (instTypeErr (pprClassPred clas tys) head_type_synonym_msg)
       ; checkTc (dopt Opt_FlexibleInstances dflags ||
                  all tcInstHeadTyAppAllTyVars tys)
                 (instTypeErr (pprClassPred clas tys) head_type_args_tyvars_msg)
       ; checkTc (dopt Opt_MultiParamTypeClasses dflags ||
                  isSingleton tys)
                 (instTypeErr (pprClassPred clas tys) head_one_type_msg)
         -- May not contain type family applications
       ; mapM_ checkTyFamFreeness tys

       ; mapM_ checkValidMonoType tys
	-- For now, I only allow tau-types (not polytypes) in 
	-- the head of an instance decl.  
	-- 	E.g.  instance C (forall a. a->a) is rejected
	-- One could imagine generalising that, but I'm not sure
	-- what all the consequences might be
       }

  where
    head_type_synonym_msg = parens (
                text "All instance types must be of the form (T t1 ... tn)" $$
                text "where T is not a synonym." $$
                text "Use -XTypeSynonymInstances if you want to disable this.")

    head_type_args_tyvars_msg = parens (vcat [
                text "All instance types must be of the form (T a1 ... an)",
                text "where a1 ... an are type *variables*,",
                text "and each type variable appears at most once in the instance head.",
                text "Use -XFlexibleInstances if you want to disable this."])

    head_one_type_msg = parens (
                text "Only one type can be given in an instance head." $$
                text "Use -XMultiParamTypeClasses if you want to allow more.")

instTypeErr :: SDoc -> SDoc -> SDoc
instTypeErr pp_ty msg
  = sep [ptext (sLit "Illegal instance declaration for") <+> quotes pp_ty, 
	 nest 4 msg]
\end{code}


%************************************************************************
%*									*
\subsection{Checking instance for termination}
%*									*
%************************************************************************

\begin{code}
checkValidInstance :: LHsType Name -> [TyVar] -> ThetaType -> Type 
                   -> TcM (Class, [TcType])
checkValidInstance hs_type tyvars theta tau
  = setSrcSpan (getLoc hs_type) $
    do	{ (clas, inst_tys) <- setSrcSpan head_loc $
                              checkValidInstHead tau

        ; undecidable_ok <- doptM Opt_UndecidableInstances

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

        ; return (clas, inst_tys)
	}
  where
    msg  = parens (vcat [ptext (sLit "the Coverage Condition fails for one of the functional dependencies;"),
			 undecidableMsg])

	-- The location of the "head" of the instance
    head_loc = case hs_type of
                 L _ (HsForAllTy _ _ _ (L loc _)) -> loc
                 L loc _                          -> loc
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

predUndecErr :: PredType -> SDoc -> SDoc
predUndecErr pred msg = sep [msg,
			nest 2 (ptext (sLit "in the constraint:") <+> pprPred pred)]

nomoreMsg, smallerMsg, undecidableMsg :: SDoc
nomoreMsg = ptext (sLit "Variable occurs more often in a constraint than in the instance head")
smallerMsg = ptext (sLit "Constraint is no smaller than the instance head")
undecidableMsg = ptext (sLit "Use -XUndecidableInstances to permit this")
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
Here, if we are prepared to allow -XUndecidableInstances we
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
validDerivPred (ClassP _ tys) = hasNoDups fvs && sizeTypes tys == length fvs
                              where fvs = fvTypes tys
validDerivPred _              = False
\end{code}

%************************************************************************
%*									*
	Checking type instance well-formedness and termination
%*									*
%************************************************************************

\begin{code}
-- Check that a "type instance" is well-formed (which includes decidability
-- unless -XUndecidableInstances is given).
--
checkValidTypeInst :: [Type] -> Type -> TcM ()
checkValidTypeInst typats rhs
  = do { -- left-hand side contains no type family applications
         -- (vanilla synonyms are fine, though)
       ; mapM_ checkTyFamFreeness typats

         -- the right-hand side is a tau type
       ; checkValidMonoType rhs

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
      tyFamInstIllegalErr ty

-- Check that a type does not contain any type family applications.
--
isTyFamFree :: Type -> Bool
isTyFamFree = null . tyFamInsts

-- Error messages

tyFamInstIllegalErr :: Type -> SDoc
tyFamInstIllegalErr ty
  = hang (ptext (sLit "Illegal type synonym family application in instance") <> 
         colon) 4 $
      ppr ty

famInstUndecErr :: Type -> SDoc -> SDoc
famInstUndecErr ty msg 
  = sep [msg, 
         nest 2 (ptext (sLit "in the type family application:") <+> 
                 pprType ty)]

nestedMsg, nomoreVarMsg, smallerAppMsg :: SDoc
nestedMsg     = ptext (sLit "Nested type family application")
nomoreVarMsg  = ptext (sLit "Variable occurs more often than in instance head")
smallerAppMsg = ptext (sLit "Application is no smaller than the instance head")
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
sizeType (PredTy pred)     = sizePred pred
sizeType (FunTy arg res)   = sizeType arg + sizeType res + 1
sizeType (AppTy fun arg)   = sizeType fun + sizeType arg
sizeType (ForAllTy _ ty)   = sizeType ty

sizeTypes :: [Type] -> Int
sizeTypes xs               = sum (map sizeType xs)

-- Size of a predicate
--
-- Equalities are a special case.  The equality itself doesn't contribute to the
-- size and as we do not count class predicates, we have to start with one less.
-- This is easy to see considering that, given
--   class C a b | a -> b
--   type family F a
-- constraints (C a b) and (F a ~ b) are equivalent in size.
sizePred :: PredType -> Int
sizePred (ClassP _ tys')   = sizeTypes tys'
sizePred (IParam _ ty)     = sizeType ty
sizePred (EqPred ty1 ty2)  = sizeType ty1 + sizeType ty2 - 1
\end{code}
