%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcBinds]{TcBinds}

\begin{code}
module TcBinds ( tcBindsAndThen, tcTopBinds, tcHsBootSigs, tcMonoBinds, tcSpecSigs ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcMatches ( tcGRHSsPat, tcMatchesFun )
import {-# SOURCE #-} TcExpr  ( tcCheckSigma, tcCheckRho )

import CmdLineOpts	( DynFlag(Opt_MonomorphismRestriction) )
import HsSyn		( HsExpr(..), HsBind(..), LHsBinds, Sig(..),
			  LSig, Match(..), HsBindGroup(..), IPBind(..), 
			  HsType(..), HsExplicitForAll(..), hsLTyVarNames, isVanillaLSig,
			  LPat, GRHSs, MatchGroup(..), emptyLHsBinds, isEmptyLHsBinds,
			  collectHsBindBinders, collectPatBinders, pprPatBind
			)
import TcHsSyn		( TcId, TcDictBinds, zonkId, mkHsLet )

import TcRnMonad
import Inst		( InstOrigin(..), newDictsAtLoc, newIPDict, instToId )
import TcEnv		( tcExtendIdEnv, tcExtendIdEnv2, tcExtendTyVarEnv2, 
			  newLocalName, tcLookupLocalIds, pprBinders,
			  tcGetGlobalTyVars )
import TcUnify		( Expected(..), tcInfer, unifyTheta, 
			  bleatEscapedTvs, sigCtxt )
import TcSimplify	( tcSimplifyInfer, tcSimplifyInferCheck, tcSimplifyRestricted, 
			  tcSimplifyToDicts, tcSimplifyIPs )
import TcHsType		( tcHsSigType, UserTypeCtxt(..), tcAddLetBoundTyVars,
			  TcSigInfo(..), TcSigFun, lookupSig
			)
import TcPat		( tcPat, PatCtxt(..) )
import TcSimplify	( bindInstsOfLocalFuns )
import TcMType		( newTyFlexiVarTy, zonkQuantifiedTyVar, 
			  tcInstSigType, zonkTcTypes, zonkTcTyVar )
import TcType		( TcTyVar, SkolemInfo(SigSkol), 
			  TcTauType, TcSigmaType, 
			  mkTyVarTy, mkForAllTys, mkFunTys, tyVarsOfType, 
			  mkForAllTy, isUnLiftedType, tcGetTyVar, 
			  mkTyVarTys, tidyOpenTyVar, tidyOpenType )
import Kind		( argTypeKind )
import VarEnv		( TyVarEnv, emptyVarEnv, lookupVarEnv, extendVarEnv, emptyTidyEnv ) 
import TysPrim		( alphaTyVar )
import Id		( mkLocalId, mkSpecPragmaId, setInlinePragma )
import Var		( idType, idName )
import Name		( Name )
import NameSet
import VarSet
import SrcLoc		( Located(..), unLoc, noLoc, getLoc )
import Bag
import Util		( isIn )
import BasicTypes	( TopLevelFlag(..), RecFlag(..), isNonRec, isRec, 
			  isNotTopLevel, isAlwaysActive )
import FiniteMap	( listToFM, lookupFM )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{Type-checking bindings}
%*									*
%************************************************************************

@tcBindsAndThen@ typechecks a @HsBinds@.  The "and then" part is because
it needs to know something about the {\em usage} of the things bound,
so that it can create specialisations of them.  So @tcBindsAndThen@
takes a function which, given an extended environment, E, typechecks
the scope of the bindings returning a typechecked thing and (most
important) an LIE.  It is this LIE which is then used as the basis for
specialising the things bound.

@tcBindsAndThen@ also takes a "combiner" which glues together the
bindings and the "thing" to make a new "thing".

The real work is done by @tcBindWithSigsAndThen@.

Recursive and non-recursive binds are handled in essentially the same
way: because of uniques there are no scoping issues left.  The only
difference is that non-recursive bindings can bind primitive values.

Even for non-recursive binding groups we add typings for each binder
to the LVE for the following reason.  When each individual binding is
checked the type of its LHS is unified with that of its RHS; and
type-checking the LHS of course requires that the binder is in scope.

At the top-level the LIE is sure to contain nothing but constant
dictionaries, which we resolve at the module level.

\begin{code}
tcTopBinds :: [HsBindGroup Name] -> TcM (LHsBinds TcId, TcLclEnv)
	-- Note: returning the TcLclEnv is more than we really
	--       want.  The bit we care about is the local bindings
	--	 and the free type variables thereof
tcTopBinds binds
  = tc_binds_and_then TopLevel glue binds $
	    do  { env <- getLclEnv
		; return (emptyLHsBinds, env) }
  where
	-- The top level bindings are flattened into a giant 
	-- implicitly-mutually-recursive MonoBinds
    glue (HsBindGroup binds1 _ _) (binds2, env) = (binds1 `unionBags` binds2, env)
    glue (HsIPBinds _) 		  _		= panic "Top-level HsIpBinds"
	-- Can't have a HsIPBinds at top level

tcHsBootSigs :: [HsBindGroup Name] -> TcM (LHsBinds TcId, TcLclEnv)
-- A hs-boot file has only one BindGroup, and it only has type
-- signatures in it.  The renamer checked all this
tcHsBootSigs [HsBindGroup _ sigs _]
  = do	{ ids <- mapM (addLocM tc_sig) (filter isVanillaLSig sigs)
	; tcExtendIdEnv ids $ do 
	{ env <- getLclEnv
	; return (emptyLHsBinds, env) }}
  where
    tc_sig (Sig (L _ name) ty)
      = do { sigma_ty <- tcHsSigType (FunSigCtxt name) ty
	   ; return (mkLocalId name sigma_ty) }

tcBindsAndThen
	:: (HsBindGroup TcId -> thing -> thing)		-- Combinator
	-> [HsBindGroup Name]
	-> TcM thing
	-> TcM thing

tcBindsAndThen = tc_binds_and_then NotTopLevel

tc_binds_and_then top_lvl combiner [] do_next
  = do_next
tc_binds_and_then top_lvl combiner (group : groups) do_next
  = tc_bind_and_then top_lvl combiner group $ 
    tc_binds_and_then top_lvl combiner groups do_next

tc_bind_and_then top_lvl combiner (HsIPBinds binds) do_next
  = getLIE do_next				`thenM` \ (result, expr_lie) ->
    mapAndUnzipM (wrapLocSndM tc_ip_bind) binds	`thenM` \ (avail_ips, binds') ->

	-- If the binding binds ?x = E, we  must now 
	-- discharge any ?x constraints in expr_lie
    tcSimplifyIPs avail_ips expr_lie	`thenM` \ dict_binds ->

    returnM (combiner (HsIPBinds binds') $
	     combiner (HsBindGroup dict_binds [] Recursive) result)
  where
	-- I wonder if we should do these one at at time
	-- Consider	?x = 4
	--		?y = ?x + 1
    tc_ip_bind (IPBind ip expr)
      = newTyFlexiVarTy argTypeKind		`thenM` \ ty ->
  	newIPDict (IPBindOrigin ip) ip ty	`thenM` \ (ip', ip_inst) ->
  	tcCheckRho expr ty			`thenM` \ expr' ->
  	returnM (ip_inst, (IPBind ip' expr'))

tc_bind_and_then top_lvl combiner (HsBindGroup binds sigs is_rec) do_next
  | isEmptyLHsBinds binds 
  = do_next
  | otherwise
 =      -- BRING ANY SCOPED TYPE VARIABLES INTO SCOPE
          -- Notice that they scope over 
          --       a) the type signatures in the binding group
          --       b) the bindings in the group
          --       c) the scope of the binding group (the "in" part)
      tcAddLetBoundTyVars binds  $
 
      case top_lvl of
          TopLevel       -- For the top level don't bother will all this
                         --  bindInstsOfLocalFuns stuff. All the top level 
                         -- things are rec'd together anyway, so it's fine to
                         -- leave them to the tcSimplifyTop, and quite a bit faster too
		-> tcBindWithSigs top_lvl binds sigs is_rec	`thenM` \ (poly_binds, poly_ids) ->
		   tc_body poly_ids				`thenM` \ (prag_binds, thing) ->
		   returnM (combiner (HsBindGroup
					(poly_binds `unionBags` prag_binds)
                                        [] -- no sigs
                                        Recursive)
                                     thing)
 
          NotTopLevel   -- For nested bindings we must do the bindInstsOfLocalFuns thing.
		| not (isRec is_rec)		-- Non-recursive group
		-> 	-- We want to keep non-recursive things non-recursive
                        -- so that we desugar unlifted bindings correctly
		    tcBindWithSigs top_lvl binds sigs is_rec	`thenM` \ (poly_binds, poly_ids) ->
                    getLIE (tc_body poly_ids)			`thenM` \ ((prag_binds, thing), lie) ->
 
                             -- Create specialisations of functions bound here
		    bindInstsOfLocalFuns lie poly_ids `thenM` \ lie_binds ->
 
		    returnM (
			combiner (HsBindGroup poly_binds [] NonRecursive) $
			combiner (HsBindGroup prag_binds [] NonRecursive) $
			combiner (HsBindGroup lie_binds  [] Recursive)    $
			 -- NB: the binds returned by tcSimplify and
			 -- bindInstsOfLocalFuns aren't guaranteed in
			 -- dependency order (though we could change that);
			 -- hence the Recursive marker.
                        thing)

		| otherwise
	        -> 	-- NB: polymorphic recursion means that a function
 			-- may use an instance of itself, we must look at the LIE arising
 			-- from the function's own right hand side.  Hence the getLIE
 			-- encloses the tcBindWithSigs.

 		   getLIE (
 		      tcBindWithSigs top_lvl binds sigs is_rec	`thenM` \ (poly_binds, poly_ids) ->
 		      tc_body poly_ids 				`thenM` \ (prag_binds, thing) ->
 		      returnM (poly_ids, poly_binds `unionBags` prag_binds, thing)
                   )   `thenM` \ ((poly_ids, extra_binds, thing), lie) ->
 
 	 	   bindInstsOfLocalFuns lie poly_ids	`thenM` \ lie_binds ->

                   returnM (combiner (HsBindGroup
                                        (extra_binds `unionBags` lie_binds)
                                        [] Recursive) thing
		   )
  where
    tc_body poly_ids 	-- Type check the pragmas and "thing inside"
      =   -- Extend the environment to bind the new polymorphic Ids
	  tcExtendIdEnv poly_ids	$
  
	  -- Build bindings and IdInfos corresponding to user pragmas
	  tcSpecSigs sigs		`thenM` \ prag_binds ->

	  -- Now do whatever happens next, in the augmented envt
	  do_next			`thenM` \ thing ->

	  returnM (prag_binds, thing)
\end{code}


%************************************************************************
%*									*
\subsection{tcBindWithSigs}
%*									*
%************************************************************************

@tcBindWithSigs@ deals with a single binding group.  It does generalisation,
so all the clever stuff is in here.

* binder_names and mbind must define the same set of Names

* The Names in tc_ty_sigs must be a subset of binder_names

* The Ids in tc_ty_sigs don't necessarily have to have the same name
  as the Name in the tc_ty_sig

\begin{code}
tcBindWithSigs	:: TopLevelFlag
		-> LHsBinds Name
		-> [LSig Name]
		-> RecFlag
		-> TcM (LHsBinds TcId, [TcId])
	-- The returned TcIds are guaranteed zonked

tcBindWithSigs top_lvl mbind sigs is_rec = do	
  {	-- TYPECHECK THE SIGNATURES
    tc_ty_sigs <- recoverM (returnM []) $
		  tcTySigs (filter isVanillaLSig sigs)
  ; let lookup_sig = lookupSig tc_ty_sigs

	-- SET UP THE MAIN RECOVERY; take advantage of any type sigs
  ; recoverM (recoveryCode mbind lookup_sig) $ do

  { traceTc (ptext SLIT("--------------------------------------------------------"))
  ; traceTc (ptext SLIT("Bindings for") <+> ppr (collectHsBindBinders mbind))

   	-- TYPECHECK THE BINDINGS
  ; ((mbind', mono_bind_infos), lie_req) 
	<- getLIE (tcMonoBinds mbind lookup_sig is_rec)

	-- CHECK FOR UNLIFTED BINDINGS
	-- These must be non-recursive etc, and are not generalised
	-- They desugar to a case expression in the end
  ; zonked_mono_tys <- zonkTcTypes (map getMonoType mono_bind_infos)
  ; if any isUnLiftedType zonked_mono_tys then
    do	{ 	-- Unlifted bindings
	  checkUnliftedBinds top_lvl is_rec mbind
	; extendLIEs lie_req
	; let exports  = zipWith mk_export mono_bind_infos zonked_mono_tys
	      mk_export (name, Nothing,  mono_id) mono_ty = ([], mkLocalId name mono_ty, mono_id)
	      mk_export (name, Just sig, mono_id) mono_ty = ([], sig_id sig,             mono_id)

	; return ( unitBag $ noLoc $ AbsBinds [] [] exports emptyNameSet mbind',
		   [poly_id | (_, poly_id, _) <- exports]) }	-- Guaranteed zonked

    else do	-- The normal lifted case: GENERALISE
  { is_unres <- isUnRestrictedGroup mbind tc_ty_sigs
  ; (tyvars_to_gen, dict_binds, dict_ids)
	<- setSrcSpan (getLoc (head (bagToList mbind)))	    $
		-- TODO: location a bit awkward, but the mbinds have been
		--	 dependency analysed and may no longer be adjacent
	   addErrCtxt (genCtxt (bndrNames mono_bind_infos)) $
	   generalise top_lvl is_unres mono_bind_infos tc_ty_sigs lie_req

	-- FINALISE THE QUANTIFIED TYPE VARIABLES
	-- The quantified type variables often include meta type variables
	-- we want to freeze them into ordinary type variables, and
	-- default their kind (e.g. from OpenTypeKind to TypeKind)
  ; tyvars_to_gen' <- mappM zonkQuantifiedTyVar tyvars_to_gen

	-- BUILD THE POLYMORPHIC RESULT IDs
  ; let
	exports  = map mk_export mono_bind_infos
	poly_ids = [poly_id | (_, poly_id, _) <- exports]
	dict_tys = map idType dict_ids

	inlines = mkNameSet [ name
			    | L _ (InlineSig True (L _ name) _) <- sigs]
			-- Any INLINE sig (regardless of phase control) 
			-- makes the RHS look small
        inline_phases = listToFM [ (name, phase)
				 | L _ (InlineSig _ (L _ name) phase) <- sigs, 
				   not (isAlwaysActive phase)]
			-- Set the IdInfo field to control the inline phase
			-- AlwaysActive is the default, so don't bother with them
	add_inlines id = attachInlinePhase inline_phases id

	mk_export (binder_name, mb_sig, mono_id)
	  = case mb_sig of
	      Just sig -> (sig_tvs sig, add_inlines (sig_id sig),  mono_id)
	      Nothing  -> (tyvars_to_gen', add_inlines new_poly_id, mono_id)
	  where
	    new_poly_id = mkLocalId binder_name poly_ty
	    poly_ty = mkForAllTys tyvars_to_gen'
		    $ mkFunTys dict_tys 
		    $ idType mono_id

	-- ZONK THE poly_ids, because they are used to extend the type 
	-- environment; see the invariant on TcEnv.tcExtendIdEnv 
  ; zonked_poly_ids <- mappM zonkId poly_ids

  ; traceTc (text "binding:" <+> ppr ((dict_ids, dict_binds),
				      exports, map idType zonked_poly_ids))

  ; return (
	    unitBag $ noLoc $
	    AbsBinds tyvars_to_gen'
		     dict_ids
		     exports
		     inlines
		     (dict_binds `unionBags` mbind'),
	    zonked_poly_ids
        )
  } } }

-- If typechecking the binds fails, then return with each
-- signature-less binder given type (forall a.a), to minimise 
-- subsequent error messages
recoveryCode mbind lookup_sig
  = do	{ traceTc (text "tcBindsWithSigs: error recovery" <+> ppr binder_names)
	; return (emptyLHsBinds, poly_ids) }
  where
    forall_a_a    = mkForAllTy alphaTyVar (mkTyVarTy alphaTyVar)
    binder_names  = collectHsBindBinders mbind
    poly_ids      = map mk_dummy binder_names
    mk_dummy name = case lookup_sig name of
    		      Just sig -> sig_id sig			-- Signature
    		      Nothing  -> mkLocalId name forall_a_a     -- No signature

attachInlinePhase inline_phases bndr
  = case lookupFM inline_phases (idName bndr) of
	Just prag -> bndr `setInlinePragma` prag
	Nothing   -> bndr

-- Check that non-overloaded unlifted bindings are
-- 	a) non-recursive,
--	b) not top level, 
--	c) not a multiple-binding group (more or less implied by (a))

checkUnliftedBinds top_lvl is_rec mbind
  = checkTc (isNotTopLevel top_lvl)
	    (unliftedBindErr "Top-level" mbind)		`thenM_`
    checkTc (isNonRec is_rec)
	    (unliftedBindErr "Recursive" mbind)		`thenM_`
    checkTc (isSingletonBag mbind)
    	    (unliftedBindErr "Multiple" mbind)
\end{code}


Polymorphic recursion
~~~~~~~~~~~~~~~~~~~~~
The game plan for polymorphic recursion in the code above is 

	* Bind any variable for which we have a type signature
	  to an Id with a polymorphic type.  Then when type-checking 
	  the RHSs we'll make a full polymorphic call.

This fine, but if you aren't a bit careful you end up with a horrendous
amount of partial application and (worse) a huge space leak. For example:

	f :: Eq a => [a] -> [a]
	f xs = ...f...

If we don't take care, after typechecking we get

	f = /\a -> \d::Eq a -> let f' = f a d
			       in
			       \ys:[a] -> ...f'...

Notice the the stupid construction of (f a d), which is of course
identical to the function we're executing.  In this case, the
polymorphic recursion isn't being used (but that's a very common case).
We'd prefer

	f = /\a -> \d::Eq a -> letrec
				 fm = \ys:[a] -> ...fm...
			       in
			       fm

This can lead to a massive space leak, from the following top-level defn
(post-typechecking)

	ff :: [Int] -> [Int]
	ff = f Int dEqInt

Now (f dEqInt) evaluates to a lambda that has f' as a free variable; but
f' is another thunk which evaluates to the same thing... and you end
up with a chain of identical values all hung onto by the CAF ff.

	ff = f Int dEqInt

	   = let f' = f Int dEqInt in \ys. ...f'...

	   = let f' = let f' = f Int dEqInt in \ys. ...f'...
		      in \ys. ...f'...

Etc.
Solution: when typechecking the RHSs we always have in hand the
*monomorphic* Ids for each binding.  So we just need to make sure that
if (Method f a d) shows up in the constraints emerging from (...f...)
we just use the monomorphic Id.  We achieve this by adding monomorphic Ids
to the "givens" when simplifying constraints.  That's what the "lies_avail"
is doing.


%************************************************************************
%*									*
\subsection{tcMonoBind}
%*									*
%************************************************************************

@tcMonoBinds@ deals with a single @MonoBind@.  
The signatures have been dealt with already.

\begin{code}
tcMonoBinds :: LHsBinds Name
	    -> TcSigFun -> RecFlag
	    -> TcM (LHsBinds TcId, [MonoBindInfo])

tcMonoBinds binds lookup_sig is_rec
  = do	{ tc_binds <- mapBagM (wrapLocM (tcLhs lookup_sig)) binds

	-- Bring (a) the scoped type variables, and (b) the Ids, into scope for the RHSs
	-- For (a) it's ok to bring them all into scope at once, even
	-- though each type sig should scope only over its own RHS,
	-- because the renamer has sorted all that out.
	; let mono_info  = getMonoBindInfo tc_binds
	      rhs_tvs    = [ (name, mkTyVarTy tv)
			   | (_, Just sig, _) <- mono_info, 
			     (name, tv) <- sig_scoped sig `zip` sig_tvs sig ]
	      rhs_id_env = map mk mono_info 	-- A binding for each term variable

	; binds' <- tcExtendTyVarEnv2 rhs_tvs   $
		    tcExtendIdEnv2   rhs_id_env $
		    traceTc (text "tcMonoBinds" <+> vcat [ppr n <+> ppr id <+> ppr (idType id) | (n,id) <- rhs_id_env]) `thenM_`
		    mapBagM (wrapLocM tcRhs) tc_binds
	; return (binds', mono_info) }
   where
    mk (name, Just sig, _)       = (name, sig_id sig)	-- Use the type sig if there is one
    mk (name, Nothing,  mono_id) = (name, mono_id)	-- otherwise use a monomorphic version

------------------------
-- tcLhs typechecks the LHS of the bindings, to construct the environment in which
-- we typecheck the RHSs.  Basically what we are doing is this: for each binder:
--	if there's a signature for it, use the instantiated signature type
--	otherwise invent a type variable
-- You see that quite directly in the FunBind case.
-- 
-- But there's a complication for pattern bindings:
--	data T = MkT (forall a. a->a)
--	MkT f = e
-- Here we can guess a type variable for the entire LHS (which will be refined to T)
-- but we want to get (f::forall a. a->a) as the RHS environment.
-- The simplest way to do this is to typecheck the pattern, and then look up the
-- bound mono-ids.  Then we want to retain the typechecked pattern to avoid re-doing
-- it; hence the TcMonoBind data type in which the LHS is done but the RHS isn't

data TcMonoBind		-- Half completed; LHS done, RHS not done
  = TcFunBind  MonoBindInfo  (Located TcId) Bool (MatchGroup Name) 
  | TcPatBind [MonoBindInfo] (LPat TcId) (GRHSs Name) TcSigmaType

type MonoBindInfo = (Name, Maybe TcSigInfo, TcId)
	-- Type signature (if any), and
	-- the monomorphic bound things

bndrNames :: [MonoBindInfo] -> [Name]
bndrNames mbi = [n | (n,_,_) <- mbi]

getMonoType :: MonoBindInfo -> TcTauType
getMonoType (_,_,mono_id) = idType mono_id

tcLhs :: TcSigFun -> HsBind Name -> TcM TcMonoBind
tcLhs lookup_sig (FunBind (L nm_loc name) inf matches)
  = do	{ let mb_sig = lookup_sig name
	; mono_name <- newLocalName name
	; mono_ty   <- mk_mono_ty mb_sig
	; let mono_id = mkLocalId mono_name mono_ty
	; return (TcFunBind (name, mb_sig, mono_id) (L nm_loc mono_id) inf matches) }
  where
    mk_mono_ty (Just sig) = return (sig_tau sig)
    mk_mono_ty Nothing    = newTyFlexiVarTy argTypeKind

tcLhs lookup_sig bind@(PatBind pat grhss _)
  = do	{ let tc_pat exp_ty = tcPat (LetPat lookup_sig) pat exp_ty lookup_infos
	; ((pat', ex_tvs, infos), pat_ty) 
		<- addErrCtxt (patMonoBindsCtxt pat grhss)
			      (tcInfer tc_pat)

	-- Don't know how to deal with pattern-bound existentials yet
	; checkTc (null ex_tvs) (existentialExplode bind)

	; return (TcPatBind infos pat' grhss pat_ty) }
  where
    names = collectPatBinders pat

	-- After typechecking the pattern, look up the binder
	-- names, which the pattern has brought into scope.
    lookup_infos :: TcM [MonoBindInfo]
    lookup_infos = do { mono_ids <- tcLookupLocalIds names
		      ; return [ (name, lookup_sig name, mono_id)
			       | (name, mono_id) <- names `zip` mono_ids] }

-------------------
tcRhs :: TcMonoBind -> TcM (HsBind TcId)
tcRhs (TcFunBind info fun'@(L _ mono_id) inf matches)
  = do	{ matches' <- tcMatchesFun (idName mono_id) matches 
				   (Check (idType mono_id))
	; return (FunBind fun' inf matches') }

tcRhs bind@(TcPatBind _ pat' grhss pat_ty)
  = do	{ grhss' <- addErrCtxt (patMonoBindsCtxt pat' grhss) $
		    tcGRHSsPat grhss (Check pat_ty)
	; return (PatBind pat' grhss' pat_ty) }


---------------------
getMonoBindInfo :: Bag (Located TcMonoBind) -> [MonoBindInfo]
getMonoBindInfo tc_binds
  = foldrBag (get_info . unLoc) [] tc_binds
  where
    get_info (TcFunBind info _ _ _)  rest = info : rest
    get_info (TcPatBind infos _ _ _) rest = infos ++ rest
\end{code}


%************************************************************************
%*									*
\subsection{getTyVarsToGen}
%*									*
%************************************************************************

Type signatures are tricky.  See Note [Signature skolems] in TcType

\begin{code}
tcTySigs :: [LSig Name] -> TcM [TcSigInfo]
-- The trick here is that all the signatures should have the same
-- context, and we want to share type variables for that context, so that
-- all the right hand sides agree a common vocabulary for their type
-- constraints
tcTySigs [] = return []

tcTySigs sigs
  = do	{ (tc_sig1 : tc_sigs) <- mappM tcTySig sigs
	; mapM (check_ctxt tc_sig1) tc_sigs
        ; return (tc_sig1 : tc_sigs) }
  where
	-- Check tha all the signature contexts are the same
	-- The type signatures on a mutually-recursive group of definitions
	-- must all have the same context (or none).
	--
	-- We unify them because, with polymorphic recursion, their types
	-- might not otherwise be related.  This is a rather subtle issue.
    check_ctxt :: TcSigInfo -> TcSigInfo -> TcM ()
    check_ctxt sig1@(TcSigInfo { sig_theta = theta1 }) sig@(TcSigInfo { sig_theta = theta })
	= setSrcSpan (instLocSrcSpan (sig_loc sig)) 	$
	  addErrCtxt (sigContextsCtxt sig1 sig)		$
	  unifyTheta theta1 theta


tcTySig :: LSig Name -> TcM TcSigInfo
tcTySig (L span (Sig (L _ name) ty))
  = setSrcSpan span		$
    do	{ sigma_ty <- tcHsSigType (FunSigCtxt name) ty
	; (tvs, theta, tau) <- tcInstSigType name scoped_names sigma_ty
	; loc <- getInstLoc (SigOrigin (SigSkol name))
	; return (TcSigInfo { sig_id = mkLocalId name sigma_ty, 
			      sig_tvs = tvs, sig_theta = theta, sig_tau = tau, 
			      sig_scoped = scoped_names, sig_loc = loc }) }
  where
		-- The scoped names are the ones explicitly mentioned
		-- in the HsForAll.  (There may be more in sigma_ty, because
		-- of nested type synonyms.  See Note [Scoped] with TcSigInfo.)
    scoped_names = case ty of
			L _ (HsForAllTy Explicit tvs _ _) -> hsLTyVarNames tvs
			other 		    		  -> []
\end{code}

\begin{code}
generalise :: TopLevelFlag -> Bool -> [MonoBindInfo] -> [TcSigInfo] -> [Inst]
	   -> TcM ([TcTyVar], TcDictBinds, [TcId])
generalise top_lvl is_unrestricted mono_infos sigs lie_req
  | not is_unrestricted	-- RESTRICTED CASE
  = 	-- Check signature contexts are empty 
    do	{ checkTc (all is_mono_sig sigs)
	  	  (restrictedBindCtxtErr bndr_names)

	-- Now simplify with exactly that set of tyvars
	-- We have to squash those Methods
	; (qtvs, binds) <- tcSimplifyRestricted doc top_lvl bndr_names 
						tau_tvs lie_req

   	-- Check that signature type variables are OK
	; final_qtvs <- checkSigsTyVars qtvs sigs

	; return (final_qtvs, binds, []) }

  | null sigs	-- UNRESTRICTED CASE, NO TYPE SIGS
  = tcSimplifyInfer doc tau_tvs lie_req

  | otherwise	-- UNRESTRICTED CASE, WITH TYPE SIGS
  = do	{ let sig1 = head sigs
	; sig_lie <- newDictsAtLoc (sig_loc sig1) (sig_theta sig1)
	; let	-- The "sig_avails" is the stuff available.  We get that from
		-- the context of the type signature, BUT ALSO the lie_avail
		-- so that polymorphic recursion works right (see comments at end of fn)
		local_meths = [mkMethInst sig mono_id | (_, Just sig, mono_id) <- mono_infos]
		sig_avails = sig_lie ++ local_meths

	-- Check that the needed dicts can be
	-- expressed in terms of the signature ones
	; (forall_tvs, dict_binds) <- tcSimplifyInferCheck doc tau_tvs sig_avails lie_req
	
   	-- Check that signature type variables are OK
	; final_qtvs <- checkSigsTyVars forall_tvs sigs

	; returnM (final_qtvs, dict_binds, map instToId sig_lie) }

  where
    bndr_names = bndrNames mono_infos
    tau_tvs = foldr (unionVarSet . tyVarsOfType . getMonoType) emptyVarSet mono_infos
    is_mono_sig sig = null (sig_theta sig)
    doc = ptext SLIT("type signature(s) for") <+> pprBinders bndr_names

    mkMethInst (TcSigInfo { sig_id = poly_id, sig_tvs = tvs, 
		            sig_theta = theta, sig_tau = tau, sig_loc = loc }) mono_id
      = Method mono_id poly_id (mkTyVarTys tvs) theta tau loc

checkSigsTyVars :: [TcTyVar] -> [TcSigInfo] -> TcM [TcTyVar]
checkSigsTyVars qtvs sigs 
  = do	{ gbl_tvs <- tcGetGlobalTyVars
	; sig_tvs_s <- mappM (check_sig gbl_tvs) sigs

	; let	-- Sigh.  Make sure that all the tyvars in the type sigs
		-- appear in the returned ty var list, which is what we are
		-- going to generalise over.  Reason: we occasionally get
		-- silly types like
		--	type T a = () -> ()
		--	f :: T a
		--	f () = ()
		-- Here, 'a' won't appear in qtvs, so we have to add it
	 	sig_tvs = foldl extendVarSetList emptyVarSet sig_tvs_s
		all_tvs = varSetElems (extendVarSetList sig_tvs qtvs)
	; returnM all_tvs }
  where
    check_sig gbl_tvs (TcSigInfo {sig_id = id, sig_tvs = tvs, 
				  sig_theta = theta, sig_tau = tau})
      = addErrCtxt (ptext SLIT("In the type signature for") <+> quotes (ppr id))	$
	addErrCtxtM (sigCtxt id tvs theta tau)						$
	do { tvs' <- checkDistinctTyVars tvs
	   ; ifM (any (`elemVarSet` gbl_tvs) tvs')
		 (bleatEscapedTvs gbl_tvs tvs tvs') 
	   ; return tvs' }

checkDistinctTyVars :: [TcTyVar] -> TcM [TcTyVar]
-- (checkDistinctTyVars tvs) checks that the tvs from one type signature
-- are still all type variables, and all distinct from each other.  
-- It returns a zonked set of type variables.
-- For example, if the type sig is
--	f :: forall a b. a -> b -> b
-- we want to check that 'a' and 'b' haven't 
--	(a) been unified with a non-tyvar type
--	(b) been unified with each other (all distinct)

checkDistinctTyVars sig_tvs
  = do	{ zonked_tvs <- mapM zonk_one sig_tvs
	; foldlM check_dup emptyVarEnv (sig_tvs `zip` zonked_tvs)
	; return zonked_tvs }
  where
    zonk_one sig_tv = do { ty <- zonkTcTyVar sig_tv
		         ; return (tcGetTyVar "checkDistinctTyVars" ty) }
	-- 'ty' is bound to be a type variable, because SigSkolTvs
	-- can only be unified with type variables

    check_dup :: TyVarEnv TcTyVar -> (TcTyVar, TcTyVar) -> TcM (TyVarEnv TcTyVar)
	-- The TyVarEnv maps each zonked type variable back to its
	-- corresponding user-written signature type variable
    check_dup acc (sig_tv, zonked_tv)
	= case lookupVarEnv acc zonked_tv of
		Just sig_tv' -> bomb_out sig_tv sig_tv'

		Nothing -> return (extendVarEnv acc zonked_tv sig_tv)

    bomb_out sig_tv1 sig_tv2
       = failWithTc (ptext SLIT("Quantified type variable") <+> quotes (ppr tidy_tv1) 
		     <+> ptext SLIT("is unified with another quantified type variable") 
		     <+> ppr tidy_tv2)
       where
	 (env1,  tidy_tv1) = tidyOpenTyVar emptyTidyEnv sig_tv1
	 (_env2, tidy_tv2) = tidyOpenTyVar env1	        sig_tv2
\end{code}    


@getTyVarsToGen@ decides what type variables to generalise over.

For a "restricted group" -- see the monomorphism restriction
for a definition -- we bind no dictionaries, and
remove from tyvars_to_gen any constrained type variables

*Don't* simplify dicts at this point, because we aren't going
to generalise over these dicts.  By the time we do simplify them
we may well know more.  For example (this actually came up)
	f :: Array Int Int
	f x = array ... xs where xs = [1,2,3,4,5]
We don't want to generate lots of (fromInt Int 1), (fromInt Int 2)
stuff.  If we simplify only at the f-binding (not the xs-binding)
we'll know that the literals are all Ints, and we can just produce
Int literals!

Find all the type variables involved in overloading, the
"constrained_tyvars".  These are the ones we *aren't* going to
generalise.  We must be careful about doing this:

 (a) If we fail to generalise a tyvar which is not actually
	constrained, then it will never, ever get bound, and lands
	up printed out in interface files!  Notorious example:
		instance Eq a => Eq (Foo a b) where ..
	Here, b is not constrained, even though it looks as if it is.
	Another, more common, example is when there's a Method inst in
	the LIE, whose type might very well involve non-overloaded
	type variables.
  [NOTE: Jan 2001: I don't understand the problem here so I'm doing 
	the simple thing instead]

 (b) On the other hand, we mustn't generalise tyvars which are constrained,
	because we are going to pass on out the unmodified LIE, with those
	tyvars in it.  They won't be in scope if we've generalised them.

So we are careful, and do a complete simplification just to find the
constrained tyvars. We don't use any of the results, except to
find which tyvars are constrained.

\begin{code}
isUnRestrictedGroup :: LHsBinds Name -> [TcSigInfo] -> TcM Bool
isUnRestrictedGroup binds sigs
  = do	{ mono_restriction <- doptM Opt_MonomorphismRestriction
	; return (not mono_restriction || all_unrestricted) }
  where 
    all_unrestricted = all (unrestricted . unLoc) (bagToList binds)
    tysig_names      = map (idName . sig_id) sigs

    unrestricted (PatBind other _ _)   = False
    unrestricted (VarBind v _)	       = v `is_elem` tysig_names
    unrestricted (FunBind v _ matches) = unrestricted_match matches 
					 || unLoc v `is_elem` tysig_names

    unrestricted_match (MatchGroup (L _ (Match [] _ _) : _) _) = False
	-- No args => like a pattern binding
    unrestricted_match other	          = True
	-- Some args => a function binding

is_elem v vs = isIn "isUnResMono" v vs
\end{code}


%************************************************************************
%*									*
\subsection{SPECIALIZE pragmas}
%*									*
%************************************************************************

@tcSpecSigs@ munches up the specialisation "signatures" that arise through *user*
pragmas.  It is convenient for them to appear in the @[RenamedSig]@
part of a binding because then the same machinery can be used for
moving them into place as is done for type signatures.

They look like this:

\begin{verbatim}
	f :: Ord a => [a] -> b -> b
	{-# SPECIALIZE f :: [Int] -> b -> b #-}
\end{verbatim}

For this we generate:
\begin{verbatim}
	f* = /\ b -> let d1 = ...
		     in f Int b d1
\end{verbatim}

where f* is a SpecPragmaId.  The **sole** purpose of SpecPragmaIds is to
retain a right-hand-side that the simplifier will otherwise discard as
dead code... the simplifier has a flag that tells it not to discard
SpecPragmaId bindings.

In this case the f* retains a call-instance of the overloaded
function, f, (including appropriate dictionaries) so that the
specialiser will subsequently discover that there's a call of @f@ at
Int, and will create a specialisation for @f@.  After that, the
binding for @f*@ can be discarded.

We used to have a form
	{-# SPECIALISE f :: <type> = g #-}
which promised that g implemented f at <type>, but we do that with 
a RULE now:
	{-# RULES (f::<type>) = g #-}

\begin{code}
tcSpecSigs :: [LSig Name] -> TcM (LHsBinds TcId)
tcSpecSigs (L loc (SpecSig (L nm_loc name) poly_ty) : sigs)
  = 	-- SPECIALISE f :: forall b. theta => tau  =  g
    setSrcSpan loc		 		$
    addErrCtxt (valSpecSigCtxt name poly_ty)	$

	-- Get and instantiate its alleged specialised type
    tcHsSigType (FunSigCtxt name) poly_ty	`thenM` \ sig_ty ->

	-- Check that f has a more general type, and build a RHS for
	-- the spec-pragma-id at the same time
    getLIE (tcCheckSigma (L nm_loc (HsVar name)) sig_ty)	`thenM` \ (spec_expr, spec_lie) ->

	-- Squeeze out any Methods (see comments with tcSimplifyToDicts)
    tcSimplifyToDicts spec_lie			`thenM` \ spec_binds ->

    	-- Just specialise "f" by building a SpecPragmaId binding
	-- It is the thing that makes sure we don't prematurely 
	-- dead-code-eliminate the binding we are really interested in.
    newLocalName name			`thenM` \ spec_name ->
    let
	spec_bind = VarBind (mkSpecPragmaId spec_name sig_ty)
		   		(mkHsLet spec_binds spec_expr)
    in

	-- Do the rest and combine
    tcSpecSigs sigs			`thenM` \ binds_rest ->
    returnM (binds_rest `snocBag` L loc spec_bind)

tcSpecSigs (other_sig : sigs) = tcSpecSigs sigs
tcSpecSigs []		      = returnM emptyLHsBinds
\end{code}

%************************************************************************
%*									*
\subsection[TcBinds-errors]{Error contexts and messages}
%*									*
%************************************************************************


\begin{code}
-- This one is called on LHS, when pat and grhss are both Name 
-- and on RHS, when pat is TcId and grhss is still Name
patMonoBindsCtxt pat grhss
  = hang (ptext SLIT("In a pattern binding:")) 4 (pprPatBind pat grhss)

-----------------------------------------------
valSpecSigCtxt v ty
  = sep [ptext SLIT("In a SPECIALIZE pragma for a value:"),
	 nest 4 (ppr v <+> dcolon <+> ppr ty)]

-----------------------------------------------
sigContextsCtxt sig1 sig2
  = vcat [ptext SLIT("When matching the contexts of the signatures for"), 
	  nest 2 (vcat [ppr id1 <+> dcolon <+> ppr (idType id1),
			ppr id2 <+> dcolon <+> ppr (idType id2)]),
	  ptext SLIT("The signature contexts in a mutually recursive group should all be identical")]
  where
    id1 = sig_id sig1
    id2 = sig_id sig2


-----------------------------------------------
unliftedBindErr flavour mbind
  = hang (text flavour <+> ptext SLIT("bindings for unlifted types aren't allowed:"))
	 4 (ppr mbind)

-----------------------------------------------
existentialExplode mbinds
  = hang (vcat [text "My brain just exploded.",
	        text "I can't handle pattern bindings for existentially-quantified constructors.",
		text "In the binding group"])
	4 (ppr mbinds)

-----------------------------------------------
restrictedBindCtxtErr binder_names
  = hang (ptext SLIT("Illegal overloaded type signature(s)"))
       4 (vcat [ptext SLIT("in a binding group for") <+> pprBinders binder_names,
		ptext SLIT("that falls under the monomorphism restriction")])

genCtxt binder_names
  = ptext SLIT("When generalising the type(s) for") <+> pprBinders binder_names
\end{code}
