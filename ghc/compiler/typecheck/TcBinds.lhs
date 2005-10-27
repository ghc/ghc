%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcBinds]{TcBinds}

\begin{code}
module TcBinds ( tcLocalBinds, tcTopBinds, 
		 tcHsBootSigs, tcMonoBinds, 
		 TcPragFun, tcSpecPrag, tcPrags, mkPragFun,
		 badBootDeclErr ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcMatches ( tcGRHSsPat, tcMatchesFun )
import {-# SOURCE #-} TcExpr  ( tcCheckRho )

import DynFlags		( DynFlag(Opt_MonomorphismRestriction, Opt_GlasgowExts) )
import HsSyn		( HsExpr(..), HsBind(..), LHsBinds, LHsBind, Sig(..),
			  HsLocalBinds(..), HsValBinds(..), HsIPBinds(..),
			  LSig, Match(..), IPBind(..), Prag(..),
			  HsType(..), LHsType, HsExplicitForAll(..), hsLTyVarNames, 
			  isVanillaLSig, sigName, placeHolderNames, isPragLSig,
			  LPat, GRHSs, MatchGroup(..), pprLHsBinds,
			  collectHsBindBinders, collectPatBinders, pprPatBind
			)
import TcHsSyn		( zonkId, (<$>) )

import TcRnMonad
import Inst		( newDictsAtLoc, newIPDict, instToId )
import TcEnv		( tcExtendIdEnv, tcExtendIdEnv2, tcExtendTyVarEnv2, 
			  tcLookupLocalIds, pprBinders,
			  tcGetGlobalTyVars )
import TcUnify		( Expected(..), tcInfer, unifyTheta, tcSub,
			  bleatEscapedTvs, sigCtxt )
import TcSimplify	( tcSimplifyInfer, tcSimplifyInferCheck, 
			  tcSimplifyRestricted, tcSimplifyIPs )
import TcHsType		( tcHsSigType, UserTypeCtxt(..), tcAddLetBoundTyVars,
			  TcSigInfo(..), TcSigFun, lookupSig
			)
import TcPat		( tcPat, PatCtxt(..) )
import TcSimplify	( bindInstsOfLocalFuns )
import TcMType		( newTyFlexiVarTy, zonkQuantifiedTyVar, 
			  tcInstSigType, zonkTcType, zonkTcTypes, zonkTcTyVar )
import TcType		( TcType, TcTyVar, SkolemInfo(SigSkol), 
			  TcTauType, TcSigmaType, isUnboxedTupleType,
			  mkTyVarTy, mkForAllTys, mkFunTys, tyVarsOfType, 
			  mkForAllTy, isUnLiftedType, tcGetTyVar, 
			  mkTyVarTys, tidyOpenTyVar )
import Kind		( argTypeKind )
import VarEnv		( TyVarEnv, emptyVarEnv, lookupVarEnv, extendVarEnv, emptyTidyEnv ) 
import TysPrim		( alphaTyVar )
import Id		( Id, mkLocalId, mkVanillaGlobal )
import IdInfo		( vanillaIdInfo )
import Var		( TyVar, idType, idName )
import Name		( Name )
import NameSet
import NameEnv
import VarSet
import SrcLoc		( Located(..), unLoc, getLoc )
import Bag
import ErrUtils		( Message )
import Digraph		( SCC(..), stronglyConnComp )
import Maybes		( fromJust, isJust, isNothing, orElse, catMaybes )
import Util		( singleton )
import BasicTypes	( TopLevelFlag(..), isTopLevel, isNotTopLevel,
			  RecFlag(..), isNonRec, InlineSpec, defaultInlineSpec )
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
tcTopBinds :: HsValBinds Name -> TcM (LHsBinds TcId, TcLclEnv)
	-- Note: returning the TcLclEnv is more than we really
	--       want.  The bit we care about is the local bindings
	--	 and the free type variables thereof
tcTopBinds binds
  = do	{ (ValBindsOut prs _, env) <- tcValBinds TopLevel binds getLclEnv
	; return (foldr (unionBags . snd) emptyBag prs, env) }
	-- The top level bindings are flattened into a giant 
	-- implicitly-mutually-recursive LHsBinds

tcHsBootSigs :: HsValBinds Name -> TcM [Id]
-- A hs-boot file has only one BindGroup, and it only has type
-- signatures in it.  The renamer checked all this
tcHsBootSigs (ValBindsOut binds sigs)
  = do	{ checkTc (null binds) badBootDeclErr
	; mapM (addLocM tc_boot_sig) (filter isVanillaLSig sigs) }
  where
    tc_boot_sig (TypeSig (L _ name) ty)
      = do { sigma_ty <- tcHsSigType (FunSigCtxt name) ty
	   ; return (mkVanillaGlobal name sigma_ty vanillaIdInfo) }
	-- Notice that we make GlobalIds, not LocalIds
tcHsBootSigs groups = pprPanic "tcHsBootSigs" (ppr groups)

badBootDeclErr :: Message
badBootDeclErr = ptext SLIT("Illegal declarations in an hs-boot file")

------------------------
tcLocalBinds :: HsLocalBinds Name -> TcM thing
	     -> TcM (HsLocalBinds TcId, thing)

tcLocalBinds EmptyLocalBinds thing_inside 
  = do	{ thing <- thing_inside
	; return (EmptyLocalBinds, thing) }

tcLocalBinds (HsValBinds binds) thing_inside
  = do	{ (binds', thing) <- tcValBinds NotTopLevel binds thing_inside
	; return (HsValBinds binds', thing) }

tcLocalBinds (HsIPBinds (IPBinds ip_binds _)) thing_inside
  = do	{ (thing, lie) <- getLIE thing_inside
	; (avail_ips, ip_binds') <- mapAndUnzipM (wrapLocSndM tc_ip_bind) ip_binds

	-- If the binding binds ?x = E, we  must now 
	-- discharge any ?x constraints in expr_lie
	; dict_binds <- tcSimplifyIPs avail_ips lie
	; return (HsIPBinds (IPBinds ip_binds' dict_binds), thing) }
  where
	-- I wonder if we should do these one at at time
	-- Consider	?x = 4
	--		?y = ?x + 1
    tc_ip_bind (IPBind ip expr)
      = newTyFlexiVarTy argTypeKind		`thenM` \ ty ->
  	newIPDict (IPBindOrigin ip) ip ty	`thenM` \ (ip', ip_inst) ->
  	tcCheckRho expr ty			`thenM` \ expr' ->
  	returnM (ip_inst, (IPBind ip' expr'))

------------------------
tcValBinds :: TopLevelFlag 
	   -> HsValBinds Name -> TcM thing
	   -> TcM (HsValBinds TcId, thing) 

tcValBinds top_lvl (ValBindsIn binds sigs) thing_inside
  = pprPanic "tcValBinds" (ppr binds)

tcValBinds top_lvl (ValBindsOut binds sigs) thing_inside
  = tcAddLetBoundTyVars binds  $
      -- BRING ANY SCOPED TYPE VARIABLES INTO SCOPE
          -- Notice that they scope over 
          --       a) the type signatures in the binding group
          --       b) the bindings in the group
          --       c) the scope of the binding group (the "in" part)
 
    do 	{   	-- Typecheck the signature
	  tc_ty_sigs <- recoverM (returnM []) (tcTySigs sigs)
	; let { prag_fn = mkPragFun sigs
	      ; sig_fn  = lookupSig tc_ty_sigs
	      ; sig_ids = map sig_id tc_ty_sigs }

		-- Extend the envt right away with all 
		-- the Ids declared with type signatures
  	; (binds', thing) <- tcExtendIdEnv sig_ids $
			     tc_val_binds top_lvl sig_fn prag_fn 
					  binds thing_inside

	; return (ValBindsOut binds' sigs, thing) }

------------------------
tc_val_binds :: TopLevelFlag -> TcSigFun -> TcPragFun
	     -> [(RecFlag, LHsBinds Name)] -> TcM thing
	     -> TcM ([(RecFlag, LHsBinds TcId)], thing)
-- Typecheck a whole lot of value bindings,
-- one strongly-connected component at a time

tc_val_binds top_lvl sig_fn prag_fn [] thing_inside
  = do	{ thing <- thing_inside
	; return ([], thing) }

tc_val_binds top_lvl sig_fn prag_fn (group : groups) thing_inside
  = do	{ (group', (groups', thing))
		<- tc_group top_lvl sig_fn prag_fn group $ 
		   tc_val_binds top_lvl sig_fn prag_fn groups thing_inside
	; return (group' ++ groups', thing) }

------------------------
tc_group :: TopLevelFlag -> TcSigFun -> TcPragFun
	 -> (RecFlag, LHsBinds Name) -> TcM thing
 	 -> TcM ([(RecFlag, LHsBinds TcId)], thing)

-- Typecheck one strongly-connected component of the original program.
-- We get a list of groups back, because there may 
-- be specialisations etc as well

tc_group top_lvl sig_fn prag_fn (NonRecursive, binds) thing_inside
  =  	-- A single non-recursive binding
     	-- We want to keep non-recursive things non-recursive
        -- so that we desugar unlifted bindings correctly
    do	{ (binds, thing) <- tcPolyBinds top_lvl NonRecursive NonRecursive
					sig_fn prag_fn binds thing_inside
	; return ([(NonRecursive, b) | b <- binds], thing) }

tc_group top_lvl sig_fn prag_fn (Recursive, binds) thing_inside
  =	-- A recursive strongly-connected component
 	-- To maximise polymorphism (with -fglasgow-exts), we do a new 
	-- strongly-connected  component analysis, this time omitting 
	-- any references to variables with type signatures.
	--
	-- Then we bring into scope all the variables with type signatures
    do	{ traceTc (text "tc_group rec" <+> pprLHsBinds binds)
	; gla_exts     <- doptM Opt_GlasgowExts
	; (binds,thing) <- if gla_exts 
			   then go new_sccs
			   else tc_binds Recursive binds thing_inside
	; return ([(Recursive, unionManyBags binds)], thing) }
		-- Rec them all together
  where
    new_sccs :: [SCC (LHsBind Name)]
    new_sccs = stronglyConnComp (mkEdges sig_fn binds)

--  go :: SCC (LHsBind Name) -> TcM ([LHsBind TcId], thing)
    go (scc:sccs) = do	{ (binds1, (binds2, thing)) <- go1 scc (go sccs)
			; return (binds1 ++ binds2, thing) }
    go [] 	  = do	{ thing <- thing_inside; return ([], thing) }

    go1 (AcyclicSCC bind) = tc_binds NonRecursive (unitBag bind)
    go1 (CyclicSCC binds) = tc_binds Recursive    (listToBag binds)

    tc_binds rec_tc binds = tcPolyBinds top_lvl Recursive rec_tc sig_fn prag_fn binds

------------------------
mkEdges :: TcSigFun -> LHsBinds Name
	-> [(LHsBind Name, BKey, [BKey])]

type BKey  = Int -- Just number off the bindings

mkEdges sig_fn binds
  = [ (bind, key, [fromJust mb_key | n <- nameSetToList (bind_fvs (unLoc bind)),
				     let mb_key = lookupNameEnv key_map n,
				     isJust mb_key,
				     no_sig n ])
    | (bind, key) <- keyd_binds
    ]
  where
    no_sig :: Name -> Bool
    no_sig n = isNothing (sig_fn n)

    keyd_binds = bagToList binds `zip` [0::BKey ..]

    bind_fvs (FunBind _ _ _ fvs) = fvs
    bind_fvs (PatBind _ _ _ fvs) = fvs
    bind_fvs bind = pprPanic "mkEdges" (ppr bind)

    key_map :: NameEnv BKey	-- Which binding it comes from
    key_map = mkNameEnv [(bndr, key) | (L _ bind, key) <- keyd_binds
				     , bndr <- bindersOfHsBind bind ]

bindersOfHsBind :: HsBind Name -> [Name]
bindersOfHsBind (PatBind pat _ _ _)     = collectPatBinders pat
bindersOfHsBind (FunBind (L _ f) _ _ _) = [f]

------------------------
tcPolyBinds :: TopLevelFlag 
	    -> RecFlag			-- Whether the group is really recursive
	    -> RecFlag			-- Whether it's recursive for typechecking purposes
	    -> TcSigFun -> TcPragFun
	    -> LHsBinds Name
 	    -> TcM thing
	    -> TcM ([LHsBinds TcId], thing)

-- Typechecks a single bunch of bindings all together, 
-- and generalises them.  The bunch may be only part of a recursive
-- group, because we use type signatures to maximise polymorphism
--
-- Deals with the bindInstsOfLocalFuns thing too
--
-- Returns a list because the input may be a single non-recursive binding,
-- in which case the dependency order of the resulting bindings is
-- important.  

tcPolyBinds top_lvl rec_group rec_tc sig_fn prag_fn scc thing_inside
  =	-- NB: polymorphic recursion means that a function
	-- may use an instance of itself, we must look at the LIE arising
	-- from the function's own right hand side.  Hence the getLIE
	-- encloses the tc_poly_binds. 
    do	{ traceTc (text "tcPolyBinds" <+> ppr scc)
	; ((binds1, poly_ids, thing), lie) <- getLIE $ 
		do { (binds1, poly_ids) <- tc_poly_binds top_lvl rec_group rec_tc
							 sig_fn prag_fn scc
		   ; thing <- tcExtendIdEnv poly_ids thing_inside
		   ; return (binds1, poly_ids, thing) }

	; if isTopLevel top_lvl 
	  then		-- For the top level don't bother will all this
			-- bindInstsOfLocalFuns stuff. All the top level 
			-- things are rec'd together anyway, so it's fine to
		        -- leave them to the tcSimplifyTop, 
			-- and quite a bit faster too
		do { extendLIEs lie; return (binds1, thing) }

	  else do	-- Nested case
		{ lie_binds <- bindInstsOfLocalFuns lie poly_ids
	 	; return (binds1 ++ [lie_binds], thing) }}

------------------------
tc_poly_binds :: TopLevelFlag		-- See comments on tcPolyBinds
	      -> RecFlag -> RecFlag
	      -> TcSigFun -> TcPragFun
	      -> LHsBinds Name
	      -> TcM ([LHsBinds TcId], [TcId])
-- Typechecks the bindings themselves
-- Knows nothing about the scope of the bindings

tc_poly_binds top_lvl rec_group rec_tc sig_fn prag_fn binds
  = let 
        binder_names = collectHsBindBinders binds
	bind_list    = bagToList binds

	loc = getLoc (head bind_list)
		-- TODO: location a bit awkward, but the mbinds have been
		--	 dependency analysed and may no longer be adjacent
    in
	-- SET UP THE MAIN RECOVERY; take advantage of any type sigs
    setSrcSpan loc				$
    recoverM (recoveryCode binder_names sig_fn)	$ do 

  { traceTc (ptext SLIT("------------------------------------------------"))
  ; traceTc (ptext SLIT("Bindings for") <+> ppr binder_names)

   	-- TYPECHECK THE BINDINGS
  ; ((binds', mono_bind_infos), lie_req) 
	<- getLIE (tcMonoBinds bind_list sig_fn rec_tc)

	-- CHECK FOR UNLIFTED BINDINGS
	-- These must be non-recursive etc, and are not generalised
	-- They desugar to a case expression in the end
  ; zonked_mono_tys <- zonkTcTypes (map getMonoType mono_bind_infos)
  ; if any isUnLiftedType zonked_mono_tys then
    do	{ 	-- Unlifted bindings
	  checkUnliftedBinds top_lvl rec_group binds' mono_bind_infos
	; extendLIEs lie_req
	; let exports  = zipWith mk_export mono_bind_infos zonked_mono_tys
	      mk_export (name, Nothing,  mono_id) mono_ty = ([], mkLocalId name mono_ty, mono_id, [])
	      mk_export (name, Just sig, mono_id) mono_ty = ([], sig_id sig,             mono_id, [])
			-- ToDo: prags

	; return ( [unitBag $ L loc $ AbsBinds [] [] exports binds'],
		   [poly_id | (_, poly_id, _, _) <- exports]) }	-- Guaranteed zonked

    else do	-- The normal lifted case: GENERALISE
  { is_unres <- isUnRestrictedGroup bind_list sig_fn
  ; (tyvars_to_gen, dict_binds, dict_ids)
	<- addErrCtxt (genCtxt (bndrNames mono_bind_infos)) $
	   generalise top_lvl is_unres mono_bind_infos lie_req

	-- FINALISE THE QUANTIFIED TYPE VARIABLES
	-- The quantified type variables often include meta type variables
	-- we want to freeze them into ordinary type variables, and
	-- default their kind (e.g. from OpenTypeKind to TypeKind)
  ; tyvars_to_gen' <- mappM zonkQuantifiedTyVar tyvars_to_gen

	-- BUILD THE POLYMORPHIC RESULT IDs
  ; exports <- mapM (mkExport prag_fn tyvars_to_gen' (map idType dict_ids))
		    mono_bind_infos

	-- ZONK THE poly_ids, because they are used to extend the type 
	-- environment; see the invariant on TcEnv.tcExtendIdEnv 
  ; let	poly_ids = [poly_id | (_, poly_id, _, _) <- exports]
  ; zonked_poly_ids <- mappM zonkId poly_ids

  ; traceTc (text "binding:" <+> ppr ((dict_ids, dict_binds),
				      map idType zonked_poly_ids))

  ; let abs_bind = L loc $ AbsBinds tyvars_to_gen'
	 		            dict_ids exports
	 		    	    (dict_binds `unionBags` binds')

  ; return ([unitBag abs_bind], zonked_poly_ids)
  } }


--------------
mkExport :: TcPragFun -> [TyVar] -> [TcType] -> MonoBindInfo
	 -> TcM ([TyVar], Id, Id, [Prag])
mkExport prag_fn inferred_tvs dict_tys (poly_name, mb_sig, mono_id)
  = do	{ prags <- tcPrags poly_id (prag_fn poly_name)
	; return (tvs, poly_id, mono_id, prags) }
  where
    (tvs, poly_id) = case mb_sig of
			Just sig -> (sig_tvs sig,  sig_id sig)
			Nothing  -> (inferred_tvs, mkLocalId poly_name poly_ty)
		   where
		     poly_ty = mkForAllTys inferred_tvs
			       $ mkFunTys dict_tys 
			       $ idType mono_id

------------------------
type TcPragFun = Name -> [LSig Name]

mkPragFun :: [LSig Name] -> TcPragFun
mkPragFun sigs = \n -> lookupNameEnv env n `orElse` []
	where
	  prs = [(fromJust (sigName sig), sig) | sig <- sigs, isPragLSig sig]
	  env = foldl add emptyNameEnv prs
	  add env (n,p) = extendNameEnv_Acc (:) singleton env n p

tcPrags :: Id -> [LSig Name] -> TcM [Prag]
tcPrags poly_id prags = mapM tc_prag prags
  where
    tc_prag (L loc prag) = setSrcSpan loc $ 
			   addErrCtxt (pragSigCtxt prag) $ 
			   tcPrag poly_id prag

pragSigCtxt prag = hang (ptext SLIT("In the pragma")) 2 (ppr prag)

tcPrag :: TcId -> Sig Name -> TcM Prag
tcPrag poly_id (SpecSig orig_name hs_ty inl) = tcSpecPrag poly_id hs_ty inl
tcPrag poly_id (SpecInstSig hs_ty)	     = tcSpecPrag poly_id hs_ty defaultInlineSpec
tcPrag poly_id (InlineSig v inl)             = return (InlinePrag inl)


tcSpecPrag :: TcId -> LHsType Name -> InlineSpec -> TcM Prag
tcSpecPrag poly_id hs_ty inl
  = do	{ spec_ty <- tcHsSigType (FunSigCtxt (idName poly_id)) hs_ty
	; (co_fn, lie)   <- getLIE (tcSub spec_ty (idType poly_id))
	; extendLIEs lie
	; let const_dicts = map instToId lie
	; return (SpecPrag (co_fn <$> (HsVar poly_id)) spec_ty const_dicts inl) }
  
--------------
-- If typechecking the binds fails, then return with each
-- signature-less binder given type (forall a.a), to minimise 
-- subsequent error messages
recoveryCode binder_names sig_fn
  = do	{ traceTc (text "tcBindsWithSigs: error recovery" <+> ppr binder_names)
	; return ([], poly_ids) }
  where
    forall_a_a    = mkForAllTy alphaTyVar (mkTyVarTy alphaTyVar)
    poly_ids      = map mk_dummy binder_names
    mk_dummy name = case sig_fn name of
    		      Just sig -> sig_id sig			-- Signature
    		      Nothing  -> mkLocalId name forall_a_a     -- No signature

-- Check that non-overloaded unlifted bindings are
-- 	a) non-recursive,
--	b) not top level, 
--	c) not a multiple-binding group (more or less implied by (a))

checkUnliftedBinds :: TopLevelFlag -> RecFlag
		   -> LHsBinds TcId -> [MonoBindInfo] -> TcM ()
checkUnliftedBinds top_lvl rec_group mbind infos
  = do 	{ checkTc (isNotTopLevel top_lvl)
	  	  (unliftedBindErr "Top-level" mbind)
	; checkTc (isNonRec rec_group)
	  	  (unliftedBindErr "Recursive" mbind)
	; checkTc (isSingletonBag mbind)
	    	  (unliftedBindErr "Multiple" mbind) 
	; mapM_ check_sig infos }
  where
    check_sig (_, Just sig, _) = checkTc (null (sig_tvs sig) && null (sig_theta sig))
					 (badUnliftedSig sig)
    check_sig other	       = return ()
\end{code}


%************************************************************************
%*									*
\subsection{tcMonoBind}
%*									*
%************************************************************************

@tcMonoBinds@ deals with a perhaps-recursive group of HsBinds.
The signatures have been dealt with already.

\begin{code}
tcMonoBinds :: [LHsBind Name]
	    -> TcSigFun
	    -> RecFlag	-- True <=> the binding is recursive for typechecking purposes
			-- 	    i.e. the binders are mentioned in their RHSs, and
			--		 we are not resuced by a type signature
	    -> TcM (LHsBinds TcId, [MonoBindInfo])

tcMonoBinds [L b_loc (FunBind (L nm_loc name) inf matches fvs)]
	    sig_fn 		-- Single function binding,
	    NonRecursive	-- binder isn't mentioned in RHS,
  | Nothing <- sig_fn name	-- ...with no type signature
  = 	-- In this very special case we infer the type of the
	-- right hand side first (it may have a higher-rank type)
	-- and *then* make the monomorphic Id for the LHS
	-- e.g.		f = \(x::forall a. a->a) -> <body>
	-- 	We want to infer a higher-rank type for f
    setSrcSpan b_loc  	$
    do	{ (matches', rhs_ty) <- tcInfer (tcMatchesFun name matches)

		-- Check for an unboxed tuple type
		--	f = (# True, False #)
		-- Zonk first just in case it's hidden inside a meta type variable
		-- (This shows up as a (more obscure) kind error 
		--  in the 'otherwise' case of tcMonoBinds.)
	; zonked_rhs_ty <- zonkTcType rhs_ty
	; checkTc (not (isUnboxedTupleType zonked_rhs_ty))
		  (unboxedTupleErr name zonked_rhs_ty)

	; mono_name <- newLocalName name
	; let mono_id = mkLocalId mono_name zonked_rhs_ty
	; return (unitBag (L b_loc (FunBind (L nm_loc mono_id) inf matches' fvs)),
		  [(name, Nothing, mono_id)]) }

tcMonoBinds binds sig_fn non_rec
  = do	{ tc_binds <- mapM (wrapLocM (tcLhs sig_fn)) binds

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
		    traceTc (text "tcMonoBinds" <+> vcat [ ppr n <+> ppr id <+> ppr (idType id) 
							 | (n,id) <- rhs_id_env]) `thenM_`
		    mapM (wrapLocM tcRhs) tc_binds
	; return (listToBag binds', mono_info) }
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
tcLhs sig_fn (FunBind (L nm_loc name) inf matches _)
  = do	{ let mb_sig = sig_fn name
	; mono_name <- newLocalName name
	; mono_ty   <- mk_mono_ty mb_sig
	; let mono_id = mkLocalId mono_name mono_ty
	; return (TcFunBind (name, mb_sig, mono_id) (L nm_loc mono_id) inf matches) }
  where
    mk_mono_ty (Just sig) = return (sig_tau sig)
    mk_mono_ty Nothing    = newTyFlexiVarTy argTypeKind

tcLhs sig_fn bind@(PatBind pat grhss _ _)
  = do	{ let tc_pat exp_ty = tcPat (LetPat sig_fn) pat exp_ty lookup_infos
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
		      ; return [ (name, sig_fn name, mono_id)
			       | (name, mono_id) <- names `zip` mono_ids] }

tcLhs sig_fn other_bind = pprPanic "tcLhs" (ppr other_bind)
	-- AbsBind, VarBind impossible

-------------------
tcRhs :: TcMonoBind -> TcM (HsBind TcId)
tcRhs (TcFunBind info fun'@(L _ mono_id) inf matches)
  = do	{ matches' <- tcMatchesFun (idName mono_id) matches 
				   (Check (idType mono_id))
	; return (FunBind fun' inf matches' placeHolderNames) }

tcRhs bind@(TcPatBind _ pat' grhss pat_ty)
  = do	{ grhss' <- addErrCtxt (patMonoBindsCtxt pat' grhss) $
		    tcGRHSsPat grhss (Check pat_ty)
	; return (PatBind pat' grhss' pat_ty placeHolderNames) }


---------------------
getMonoBindInfo :: [Located TcMonoBind] -> [MonoBindInfo]
getMonoBindInfo tc_binds
  = foldr (get_info . unLoc) [] tc_binds
  where
    get_info (TcFunBind info _ _ _)  rest = info : rest
    get_info (TcPatBind infos _ _ _) rest = infos ++ rest
\end{code}


%************************************************************************
%*									*
		Generalisation
%*									*
%************************************************************************

\begin{code}
generalise :: TopLevelFlag -> Bool 
	   -> [MonoBindInfo] -> [Inst]
	   -> TcM ([TcTyVar], TcDictBinds, [TcId])
generalise top_lvl is_unrestricted mono_infos lie_req
  | not is_unrestricted	-- RESTRICTED CASE
  = 	-- Check signature contexts are empty 
    do	{ checkTc (all is_mono_sig sigs)
	  	  (restrictedBindCtxtErr bndrs)

	-- Now simplify with exactly that set of tyvars
	-- We have to squash those Methods
	; (qtvs, binds) <- tcSimplifyRestricted doc top_lvl bndrs 
						tau_tvs lie_req

   	-- Check that signature type variables are OK
	; final_qtvs <- checkSigsTyVars qtvs sigs

	; return (final_qtvs, binds, []) }

  | null sigs	-- UNRESTRICTED CASE, NO TYPE SIGS
  = tcSimplifyInfer doc tau_tvs lie_req

  | otherwise	-- UNRESTRICTED CASE, WITH TYPE SIGS
  = do	{ sig_lie <- unifyCtxts sigs	-- sigs is non-empty
	; let	-- The "sig_avails" is the stuff available.  We get that from
		-- the context of the type signature, BUT ALSO the lie_avail
		-- so that polymorphic recursion works right (see Note [Polymorphic recursion])
		local_meths = [mkMethInst sig mono_id | (_, Just sig, mono_id) <- mono_infos]
		sig_avails = sig_lie ++ local_meths

	-- Check that the needed dicts can be
	-- expressed in terms of the signature ones
	; (forall_tvs, dict_binds) <- tcSimplifyInferCheck doc tau_tvs sig_avails lie_req
	
   	-- Check that signature type variables are OK
	; final_qtvs <- checkSigsTyVars forall_tvs sigs

	; returnM (final_qtvs, dict_binds, map instToId sig_lie) }
  where
    bndrs   = bndrNames mono_infos
    sigs    = [sig | (_, Just sig, _) <- mono_infos]
    tau_tvs = foldr (unionVarSet . tyVarsOfType . getMonoType) emptyVarSet mono_infos
    is_mono_sig sig = null (sig_theta sig)
    doc = ptext SLIT("type signature(s) for") <+> pprBinders bndrs

    mkMethInst (TcSigInfo { sig_id = poly_id, sig_tvs = tvs, 
		            sig_theta = theta, sig_tau = tau, sig_loc = loc }) mono_id
      = Method mono_id poly_id (mkTyVarTys tvs) theta tau loc


-- Check that all the signature contexts are the same
-- The type signatures on a mutually-recursive group of definitions
-- must all have the same context (or none).
--
-- The trick here is that all the signatures should have the same
-- context, and we want to share type variables for that context, so that
-- all the right hand sides agree a common vocabulary for their type
-- constraints
--
-- We unify them because, with polymorphic recursion, their types
-- might not otherwise be related.  This is a rather subtle issue.
unifyCtxts :: [TcSigInfo] -> TcM [Inst]
unifyCtxts (sig1 : sigs) 	-- Argument is always non-empty
  = do	{ mapM unify_ctxt sigs
	; newDictsAtLoc (sig_loc sig1) (sig_theta sig1) }
  where
    theta1 = sig_theta sig1
    unify_ctxt :: TcSigInfo -> TcM ()
    unify_ctxt sig@(TcSigInfo { sig_theta = theta })
	= setSrcSpan (instLocSrcSpan (sig_loc sig)) 	$
	  addErrCtxt (sigContextsCtxt sig1 sig)		$
	  unifyTheta theta1 theta

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
		     <+> quotes (ppr tidy_tv2))
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

Note [Polymorphic recursion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
		Signatures
%*									*
%************************************************************************

Type signatures are tricky.  See Note [Signature skolems] in TcType

\begin{code}
tcTySigs :: [LSig Name] -> TcM [TcSigInfo]
tcTySigs sigs = do { mb_sigs <- mappM tcTySig (filter isVanillaLSig sigs)
		   ; return (catMaybes mb_sigs) }

tcTySig :: LSig Name -> TcM (Maybe TcSigInfo)
tcTySig (L span (TypeSig (L _ name) ty))
  = recoverM (return Nothing)	$
    setSrcSpan span		$
    do	{ sigma_ty <- tcHsSigType (FunSigCtxt name) ty
	; (tvs, theta, tau) <- tcInstSigType name scoped_names sigma_ty
	; loc <- getInstLoc (SigOrigin (SigSkol name))
	; return (Just (TcSigInfo { sig_id = mkLocalId name sigma_ty, 
				    sig_tvs = tvs, sig_theta = theta, sig_tau = tau, 
				    sig_scoped = scoped_names, sig_loc = loc })) }
  where
	-- The scoped names are the ones explicitly mentioned
	-- in the HsForAll.  (There may be more in sigma_ty, because
	-- of nested type synonyms.  See Note [Scoped] with TcSigInfo.)
    scoped_names = case ty of
			L _ (HsForAllTy Explicit tvs _ _) -> hsLTyVarNames tvs
			other 		    		  -> []

isUnRestrictedGroup :: [LHsBind Name] -> TcSigFun -> TcM Bool
isUnRestrictedGroup binds sig_fn
  = do	{ mono_restriction <- doptM Opt_MonomorphismRestriction
	; return (not mono_restriction || all_unrestricted) }
  where 
    all_unrestricted = all (unrestricted . unLoc) binds
    has_sig n = isJust (sig_fn n)

    unrestricted (PatBind other _ _ _)   = False
    unrestricted (VarBind v _)	         = has_sig v
    unrestricted (FunBind v _ matches _) = unrestricted_match matches 
					 || has_sig (unLoc v)

    unrestricted_match (MatchGroup (L _ (Match [] _ _) : _) _) = False
	-- No args => like a pattern binding
    unrestricted_match other	          = True
	-- Some args => a function binding
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

badUnliftedSig sig
  = hang (ptext SLIT("Illegal polymorphic signature in an unlifted binding"))
	 4 (ppr sig)

-----------------------------------------------
unboxedTupleErr name ty
  = hang (ptext SLIT("Illegal binding of unboxed tuple"))
	 4 (ppr name <+> dcolon <+> ppr ty)

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
