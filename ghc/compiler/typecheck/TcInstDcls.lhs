%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcInstDecls]{Typechecking instance declarations}

\begin{code}
#include "HsVersions.h"

module TcInstDcls (
	tcInstDecls1,
	tcInstDecls2
    ) where


IMP_Ubiq()

import HsSyn		( HsDecl(..), InstDecl(..), TyDecl, ClassDecl, DefaultDecl,
			  FixityDecl, IfaceSig, Sig(..),
			  SpecInstSig(..), HsBinds(..),
			  MonoBinds(..), GRHSsAndBinds(..), GRHS(..), Match, 
			  InPat(..), OutPat(..), HsExpr(..), HsLit(..),
			  Stmt, DoOrListComp, ArithSeqInfo, Fake, Fixity,
			  HsType(..), HsTyVar,
			  SYN_IE(RecFlag), recursive, nonRecursive, collectMonoBinders,
			  andMonoBinds
			)
import RnHsSyn		( SYN_IE(RenamedHsBinds), SYN_IE(RenamedMonoBinds),
			  SYN_IE(RenamedInstDecl), SYN_IE(RenamedFixityDecl), SYN_IE(RenamedHsExpr),
			  SYN_IE(RenamedSig), SYN_IE(RenamedSpecInstSig), SYN_IE(RenamedHsDecl)
			)
import TcHsSyn		( SYN_IE(TcHsBinds),
			  SYN_IE(TcMonoBinds), SYN_IE(TcExpr), tcIdType,
			  mkHsTyLam, mkHsTyApp,
			  mkHsDictLam, mkHsDictApp )

import TcBinds		( tcPragmaSigs )
import TcClassDcl	( tcMethodBind, badMethodErr )
import TcMonad
import RnMonad		( SYN_IE(RnNameSupply) )
import Inst		( Inst, InstOrigin(..), SYN_IE(InstanceMapper),
			  instToId, newDicts, newMethod, SYN_IE(LIE), emptyLIE, plusLIE )
import PragmaInfo	( PragmaInfo(..) )
import TcDeriv		( tcDeriving )
import TcEnv		( tcLookupClass, newLocalId, tcExtendGlobalTyVars, tcGetGlobalTyVars,
			  tcExtendGlobalValEnv, tcAddImportedIdInfo
			)
import SpecEnv		( SpecEnv )
import TcGRHSs		( tcGRHSsAndBinds )
import TcInstUtil	( InstInfo(..), mkInstanceRelatedIds, buildInstanceEnvs )
import TcKind		( TcKind, unifyKind )
import TcMatches	( tcMatchesFun )
import TcMonoType	( tcTyVarScope, tcContext, tcHsTypeKind )
import TcSimplify	( tcSimplifyAndCheck )
import TcType		( TcIdOcc(..), SYN_IE(TcIdBndr), SYN_IE(TcType), SYN_IE(TcTyVar), SYN_IE(TcTyVarSet), 
			  tcInstSigTyVars, tcInstType, tcInstSigTcType, 
			  tcInstTheta, tcInstTcType, tcInstSigType
			)
import Unify		( unifyTauTy, unifyTauTyLists )


import Bag		( emptyBag, unitBag, unionBags, unionManyBags,
			  concatBag, foldBag, bagToList, listToBag,
			  Bag )
import CmdLineOpts	( opt_GlasgowExts,
			  opt_PprUserLength, opt_SpecialiseOverloaded,
			  opt_WarnMissingMethods
			)
import Class		( GenClass,
			  classBigSig,
			  classDefaultMethodId, SYN_IE(Class)
			  )
import Id		( GenId, idType, replacePragmaInfo,
			  isNullaryDataCon, dataConArgTys, SYN_IE(Id) )
import ListSetOps	( minusList )
import Maybes 		( maybeToBool, expectJust, seqMaybe, catMaybes )
import Name		( nameOccName, getSrcLoc, mkLocalName,
			  isLocallyDefined, OccName, Name{--O only-}, SYN_IE(Module),
			  NamedThing(..)
			)
import PrelVals		( nO_EXPLICIT_METHOD_ERROR_ID, nO_DEFAULT_METHOD_ERROR_ID )
import PprType		( GenType, GenTyVar, GenClass, TyCon,
			  pprParendGenType
			)
import Outputable
import SrcLoc		( SrcLoc, noSrcLoc )
import Pretty
import TyCon		( isSynTyCon, isDataTyCon, derivedClasses )
import Type		( GenType(..), SYN_IE(ThetaType), mkTyVarTys, isPrimType,
			  splitSigmaTy, splitAppTys, isTyVarTy, matchTy, mkSigmaTy,
			  getTyCon_maybe, maybeAppTyCon, SYN_IE(Type), getTyVar,
			  maybeBoxedPrimType, maybeAppDataTyCon, splitRhoTy, eqTy
			)
import TyVar		( GenTyVar, SYN_IE(GenTyVarSet), tyVarSetToList, 
		          mkTyVarSet, unionTyVarSets, SYN_IE(TyVar) )
import TysPrim		( byteArrayPrimTyCon, mutableByteArrayPrimTyCon )
import TysWiredIn	( stringTy )
import Unique		( Unique, cCallableClassKey, cReturnableClassKey, Uniquable(..) )
import Util		( zipEqual, panic, pprPanic, pprTrace, removeDups, Ord3(..)
#if __GLASGOW_HASKELL__ < 202
		          , trace 
#endif
			)
\end{code}

Typechecking instance declarations is done in two passes. The first
pass, made by @tcInstDecls1@, collects information to be used in the
second pass.

This pre-processed info includes the as-yet-unprocessed bindings
inside the instance declaration.  These are type-checked in the second
pass, when the class-instance envs and GVE contain all the info from
all the instance and value decls.  Indeed that's the reason we need
two passes over the instance decls.


Here is the overall algorithm.
Assume that we have an instance declaration

    instance c => k (t tvs) where b

\begin{enumerate}
\item
$LIE_c$ is the LIE for the context of class $c$
\item
$betas_bar$ is the free variables in the class method type, excluding the
   class variable
\item
$LIE_cop$ is the LIE constraining a particular class method
\item
$tau_cop$ is the tau type of a class method
\item
$LIE_i$ is the LIE for the context of instance $i$
\item
$X$ is the instance constructor tycon
\item
$gammas_bar$ is the set of type variables of the instance
\item
$LIE_iop$ is the LIE for a particular class method instance
\item
$tau_iop$ is the tau type for this instance of a class method
\item
$alpha$ is the class variable
\item
$LIE_cop' = LIE_cop [X gammas_bar / alpha, fresh betas_bar]$
\item
$tau_cop' = tau_cop [X gammas_bar / alpha, fresh betas_bar]$
\end{enumerate}

ToDo: Update the list above with names actually in the code.

\begin{enumerate}
\item
First, make the LIEs for the class and instance contexts, which means
instantiate $thetaC [X inst_tyvars / alpha ]$, yielding LIElistC' and LIEC',
and make LIElistI and LIEI.
\item
Then process each method in turn.
\item
order the instance methods according to the ordering of the class methods
\item
express LIEC' in terms of LIEI, yielding $dbinds_super$ or an error
\item
Create final dictionary function from bindings generated already
\begin{pseudocode}
df = lambda inst_tyvars
       lambda LIEI
	 let Bop1
	     Bop2
	     ...
	     Bopn
	 and dbinds_super
	      in <op1,op2,...,opn,sd1,...,sdm>
\end{pseudocode}
Here, Bop1 \ldots Bopn bind the methods op1 \ldots opn,
and $dbinds_super$ bind the superclass dictionaries sd1 \ldots sdm.
\end{enumerate}

\begin{code}
tcInstDecls1 :: TcEnv s			-- Contains IdInfo for dfun ids
	     -> [RenamedHsDecl]
	     -> Module			-- module name for deriving
	     -> RnNameSupply			-- for renaming derivings
	     -> TcM s (Bag InstInfo,
		       RenamedHsBinds,
		       PprStyle -> Doc)

tcInstDecls1 unf_env decls mod_name rn_name_supply
  = 	-- Do the ordinary instance declarations
    mapNF_Tc (tcInstDecl1 unf_env mod_name) 
	     [inst_decl | InstD inst_decl <- decls]	`thenNF_Tc` \ inst_info_bags ->
    let
	decl_inst_info = unionManyBags inst_info_bags
    in
	-- Handle "derived" instances; note that we only do derivings
	-- for things in this module; we ignore deriving decls from
	-- interfaces!
    tcDeriving mod_name rn_name_supply decl_inst_info
		    	`thenTc` \ (deriv_inst_info, deriv_binds, ddump_deriv) ->

    let
	full_inst_info = deriv_inst_info `unionBags` decl_inst_info
    in
    returnTc (full_inst_info, deriv_binds, ddump_deriv)


tcInstDecl1 :: TcEnv s -> Module -> RenamedInstDecl -> NF_TcM s (Bag InstInfo)

tcInstDecl1 unf_env mod_name (InstDecl poly_ty binds uprags (Just dfun_name) src_loc)
  = 	-- Prime error recovery, set source location
    recoverNF_Tc (returnNF_Tc emptyBag)	$
    tcAddSrcLoc src_loc			$

	-- Look things up
    tcLookupClass class_name		`thenTc` \ (clas_kind, clas) ->

	-- Typecheck the context and instance type
    tcTyVarScope tyvar_names (\ tyvars ->
	tcContext context		`thenTc` \ theta ->
	tcHsTypeKind inst_ty		`thenTc` \ (tau_kind, tau) ->
	unifyKind clas_kind tau_kind	`thenTc_`
	returnTc (tyvars, theta, tau)
    )					`thenTc` \ (inst_tyvars, inst_theta, inst_tau) ->

	-- Check for respectable instance type
    scrutiniseInstanceType dfun_name clas inst_tau
					`thenTc` \ (inst_tycon,arg_tys) ->

	-- Make the dfun id and constant-method ids
    let
	(dfun_id, dfun_theta) = mkInstanceRelatedIds dfun_name
				         clas inst_tyvars inst_tau inst_theta
	-- Add info from interface file
	final_dfun_id = tcAddImportedIdInfo unf_env dfun_id
    in
    returnTc (unitBag (InstInfo clas inst_tyvars inst_tau inst_theta	
			dfun_theta final_dfun_id
			     	binds src_loc uprags))
  where
    (tyvar_names, context, dict_ty) = case poly_ty of
					HsForAllTy tvs cxt dict_ty -> (tvs, cxt, dict_ty)
					other			   -> ([],  [],  poly_ty)
    (class_name, inst_ty) = case dict_ty of
				MonoDictTy cls ty -> (cls,ty)
				other -> pprPanic "Malformed instance decl" (ppr PprDebug poly_ty)
\end{code}


%************************************************************************
%*									*
\subsection{Type-checking instance declarations, pass 2}
%*									*
%************************************************************************

\begin{code}
tcInstDecls2 :: Bag InstInfo
	     -> NF_TcM s (LIE s, TcMonoBinds s)

tcInstDecls2 inst_decls
  = foldBag combine tcInstDecl2 (returnNF_Tc (emptyLIE, EmptyMonoBinds)) inst_decls
  where
    combine tc1 tc2 = tc1 	`thenNF_Tc` \ (lie1, binds1) ->
		      tc2	`thenNF_Tc` \ (lie2, binds2) ->
		      returnNF_Tc (lie1 `plusLIE` lie2,
				   binds1 `AndMonoBinds` binds2)
\end{code}


======= New documentation starts here (Sept 92)	 ==============

The main purpose of @tcInstDecl2@ is to return a @HsBinds@ which defines
the dictionary function for this instance declaration.	For example
\begin{verbatim}
	instance Foo a => Foo [a] where
		op1 x = ...
		op2 y = ...
\end{verbatim}
might generate something like
\begin{verbatim}
	dfun.Foo.List dFoo_a = let op1 x = ...
				   op2 y = ...
			       in
				   Dict [op1, op2]
\end{verbatim}

HOWEVER, if the instance decl has no context, then it returns a
bigger @HsBinds@ with declarations for each method.  For example
\begin{verbatim}
	instance Foo [a] where
		op1 x = ...
		op2 y = ...
\end{verbatim}
might produce
\begin{verbatim}
	dfun.Foo.List a = Dict [Foo.op1.List a, Foo.op2.List a]
	const.Foo.op1.List a x = ...
	const.Foo.op2.List a y = ...
\end{verbatim}
This group may be mutually recursive, because (for example) there may
be no method supplied for op2 in which case we'll get
\begin{verbatim}
	const.Foo.op2.List a = default.Foo.op2 (dfun.Foo.List a)
\end{verbatim}
that is, the default method applied to the dictionary at this type.

What we actually produce in either case is:

	AbsBinds [a] [dfun_theta_dicts]
		 [(dfun.Foo.List, d)] ++ (maybe) [(const.Foo.op1.List, op1), ...]
		 { d = (sd1,sd2, ..., op1, op2, ...)
		   op1 = ...
		   op2 = ...
	 	 }

The "maybe" says that we only ask AbsBinds to make global constant methods
if the dfun_theta is empty.

		
For an instance declaration, say,

	instance (C1 a, C2 b) => C (T a b) where
		...

where the {\em immediate} superclasses of C are D1, D2, we build a dictionary
function whose type is

	(C1 a, C2 b, D1 (T a b), D2 (T a b)) => C (T a b)

Notice that we pass it the superclass dictionaries at the instance type; this
is the ``Mark Jones optimisation''.  The stuff before the "=>" here
is the @dfun_theta@ below.

First comes the easy case of a non-local instance decl.

\begin{code}
tcInstDecl2 :: InstInfo -> NF_TcM s (LIE s, TcMonoBinds s)

tcInstDecl2 (InstInfo clas inst_tyvars inst_ty
		      inst_decl_theta dfun_theta
		      dfun_id monobinds
		      locn uprags)
  | not (isLocallyDefined dfun_id)
  = returnNF_Tc (emptyLIE, EmptyMonoBinds)

{-
  -- I deleted this "optimisation" because when importing these
  -- instance decls the renamer would look for the dfun bindings and they weren't there.
  -- This would be fixable, but it seems simpler just to produce a tiny void binding instead,
  -- even though it's never used.

	-- This case deals with CCallable etc, which don't need any bindings
  | isNoDictClass clas			
  = returnNF_Tc (emptyLIE, EmptyBinds)
-}

  | otherwise
  =	 -- Prime error recovery
    recoverNF_Tc (returnNF_Tc (emptyLIE, EmptyMonoBinds))  $
    tcAddSrcLoc locn					   $

	-- Get the class signature
    tcInstSigTyVars inst_tyvars		`thenNF_Tc` \ (inst_tyvars', _, tenv) ->
    let 
	origin = InstanceDeclOrigin
        (class_tyvar,
	 super_classes, sc_sel_ids,
	 op_sel_ids, defm_ids) = classBigSig clas
    in
    tcInstType tenv inst_ty		`thenNF_Tc` \ inst_ty' ->
    tcInstTheta tenv dfun_theta		`thenNF_Tc` \ dfun_theta' ->
    tcInstTheta tenv inst_decl_theta	`thenNF_Tc` \ inst_decl_theta' ->
    let
	sc_theta'        = super_classes `zip` repeat inst_ty'
    in
	 -- Create dictionary Ids from the specified instance contexts.
    newDicts origin sc_theta'		`thenNF_Tc` \ (sc_dicts,        sc_dict_ids) ->
    newDicts origin dfun_theta'		`thenNF_Tc` \ (dfun_arg_dicts,  dfun_arg_dicts_ids)  ->
    newDicts origin inst_decl_theta'	`thenNF_Tc` \ (inst_decl_dicts, _) ->
    newDicts origin [(clas,inst_ty')]	`thenNF_Tc` \ (this_dict,       [this_dict_id]) ->

	-- Now process any INLINE or SPECIALIZE pragmas for the methods
	-- ...[NB May 97; all ignored except INLINE]
    tcPragmaSigs uprags		`thenTc` \ (prag_fn, spec_binds, spec_lie) ->

	 -- Check that all the method bindings come from this class
    let
	inst_tyvars_set' = mkTyVarSet inst_tyvars'
	check_from_this_class (bndr, loc)
	  | nameOccName bndr `elem` sel_names = returnTc ()
	  | otherwise			      = recoverTc (returnTc ()) $
						tcAddSrcLoc loc $
						failTc (badMethodErr bndr clas)
	sel_names = map getOccName op_sel_ids
    in
    mapTc check_from_this_class (bagToList (collectMonoBinders monobinds))	`thenTc_`

	  -- Type check the method bindings themselves
    tcExtendGlobalTyVars inst_tyvars_set' (
        tcExtendGlobalValEnv (catMaybes defm_ids) $
		-- Default-method Ids may be mentioned in synthesised RHSs 

	mapAndUnzip3Tc (tcInstMethodBind clas inst_ty' monobinds) 
		       (op_sel_ids `zip` defm_ids)
    )		 	`thenTc` \ (method_binds_s, insts_needed_s, meth_lies_w_ids) ->

	-- Check the overloading constraints of the methods and superclasses
    let
	(meth_lies, meth_ids) = unzip meth_lies_w_ids
	avail_insts	 -- These insts are in scope; quite a few, eh?
	  = this_dict `plusLIE` dfun_arg_dicts `plusLIE`  unionManyBags meth_lies
    in
    tcAddErrCtxt bindSigCtxt (
        tcSimplifyAndCheck
		 inst_tyvars_set'			-- Local tyvars
		 avail_insts
		 (sc_dicts `unionBags` 
		  unionManyBags insts_needed_s)		-- Need to get defns for all these
    )					 `thenTc` \ (const_lie, super_binds) ->

	-- Check that we *could* construct the superclass dictionaries,
	-- even though we are *actually* going to pass the superclass dicts in;
	-- the check ensures that the caller will never have a problem building
	-- them.
    tcAddErrCtxt superClassSigCtxt (
        tcSimplifyAndCheck
		 inst_tyvars_set'		-- Local tyvars
		 inst_decl_dicts		-- The instance dictionaries available
		 sc_dicts			-- The superclass dicationaries reqd
    )					`thenTc_`
						-- Ignore the result; we're only doing
						-- this to make sure it can be done.

	-- Create the result bindings
    let
	dict_bind    = VarMonoBind this_dict_id (Dictionary sc_dict_ids meth_ids)
	method_binds = andMonoBinds method_binds_s

	main_bind
	  = AbsBinds
		 inst_tyvars'
		 dfun_arg_dicts_ids
		 [(inst_tyvars', RealId dfun_id, this_dict_id)] 
		 (super_binds	`AndMonoBinds` 
		  method_binds	`AndMonoBinds`
		  dict_bind)
    in
    returnTc (const_lie `plusLIE` spec_lie,
	      main_bind `AndMonoBinds` spec_binds)
\end{code}


%************************************************************************
%*									*
\subsection{Processing each method}
%*									*
%************************************************************************

\begin{code}
tcInstMethodBind 
	:: Class
	-> TcType s					-- Instance type
	-> RenamedMonoBinds				-- Method binding
	-> (Id, Maybe Id)				-- Selector id and default-method id
	-> TcM s (TcMonoBinds s, LIE s, (LIE s, TcIdOcc s))

tcInstMethodBind clas inst_ty meth_binds (sel_id, maybe_dm_id)
  = tcGetSrcLoc			`thenNF_Tc` \ loc ->
    tcGetUnique			`thenNF_Tc` \ uniq ->
    let
	meth_occ	  = getOccName sel_id
	default_meth_name = mkLocalName uniq meth_occ loc
	maybe_meth_bind   = find meth_occ meth_binds 
        the_meth_bind     = case maybe_meth_bind of
				  Just stuff -> stuff
				  Nothing    -> mk_default_bind default_meth_name
    in

	-- Warn if no method binding, only if -fwarn-missing-methods
    
    warnTc (opt_WarnMissingMethods && 
	    not (maybeToBool maybe_meth_bind) &&
	    not (maybeToBool maybe_dm_id))	
	(omittedMethodWarn sel_id clas)		`thenNF_Tc_`

	-- Typecheck the method binding
    tcMethodBind clas origin inst_ty sel_id the_meth_bind
  where
    origin = InstanceDeclOrigin 	-- Poor

    find occ EmptyMonoBinds 	  = Nothing
    find occ (AndMonoBinds b1 b2) = find occ b1 `seqMaybe` find occ b2

    find occ b@(FunMonoBind op_name _ _ _)          | nameOccName op_name == occ = Just b
						    | otherwise		  = Nothing
    find occ b@(PatMonoBind (VarPatIn op_name) _ _) | nameOccName op_name == occ = Just b
						    | otherwise		  = Nothing
    find occ other = panic "Urk! Bad instance method binding"


    mk_default_bind local_meth_name
      = PatMonoBind (VarPatIn local_meth_name)
		    (GRHSsAndBindsIn [OtherwiseGRHS default_expr noSrcLoc] EmptyBinds)
		    noSrcLoc

    default_expr = case maybe_dm_id of
			Just dm_id -> HsVar (getName dm_id)	-- There's a default method
			Nothing    -> error_expr		-- No default method

    error_expr = HsApp (HsVar (getName nO_DEFAULT_METHOD_ERROR_ID)) 
			      (HsLit (HsString (_PK_ error_msg)))

    error_msg = show (hcat [ppr (PprForUser opt_PprUserLength) (getSrcLoc sel_id), text "|", 
			    ppr (PprForUser opt_PprUserLength) sel_id
		])
\end{code}



%************************************************************************
%*									*
\subsection{Type-checking specialise instance pragmas}
%*									*
%************************************************************************

\begin{code}
{- LATER
tcSpecInstSigs :: E -> CE -> TCE
	       -> Bag InstInfo		-- inst decls seen (declared and derived)
	       -> [RenamedSpecInstSig]	-- specialise instance upragmas
	       -> TcM (Bag InstInfo)	-- new, overlapped, inst decls

tcSpecInstSigs e ce tce inst_infos []
  = returnTc emptyBag

tcSpecInstSigs e ce tce inst_infos sigs
  = buildInstanceEnvs inst_infos 	`thenTc`    \ inst_mapper ->
    tc_inst_spec_sigs inst_mapper sigs	`thenNF_Tc` \ spec_inst_infos ->
    returnTc spec_inst_infos
  where
    tc_inst_spec_sigs inst_mapper []
      = returnNF_Tc emptyBag
    tc_inst_spec_sigs inst_mapper (sig:sigs)
      = tcSpecInstSig e ce tce inst_infos inst_mapper sig	`thenNF_Tc` \ info_sig ->
	tc_inst_spec_sigs inst_mapper sigs			`thenNF_Tc` \ info_sigs ->
	returnNF_Tc (info_sig `unionBags` info_sigs)

tcSpecInstSig :: E -> CE -> TCE
	      -> Bag InstInfo
	      -> InstanceMapper
	      -> RenamedSpecInstSig
	      -> NF_TcM (Bag InstInfo)

tcSpecInstSig e ce tce inst_infos inst_mapper (SpecInstSig class_name ty src_loc)
  = recoverTc emptyBag			(
    tcAddSrcLoc src_loc			(
    let
	clas = lookupCE ce class_name -- Renamer ensures this can't fail

	-- Make some new type variables, named as in the specialised instance type
	ty_names 			  = extractHsTyNames ???is_tyvarish_name??? ty
	(tmpl_e,inst_tmpls,inst_tmpl_tys) = mkTVE ty_names
    in
    babyTcMtoTcM (tcInstanceType ce tce tmpl_e True src_loc ty)
				`thenTc` \ inst_ty ->
    let
	maybe_tycon = case maybeAppDataTyCon inst_ty of
			 Just (tc,_,_) -> Just tc
			 Nothing       -> Nothing

	maybe_unspec_inst = lookup_unspec_inst clas maybe_tycon inst_infos
    in
	-- Check that we have a local instance declaration to specialise
    checkMaybeTc maybe_unspec_inst
	    (specInstUnspecInstNotFoundErr clas inst_ty src_loc)  `thenTc_`

	-- Create tvs to substitute for tmpls while simplifying the context
    copyTyVars inst_tmpls	`thenNF_Tc` \ (tv_e, inst_tvs, inst_tv_tys) ->
    let
	Just (InstInfo _ unspec_tyvars unspec_inst_ty unspec_theta
		       _ _ binds _ uprag) = maybe_unspec_inst

    	subst = case matchTy unspec_inst_ty inst_ty of
		     Just subst -> subst
		     Nothing    -> panic "tcSpecInstSig:matchTy"

	subst_theta    = instantiateThetaTy subst unspec_theta
	subst_tv_theta = instantiateThetaTy tv_e subst_theta

	mk_spec_origin clas ty
	  = InstanceSpecOrigin inst_mapper clas ty src_loc
	-- I'm VERY SUSPICIOUS ABOUT THIS
	-- the inst-mapper is in a knot at this point so it's no good
	-- looking at it in tcSimplify...
    in
    tcSimplifyThetas mk_spec_origin subst_tv_theta
				`thenTc` \ simpl_tv_theta ->
    let
	simpl_theta = [ (clas, tv_to_tmpl tv) | (clas, tv) <- simpl_tv_theta ]

	tv_tmpl_map   = zipEqual "tcSpecInstSig" inst_tv_tys inst_tmpl_tys
	tv_to_tmpl tv = assoc "tcSpecInstSig" tv_tmpl_map tv
    in
    mkInstanceRelatedIds 
			 clas inst_tmpls inst_ty simpl_theta uprag
				`thenNF_Tc` \ (dfun_id, dfun_theta, const_meth_ids) ->

    getSwitchCheckerTc		`thenNF_Tc` \ sw_chkr ->
    (if sw_chkr SpecialiseTrace then
	pprTrace "Specialised Instance: "
	(vcat [hsep [if null simpl_theta then empty else ppr PprDebug simpl_theta,
			  if null simpl_theta then empty else ptext SLIT("=>"),
			  ppr PprDebug clas,
			  pprParendGenType PprDebug inst_ty],
		   hsep [ptext SLIT("        derived from:"),
			  if null unspec_theta then empty else ppr PprDebug unspec_theta,
			  if null unspec_theta then empty else ptext SLIT("=>"),
			  ppr PprDebug clas,
			  pprParendGenType PprDebug unspec_inst_ty]])
    else id) (

    returnTc (unitBag (InstInfo clas inst_tmpls inst_ty simpl_theta
				dfun_theta dfun_id
				binds src_loc uprag))
    )))


lookup_unspec_inst clas maybe_tycon inst_infos
  = case filter (match_info match_inst_ty) (bagToList inst_infos) of
	[]       -> Nothing
	(info:_) -> Just info
  where
    match_info match_ty (InstInfo inst_clas _ inst_ty _ _ _ _ _ from_here _ _ _)
      = from_here && clas == inst_clas &&
	match_ty inst_ty && is_plain_instance inst_ty

    match_inst_ty = case maybe_tycon of
		      Just tycon -> match_tycon tycon
		      Nothing    -> match_fun

    match_tycon tycon inst_ty = case (maybeAppDataTyCon inst_ty) of
	  Just (inst_tc,_,_) -> tycon == inst_tc
	  Nothing            -> False

    match_fun inst_ty = isFunType inst_ty


is_plain_instance inst_ty
  = case (maybeAppDataTyCon inst_ty) of
      Just (_,tys,_) -> all isTyVarTemplateTy tys
      Nothing	     -> case maybeUnpackFunTy inst_ty of
			  Just (arg, res) -> isTyVarTemplateTy arg && isTyVarTemplateTy res
			  Nothing	  -> error "TcInstDecls:is_plain_instance"
-}
\end{code}


Checking for a decent instance type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
@scrutiniseInstanceType@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere). In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.

\begin{code}
scrutiniseInstanceType dfun_name clas inst_tau
	-- TYCON CHECK
  | not (maybeToBool inst_tycon_maybe) || isSynTyCon inst_tycon
  = failTc (instTypeErr inst_tau)

  	-- IMPORTED INSTANCES ARE OK (but see tcInstDecl1)
  | not (isLocallyDefined dfun_name)
  = returnTc (inst_tycon,arg_tys)

	-- TYVARS CHECK
  | not (opt_GlasgowExts ||
	 (all isTyVarTy arg_tys && null tyvar_dups)
    )
  = failTc (instTypeErr inst_tau)

  	-- DERIVING CHECK
	-- It is obviously illegal to have an explicit instance
	-- for something that we are also planning to `derive'
	-- Though we can have an explicit instance which is more
	-- specific than the derived instance
  | clas `elem` (derivedClasses inst_tycon)
    && all isTyVarTy arg_tys
  = failTc (derivingWhenInstanceExistsErr clas inst_tycon)

  |	-- CCALL CHECK
	-- To verify that a user declaration of a CCallable/CReturnable 
	-- instance is OK, we must be able to see the constructor(s)
	-- of the instance type (see next guard.)
	--  
        -- We flag this separately to give a more precise error msg.
        --
    (uniqueOf clas == cCallableClassKey   && not constructors_visible) ||
    (uniqueOf clas == cReturnableClassKey && not constructors_visible)
  = failTc (invisibleDataConPrimCCallErr clas inst_tau)

  |	-- CCALL CHECK
	-- A user declaration of a CCallable/CReturnable instance
	-- must be for a "boxed primitive" type.
    (uniqueOf clas == cCallableClassKey   && not (ccallable_type   inst_tau)) ||
    (uniqueOf clas == cReturnableClassKey && not (creturnable_type inst_tau))
  = failTc (nonBoxedPrimCCallErr clas inst_tau)

  | otherwise
  = returnTc (inst_tycon,arg_tys)

  where
    (possible_tycon, arg_tys) = splitAppTys inst_tau
    inst_tycon_maybe	      = getTyCon_maybe possible_tycon
    inst_tycon 		      = expectJust "tcInstDecls1:inst_tycon" inst_tycon_maybe
    (_, tyvar_dups)	      = removeDups cmp (map (getTyVar "tcInstDecls1:getTyVarTy") arg_tys)

    constructors_visible      =
        case maybeAppDataTyCon inst_tau of
           Just (_,_,[])   -> False
	   everything_else -> True

-- These conditions come directly from what the DsCCall is capable of.
-- Totally grotesque.  Green card should solve this.

ccallable_type   ty = isPrimType ty ||				-- Allow CCallable Int# etc
                      maybeToBool (maybeBoxedPrimType ty) ||	-- Ditto Int etc
		      ty `eqTy` stringTy ||
		      byte_arr_thing
  where
    byte_arr_thing = case maybeAppDataTyCon ty of
			Just (tycon, ty_args, [data_con]) | isDataTyCon tycon -> 
		     		length data_con_arg_tys == 2 &&
				maybeToBool maybe_arg2_tycon &&
				(arg2_tycon == byteArrayPrimTyCon ||
				 arg2_tycon == mutableByteArrayPrimTyCon)
			     where
				data_con_arg_tys = dataConArgTys data_con ty_args
				(data_con_arg_ty1 : data_con_arg_ty2 : _) = data_con_arg_tys
				maybe_arg2_tycon = maybeAppTyCon data_con_arg_ty2
				Just (arg2_tycon,_) = maybe_arg2_tycon

			other -> False

creturnable_type ty = maybeToBool (maybeBoxedPrimType ty) ||
			-- Or, a data type with a single nullary constructor
		      case (maybeAppDataTyCon ty) of
			Just (tycon, tys_applied, [data_con])
				-> isNullaryDataCon data_con
			other -> False
\end{code}

\begin{code}

instTypeErr ty sty
  = case ty of
      SynTy tc _ _ -> hsep [ptext SLIT("The type synonym"), ppr sty tc, rest_of_msg]
      TyVarTy tv   -> hsep [ptext SLIT("The type variable"), ppr sty tv, rest_of_msg]
      other	   -> sep [ptext SLIT("The type"), nest 4 (ppr sty ty), rest_of_msg]
  where
    rest_of_msg = ptext SLIT("cannot be used as an instance type")

derivingWhenInstanceExistsErr clas tycon sty
  = hang (hsep [ptext SLIT("Deriving class"), 
		       ppr sty clas, 
		       ptext SLIT("type"), ppr sty tycon])
         4 (ptext SLIT("when an explicit instance exists"))

nonBoxedPrimCCallErr clas inst_ty sty
  = hang (ptext SLIT("Unacceptable instance type for ccall-ish class"))
	 4 (hsep [ ptext SLIT("class"), ppr sty clas, ptext SLIT("type"),
    		        ppr sty inst_ty])

{-
  Declaring CCallable & CReturnable instances in a module different
  from where the type was defined. Caused by importing data type
  abstractly (either programmatically or by the renamer being over-eager
  in its pruning.)
-}
invisibleDataConPrimCCallErr clas inst_ty sty
  = hang (hsep [(ppr sty inst_ty <> ptext SLIT("s constructors not visible when checking")),
                ppr sty clas, ptext SLIT("instance")])
        4 (hsep [ptext SLIT("(Try either importing"), ppr sty inst_ty, 
	         ptext SLIT("non-abstractly, or compile using -fno-prune-tydecls ..)")])

omittedMethodWarn sel_id clas sty
  = sep [ptext SLIT("Warning: no explicit method nor default method for") <+> ppr sty sel_id, 
	 ptext SLIT("in an instance declaration for") <+> ppr sty clas]

instMethodNotInClassErr occ clas sty
  = hang (ptext SLIT("Instance mentions a method not in the class"))
	 4 (hsep [ptext SLIT("class"), ppr sty clas, ptext SLIT("method"),
    		       ppr sty occ])

patMonoBindsCtxt pbind sty
  = hang (ptext SLIT("In a pattern binding:"))
	 4 (ppr sty pbind)

methodSigCtxt name ty sty
  = hang (hsep [ptext SLIT("When matching the definition of class method"),
	               ppr sty name, ptext SLIT("to its signature :") ])
	 4 (ppr sty ty)

bindSigCtxt sty
  = ptext SLIT("When checking methods of an instance declaration")

superClassSigCtxt sty
  = ptext SLIT("When checking superclass constraints of an instance declaration")
\end{code}
