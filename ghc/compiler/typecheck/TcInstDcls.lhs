%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcInstDecls]{Typechecking instance declarations}

\begin{code}
#include "HsVersions.h"

module TcInstDcls (
	tcInstDecls1,
	tcInstDecls2,
	processInstBinds
    ) where


import Ubiq

import HsSyn		( InstDecl(..), FixityDecl, Sig(..),
			  SpecInstSig(..), HsBinds(..), Bind(..),
			  MonoBinds(..), GRHSsAndBinds, Match, 
			  InPat(..), OutPat(..), HsExpr(..), HsLit(..),
			  Stmt, Qual, ArithSeqInfo, Fake,
			  PolyType(..), MonoType )
import RnHsSyn		( RenamedHsBinds(..), RenamedMonoBinds(..),
			  RenamedInstDecl(..), RenamedFixityDecl(..),
			  RenamedSig(..), RenamedSpecInstSig(..),
			  RnName(..){-incl instance Outputable-}
			)
import TcHsSyn		( TcIdOcc(..), TcHsBinds(..),
			  TcMonoBinds(..), TcExpr(..), tcIdType,
			  mkHsTyLam, mkHsTyApp,
			  mkHsDictLam, mkHsDictApp )


import TcMonad		hiding ( rnMtoTcM )
import GenSpecEtc	( checkSigTyVars )
import Inst		( Inst, InstOrigin(..), InstanceMapper(..),
			  newDicts, newMethod, LIE(..), emptyLIE, plusLIE )
import TcBinds		( tcPragmaSigs )
import TcDeriv		( tcDeriving )
import TcEnv		( tcLookupClass, tcTyVarScope, newLocalId )
import TcGRHSs		( tcGRHSsAndBinds )
import TcInstUtil	( InstInfo(..), mkInstanceRelatedIds, buildInstanceEnvs )
import TcKind		( TcKind, unifyKind )
import TcMatches	( tcMatchesFun )
import TcMonoType	( tcContext, tcMonoTypeKind )
import TcSimplify	( tcSimplifyAndCheck, tcSimplifyThetas )
import TcType		( TcType(..), TcTyVar(..),
			  tcInstSigTyVars, tcInstType, tcInstTheta
			)
import Unify		( unifyTauTy )


import Bag		( emptyBag, unitBag, unionBags, unionManyBags,
			  concatBag, foldBag, bagToList )
import CmdLineOpts	( opt_GlasgowExts, opt_CompilingPrelude,
			  opt_OmitDefaultInstanceMethods,
			  opt_SpecialiseOverloaded )
import Class		( GenClass, GenClassOp, 
			  isCcallishClass, classBigSig,
			  classOps, classOpLocalType,
			  classOpTagByString
			  )
import Id		( GenId, idType, isDefaultMethodId_maybe )
import ListSetOps	( minusList )
import Maybes 		( maybeToBool, expectJust )
import Name		( getLocalName, origName, nameOf, Name{--O only-} )
import PrelVals		( nO_EXPLICIT_METHOD_ERROR_ID )
import PrelMods		( pRELUDE )
import PprType		( GenType, GenTyVar, GenClass, GenClassOp, TyCon,
			  pprParendGenType
			)
import PprStyle
import Pretty
import RnUtils		( RnEnv(..) )
import TyCon		( isSynTyCon, derivedFor )
import Type		( GenType(..),  ThetaType(..), mkTyVarTys,
			  splitSigmaTy, splitAppTy, isTyVarTy, matchTy, mkSigmaTy,
			  getTyCon_maybe, maybeBoxedPrimType
			)
import TyVar		( GenTyVar, mkTyVarSet )
import TysWiredIn	( stringTy )
import Unique		( Unique )
import Util		( panic )
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
tcInstDecls1 :: Bag RenamedInstDecl
	     -> [RenamedSpecInstSig]
	     -> Module			-- module name for deriving
	     -> RnEnv			-- for renaming derivings
	     -> [RenamedFixityDecl]	-- fixities for deriving
	     -> TcM s (Bag InstInfo,
		       RenamedHsBinds,
		       PprStyle -> Pretty)

tcInstDecls1 inst_decls specinst_sigs mod_name rn_env fixities
  = 	-- Do the ordinary instance declarations
    mapBagNF_Tc (tcInstDecl1 mod_name) inst_decls
			`thenNF_Tc` \ inst_info_bags ->
    let
	decl_inst_info = concatBag inst_info_bags
    in
	-- Handle "derived" instances; note that we only do derivings
	-- for things in this module; we ignore deriving decls from
	-- interfaces! We pass fixities, because they may be used
	-- in deriving Read and Show.
    tcDeriving mod_name rn_env decl_inst_info fixities
		    	`thenTc` \ (deriv_inst_info, deriv_binds, ddump_deriv) ->

    let
	inst_info = deriv_inst_info `unionBags` decl_inst_info
    in
{- LATER
	-- Handle specialise instance pragmas
    tcSpecInstSigs inst_info specinst_sigs
			`thenTc` \ spec_inst_info ->
-}
    let
	spec_inst_info = emptyBag	-- For now

	full_inst_info = inst_info `unionBags` spec_inst_info
    in
    returnTc (full_inst_info, deriv_binds, ddump_deriv)


tcInstDecl1 :: FAST_STRING -> RenamedInstDecl -> NF_TcM s (Bag InstInfo)

tcInstDecl1 mod_name
	    (InstDecl class_name
		      poly_ty@(HsForAllTy tyvar_names context inst_ty)
		      binds
		      from_here inst_mod uprags pragmas src_loc)
  = 	-- Prime error recovery, set source location
    recoverNF_Tc (returnNF_Tc emptyBag)	$
    tcAddSrcLoc src_loc			$

	-- Look things up
    tcLookupClass class_name		`thenNF_Tc` \ (clas_kind, clas) ->

    let
	de_rn (RnName n) = n
    in
	-- Typecheck the context and instance type
    tcTyVarScope (map de_rn tyvar_names) (\ tyvars ->
	tcContext context		`thenTc` \ theta ->
	tcMonoTypeKind inst_ty		`thenTc` \ (tau_kind, tau) ->
	unifyKind clas_kind tau_kind	`thenTc_`
	returnTc (tyvars, theta, tau)
    )					`thenTc` \ (inst_tyvars, inst_theta, inst_tau) ->

	-- Check for respectable instance type
    scrutiniseInstanceType from_here clas inst_tau
					`thenTc` \ (inst_tycon,arg_tys) ->

	-- Deal with the case where we are deriving
	-- and importing the same instance
    if (not from_here && (clas `derivedFor` inst_tycon)
	              && all isTyVarTy arg_tys)
    then
	if not opt_CompilingPrelude && maybeToBool inst_mod &&
	   mod_name == expectJust "inst_mod" inst_mod
 	then
		-- Imported instance came from this module;
		-- discard and derive fresh instance
	    returnTc emptyBag		
	else
		-- Imported instance declared in another module;
		-- report duplicate instance error
	    failTc (derivingWhenInstanceImportedErr inst_mod clas inst_tycon)
    else

	-- Make the dfun id and constant-method ids
    mkInstanceRelatedIds from_here inst_mod pragmas
		         clas inst_tyvars inst_tau inst_theta uprags
					`thenTc` \ (dfun_id, dfun_theta, const_meth_ids) ->

    returnTc (unitBag (InstInfo clas inst_tyvars inst_tau inst_theta	
				dfun_theta dfun_id const_meth_ids
			     	binds from_here inst_mod src_loc uprags))
\end{code}


%************************************************************************
%*									*
\subsection{Type-checking instance declarations, pass 2}
%*									*
%************************************************************************

\begin{code}
tcInstDecls2 :: Bag InstInfo
	     -> NF_TcM s (LIE s, TcHsBinds s)

tcInstDecls2 inst_decls
  = foldBag combine tcInstDecl2 (returnNF_Tc (emptyLIE, EmptyBinds)) inst_decls
  where
    combine tc1 tc2 = tc1 	`thenNF_Tc` \ (lie1, binds1) ->
		      tc2	`thenNF_Tc` \ (lie2, binds2) ->
		      returnNF_Tc (lie1 `plusLIE` lie2,
				   binds1 `ThenBinds` binds2)
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
tcInstDecl2 :: InstInfo
	    -> NF_TcM s (LIE s, TcHsBinds s)

tcInstDecl2 (InstInfo _ _ _ _ _ _ _ _ False{-import-} _ _ _)
  = returnNF_Tc (emptyLIE, EmptyBinds)

tcInstDecl2 (InstInfo clas inst_tyvars inst_ty
		      inst_decl_theta dfun_theta
		      dfun_id const_meth_ids monobinds
		      True{-here-} inst_mod locn uprags)
  =	 -- Prime error recovery
    recoverNF_Tc (returnNF_Tc (emptyLIE, EmptyBinds)) 	$
    tcAddSrcLoc locn					$

	-- Get the class signature
    tcInstSigTyVars inst_tyvars		`thenNF_Tc` \ (inst_tyvars', _, tenv) ->
    let 
        (class_tyvar,
	 super_classes, sc_sel_ids,
	 class_ops, op_sel_ids, defm_ids) = classBigSig clas
    in
    tcInstType tenv inst_ty		`thenNF_Tc` \ inst_ty' ->
    tcInstTheta tenv dfun_theta		`thenNF_Tc` \ dfun_theta' ->
    tcInstTheta tenv inst_decl_theta	`thenNF_Tc` \ inst_decl_theta' ->
    let
	sc_theta'        = super_classes `zip` (repeat inst_ty')
 	origin    	 = InstanceDeclOrigin
	mk_method sel_id = newMethodId sel_id inst_ty' origin locn
    in
	 -- Create dictionary Ids from the specified instance contexts.
    newDicts origin sc_theta'		`thenNF_Tc` \ (sc_dicts,        sc_dict_ids) ->
    newDicts origin dfun_theta'		`thenNF_Tc` \ (dfun_arg_dicts,  dfun_arg_dicts_ids)  ->
    newDicts origin inst_decl_theta'	`thenNF_Tc` \ (inst_decl_dicts, _) ->
    newDicts origin [(clas,inst_ty')]	`thenNF_Tc` \ (this_dict,       [this_dict_id]) ->

	 -- Create method variables
    mapAndUnzipNF_Tc mk_method op_sel_ids	`thenNF_Tc` \ (meth_insts_s, meth_ids) ->

	 -- Collect available Insts
    let
	avail_insts	 -- These insts are in scope; quite a few, eh?
	  = unionManyBags (this_dict : dfun_arg_dicts : meth_insts_s) 

	mk_method_expr
	  = if opt_OmitDefaultInstanceMethods then
		makeInstanceDeclNoDefaultExpr     origin meth_ids defm_ids inst_ty' clas inst_mod
	    else
		makeInstanceDeclDefaultMethodExpr origin meth_ids defm_ids inst_ty' this_dict_id 
    in
    processInstBinds clas mk_method_expr inst_tyvars' avail_insts meth_ids monobinds
					 	`thenTc` \ (insts_needed, method_mbinds) ->
    let
	-- Create the dict and method binds
	dict_bind
	    = VarMonoBind this_dict_id (Dictionary sc_dict_ids meth_ids)

	dict_and_method_binds
	    = dict_bind `AndMonoBinds` method_mbinds

	inst_tyvars_set' = mkTyVarSet inst_tyvars'
    in
	-- Check the overloading constraints of the methods and superclasses
    tcAddErrCtxt (bindSigCtxt meth_ids) (
	tcSimplifyAndCheck
		 inst_tyvars_set'			-- Local tyvars
		 avail_insts
		 (sc_dicts `unionBags` insts_needed)	-- Need to get defns for all these
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

	-- Now process any SPECIALIZE pragmas for the methods
    let
	spec_sigs = [ s | s@(SpecSig _ _ _ _) <- uprags ]
    in
    tcPragmaSigs spec_sigs		`thenTc` \ (_, spec_binds, spec_lie) ->
    let
	-- Complete the binding group, adding any spec_binds
	inst_binds
	  = AbsBinds
		 inst_tyvars'
		 dfun_arg_dicts_ids
		 ((this_dict_id, RealId dfun_id) 
		  : (meth_ids `zip` (map RealId const_meth_ids)))
			-- const_meth_ids will often be empty
		 super_binds
		 (RecBind dict_and_method_binds)

	    `ThenBinds`
	    spec_binds
    in

    returnTc (const_lie `plusLIE` spec_lie, inst_binds)
\end{code}

@mkMethodId@ manufactures an id for a local method.
It's rather turgid stuff, because there are two cases:

  (a) For methods with no local polymorphism, we can make an Inst of the 
      class-op selector function and a corresp InstId; 
      which is good because then other methods which call
      this one will do so directly.

  (b) For methods with local polymorphism, we can't do this.  For example,

	 class Foo a where
		op :: (Num b) => a -> b -> a

      Here the type of the class-op-selector is

	forall a b. (Foo a, Num b) => a -> b -> a

      The locally defined method at (say) type Float will have type

	forall b. (Num b) => Float -> b -> Float

      and the one is not an instance of the other.

      So for these we just make a local (non-Inst) id with a suitable type.

How disgusting.

\begin{code}
newMethodId sel_id inst_ty origin loc
  = let (sel_tyvars,sel_theta,sel_tau) = splitSigmaTy (idType sel_id)
	(_:meth_theta) = sel_theta	-- The local theta is all except the
					-- first element of the context
    in 
       case sel_tyvars of
	-- Ah! a selector for a class op with no local polymorphism
	-- Build an Inst for this
	[clas_tyvar] -> newMethod origin (RealId sel_id) [inst_ty]

	-- Ho! a selector for a class op with local polymorphism.
	-- Just make a suitably typed local id for this
	(clas_tyvar:local_tyvars) -> 
		tcInstType [(clas_tyvar,inst_ty)]
			   (mkSigmaTy local_tyvars meth_theta sel_tau)
								`thenNF_Tc` \ method_ty ->
		newLocalId (getLocalName sel_id) method_ty	`thenNF_Tc` \ meth_id ->
		returnNF_Tc (emptyLIE, meth_id)
\end{code}

The next function makes a default method which calls the global default method, at
the appropriate instance type.

See the notes under default decls in TcClassDcl.lhs.

\begin{code}
makeInstanceDeclDefaultMethodExpr
	:: InstOrigin s
	-> [TcIdOcc s]
	-> [Id]
	-> TcType s
	-> TcIdOcc s
	-> Int
	-> NF_TcM s (TcExpr s)

makeInstanceDeclDefaultMethodExpr origin meth_ids defm_ids inst_ty this_dict tag
  = newDicts origin op_theta		`thenNF_Tc` \ (op_lie,op_dicts) ->

	-- def_op_id = /\ op_tyvars -> \ op_dicts ->
	--		  defm_id inst_ty op_tyvars this_dict op_dicts
    returnNF_Tc (
      mkHsTyLam op_tyvars (
      mkHsDictLam op_dicts (
      mkHsDictApp (mkHsTyApp (HsVar (RealId defm_id))
			     (inst_ty :  mkTyVarTys op_tyvars))
		  (this_dict : op_dicts)
      )))
 where
    idx	    = tag - 1
    meth_id = meth_ids !! idx
    defm_id = defm_ids  !! idx
    (op_tyvars, op_theta, op_tau) = splitSigmaTy (tcIdType meth_id)

makeInstanceDeclNoDefaultExpr
	:: InstOrigin s
	-> [TcIdOcc s]
	-> [Id]
	-> TcType s
	-> Class
	-> Maybe Module
	-> Int
	-> NF_TcM s (TcExpr s)

makeInstanceDeclNoDefaultExpr origin meth_ids defm_ids inst_ty clas inst_mod tag
  = newDicts origin op_theta		`thenNF_Tc` \ (op_lie, op_dicts) ->

	-- Produce a warning if the default instance method
	-- has been omitted when one exists in the class
    warnTc (not err_defm_ok)
	   (omitDefaultMethodWarn clas_op clas_name inst_ty)
					`thenNF_Tc_`
    returnNF_Tc (mkHsTyLam op_tyvars (
		 mkHsDictLam op_dicts (
		 HsApp (mkHsTyApp (HsVar (RealId nO_EXPLICIT_METHOD_ERROR_ID)) [op_tau])
		     (HsLitOut (HsString (_PK_ error_msg)) stringTy))))
  where
    idx	    = tag - 1
    meth_id = meth_ids  !! idx
    clas_op = (classOps clas) !! idx
    defm_id = defm_ids  !! idx
    (op_tyvars,op_theta,op_tau) = splitSigmaTy (tcIdType meth_id)

    Just (_, _, err_defm_ok) = isDefaultMethodId_maybe defm_id

    mod_str = case inst_mod of { Nothing -> pRELUDE; Just m -> m }

    error_msg = _UNPK_ mod_str ++ "." ++ _UNPK_ clas_name ++ "."
	     	++ (ppShow 80 (ppr PprForUser inst_ty)) ++ "."
	     	++ (ppShow 80 (ppr PprForUser clas_op))	++ "\""

    clas_name = nameOf (origName clas)
\end{code}


%************************************************************************
%*									*
\subsection{Processing each method}
%*									*
%************************************************************************

@processInstBinds@ returns a @MonoBinds@ which binds
all the method ids (which are passed in).  It is used
	- both for instance decls,
	- and to compile the default-method declarations in a class decl.

Any method ids which don't have a binding have a suitable default
binding created for them. The actual right-hand side used is
created using a function which is passed in, because the right thing to
do differs between instance and class decls.

\begin{code}
processInstBinds
	:: Class
	-> (Int -> NF_TcM s (TcExpr s))    -- Function to make default method
	-> [TcTyVar s]			   -- Tyvars for this instance decl
	-> LIE s			   -- available Insts
	-> [TcIdOcc s]			   -- Local method ids in tag order
					   --	(instance tyvars are free in their types)
	-> RenamedMonoBinds
	-> TcM s (LIE s,		   -- These are required
		  TcMonoBinds s)

processInstBinds clas mk_default_method_rhs inst_tyvars avail_insts method_ids monobinds
  =
	 -- Process the explicitly-given method bindings
    processInstBinds1 clas inst_tyvars avail_insts method_ids monobinds
	 		`thenTc` \ (tags, insts_needed_in_methods, method_binds) ->

	 -- Find the methods not handled, and make default method bindings for them.
    let
	unmentioned_tags = [1.. length method_ids] `minusList` tags
    in
    mapNF_Tc mk_default_method unmentioned_tags
			`thenNF_Tc` \ default_bind_list ->

    returnTc (insts_needed_in_methods,
	      foldr AndMonoBinds method_binds default_bind_list)
  where
	-- From a tag construct us the passed-in function to construct
	-- the binding for the default method
    mk_default_method tag = mk_default_method_rhs tag 	`thenNF_Tc` \ rhs ->
			    returnNF_Tc (VarMonoBind (method_ids !! (tag-1)) rhs)
\end{code}

\begin{code}
processInstBinds1
	:: Class
	-> [TcTyVar s]		-- Tyvars for this instance decl
	-> LIE s		-- available Insts
	-> [TcIdOcc s]		-- Local method ids in tag order (instance tyvars are free),
	-> RenamedMonoBinds
	-> TcM s ([Int],	-- Class-op tags accounted for
		  LIE s,	-- These are required
		  TcMonoBinds s)

processInstBinds1 clas inst_tyvars avail_insts method_ids EmptyMonoBinds
  = returnTc ([], emptyLIE, EmptyMonoBinds)

processInstBinds1 clas inst_tyvars avail_insts method_ids (AndMonoBinds mb1 mb2)
  = processInstBinds1 clas inst_tyvars avail_insts method_ids mb1
				 `thenTc`	\ (op_tags1,dicts1,method_binds1) ->
    processInstBinds1 clas inst_tyvars avail_insts method_ids mb2
				 `thenTc`	\ (op_tags2,dicts2,method_binds2) ->
    returnTc (op_tags1 ++ op_tags2,
	      dicts1 `unionBags` dicts2,
	      AndMonoBinds method_binds1 method_binds2)
\end{code}

\begin{code}
processInstBinds1 clas inst_tyvars avail_insts method_ids mbind
  =
    -- Find what class op is being defined here.  The complication is
    -- that we could have a PatMonoBind or a FunMonoBind.  If the
    -- former, it should only bind a single variable, or else we're in
    -- trouble (I'm not sure what the static semantics of methods
    -- defined in a pattern binding with multiple patterns is!)
    -- Renamer has reduced us to these two cases.
    let
	(op,locn) = case mbind of
		      FunMonoBind op _ _ locn	       -> (op, locn)
		      PatMonoBind (VarPatIn op) _ locn -> (op, locn)

        occ    = getLocalName op
	origin = InstanceDeclOrigin
    in
    tcAddSrcLoc locn			 $

    -- Make a method id for the method
    let
	tag       = classOpTagByString clas occ
	method_id = method_ids !! (tag-1)

	method_ty = tcIdType method_id
	(method_tyvars, method_theta, method_tau) = splitSigmaTy method_ty
    in
    newDicts origin method_theta		`thenNF_Tc` \ (method_dicts,method_dict_ids) ->

    case (method_tyvars, method_dict_ids) of

      ([],[]) -> -- The simple case; no local polymorphism or overloading in the method

		-- Type check the method itself
	tcMethodBind method_id method_tau mbind	`thenTc` \ (mbind', lieIop) ->
	returnTc ([tag], lieIop, mbind')

      other ->	-- It's a locally-polymorphic and/or overloaded method; UGH!

		-- Make a new id for (a) the local, non-overloaded method
		-- and		     (b) the locally-overloaded method
		-- The latter is needed just so we can return an AbsBinds wrapped
		-- up inside a MonoBinds.

	newLocalId occ method_tau		`thenNF_Tc` \ local_id ->
	newLocalId occ method_ty		`thenNF_Tc` \ copy_id ->
	let
	    inst_method_tyvars = inst_tyvars ++ method_tyvars
	in
		-- Typecheck the method
	tcMethodBind local_id method_tau mbind `thenTc` \ (mbind', lieIop) ->

		-- Check the overloading part of the signature.
		-- Simplify everything fully, even though some
		-- constraints could "really" be left to the next
		-- level out. The case which forces this is
		--
		-- 	class Foo a where { op :: Bar a => a -> a }
		--
		-- Here we must simplify constraints on "a" to catch all
		-- the Bar-ish things.
	tcAddErrCtxt (methodSigCtxt op method_ty) (
	  tcSimplifyAndCheck
		(mkTyVarSet inst_method_tyvars)
		(method_dicts `plusLIE` avail_insts)
		lieIop
	) 					 `thenTc` \ (f_dicts, dict_binds) ->

	returnTc ([tag],
		  f_dicts,
		  VarMonoBind method_id
			 (HsLet
			     (AbsBinds
				method_tyvars
				method_dict_ids
				[(local_id, copy_id)]
				dict_binds
				(NonRecBind mbind'))
			     (HsVar copy_id)))
\end{code}

\begin{code}
tcMethodBind :: TcIdOcc s -> TcType s -> RenamedMonoBinds
	     -> TcM s (TcMonoBinds s, LIE s)

tcMethodBind meth_id meth_ty (FunMonoBind name inf matches locn)
  = tcMatchesFun name meth_ty matches `thenTc` \ (rhs', lie) ->
    returnTc (FunMonoBind meth_id inf rhs' locn, lie)

tcMethodBind meth_id meth_ty pbind@(PatMonoBind pat grhss_and_binds locn)
  -- pat is sure to be a (VarPatIn op)
  = tcAddErrCtxt (patMonoBindsCtxt pbind) $
    tcGRHSsAndBinds grhss_and_binds 	`thenTc` \ (grhss_and_binds', lie, rhs_ty) ->
    unifyTauTy meth_ty rhs_ty 		`thenTc_`
    returnTc (PatMonoBind (VarPat meth_id) grhss_and_binds' locn, lie)
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
	ty_names 			  = extractMonoTyNames ???is_tyvarish_name??? ty
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
		       _ _ _ binds True{-from here-} mod _ uprag) = maybe_unspec_inst

    	subst = case matchTy unspec_inst_ty inst_ty of
		     Just subst -> subst
		     Nothing    -> panic "tcSpecInstSig:matchTy"

	subst_theta    = instantiateThetaTy subst unspec_theta
	subst_tv_theta = instantiateThetaTy tv_e subst_theta

	mk_spec_origin clas ty
	  = InstanceSpecOrigin inst_mapper clas ty src_loc
    in
    tcSimplifyThetas mk_spec_origin subst_tv_theta
				`thenTc` \ simpl_tv_theta ->
    let
	simpl_theta = [ (clas, tv_to_tmpl tv) | (clas, tv) <- simpl_tv_theta ]

	tv_tmpl_map = inst_tv_tys `zipEqual` inst_tmpl_tys
	tv_to_tmpl tv = assoc "tcSpecInstSig" tv_tmpl_map tv
    in
    mkInstanceRelatedIds e True{-from here-} mod NoInstancePragmas src_loc
			 clas inst_tmpls inst_ty simpl_theta uprag
				`thenTc` \ (dfun_id, dfun_theta, const_meth_ids) ->

    getSwitchCheckerTc		`thenNF_Tc` \ sw_chkr ->
    (if sw_chkr SpecialiseTrace then
	pprTrace "Specialised Instance: "
	(ppAboves [ppCat [if null simpl_theta then ppNil else ppr PprDebug simpl_theta,
			  if null simpl_theta then ppNil else ppStr "=>",
			  ppr PprDebug clas,
			  pprParendGenType PprDebug inst_ty],
		   ppCat [ppStr "        derived from:",
			  if null unspec_theta then ppNil else ppr PprDebug unspec_theta,
			  if null unspec_theta then ppNil else ppStr "=>",
			  ppr PprDebug clas,
			  pprParendGenType PprDebug unspec_inst_ty]])
    else id) (

    returnTc (unitBag (InstInfo clas inst_tmpls inst_ty simpl_theta
				dfun_theta dfun_id const_meth_ids
				binds True{-from here-} mod src_loc uprag))
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
scrutiniseInstanceType from_here clas inst_tau
	-- TYCON CHECK
  | not (maybeToBool inst_tycon_maybe) || isSynTyCon inst_tycon
  = failTc (instTypeErr inst_tau)

  	-- IMPORTED INSTANCES ARE OK (but see tcInstDecl1)
  | from_here
  = returnTc (inst_tycon,arg_tys)

	-- TYVARS CHECK
  | not (all isTyVarTy arg_tys ||
         not from_here	       ||
	 opt_GlasgowExts)
  = failTc (instTypeErr inst_tau)

  	-- DERIVING CHECK
	-- It is obviously illegal to have an explicit instance
	-- for something that we are also planning to `derive'
	-- Though we can have an explicit instance which is more
	-- specific than the derived instance
  | clas `derivedFor` inst_tycon
    && all isTyVarTy arg_tys
  = failTc (derivingWhenInstanceExistsErr clas inst_tycon)

  |	-- CCALL CHECK
	-- A user declaration of a CCallable/CReturnable instance
	-- must be for a "boxed primitive" type.
    isCcallishClass clas
    && not opt_CompilingPrelude		-- which allows anything
    && maybeToBool (maybeBoxedPrimType inst_tau)
  = failTc (nonBoxedPrimCCallErr clas inst_tau)

  | otherwise
  = returnTc (inst_tycon,arg_tys)

  where
    (possible_tycon, arg_tys) = splitAppTy inst_tau
    inst_tycon_maybe	      = getTyCon_maybe possible_tycon
    inst_tycon 		      = expectJust "tcInstDecls1:inst_tycon" inst_tycon_maybe
\end{code}

\begin{code}

instTypeErr ty sty
  = case ty of
      SynTy tc _ _ -> ppBesides [ppStr "The type synonym `", ppr sty tc, rest_of_msg]
      TyVarTy tv   -> ppBesides [ppStr "The type variable `", ppr sty tv, rest_of_msg]
      other	   -> ppBesides [ppStr "The type `", ppr sty ty, rest_of_msg]
  where
    rest_of_msg = ppStr "' cannot be used as an instance type."

derivingWhenInstanceExistsErr clas tycon sty
  = ppHang (ppBesides [ppStr "Deriving class `", ppr sty clas, ppStr "' type `", ppr sty tycon, ppStr "'"])
         4 (ppStr "when an explicit instance exists")

derivingWhenInstanceImportedErr inst_mod clas tycon sty
  = ppHang (ppBesides [ppStr "Deriving class `", ppr sty clas, ppStr "' type `", ppr sty tycon, ppStr "'"])
         4 (ppBesides [ppStr "when an instance declared in module `", pp_mod, ppStr "' has been imported"])
  where
    pp_mod = case inst_mod of
	       Nothing -> ppPStr SLIT("the standard Prelude")
	       Just  m -> ppBesides [ppStr "module `", ppPStr m, ppStr "'"]

nonBoxedPrimCCallErr clas inst_ty sty
  = ppHang (ppStr "Instance isn't for a `boxed-primitive' type")
	 4 (ppBesides [ ppStr "class `", ppr sty clas, ppStr "' type `",
    		        ppr sty inst_ty, ppStr "'"])

omitDefaultMethodWarn clas_op clas_name inst_ty sty
  = ppCat [ppStr "Warning: Omitted default method for",
	   ppr sty clas_op, ppStr "in instance",
	   ppPStr clas_name, pprParendGenType sty inst_ty]


patMonoBindsCtxt pbind sty
  = ppHang (ppStr "In a pattern binding:")
	 4 (ppr sty pbind)

methodSigCtxt name ty sty
  = ppHang (ppBesides [ppStr "When matching the definition of class method `",
	               ppr sty name, ppStr "' to its signature :" ])
	 4 (ppr sty ty)

bindSigCtxt method_ids sty
  = ppHang (ppStr "When checking type signatures for: ")
	 4 (ppInterleave (ppStr ", ") (map (ppr sty) method_ids))

superClassSigCtxt sty
  = ppStr "When checking superclass constraints on instance declaration"

\end{code}
