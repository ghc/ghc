%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcInstDecls]{Typechecking instance declarations}

\begin{code}
module TcInstDcls ( tcInstDecls1, tcInstDecls2, tcAddDeclCtxt ) where

#include "HsVersions.h"


import CmdLineOpts	( DynFlag(..), dopt )

import HsSyn		( HsDecl(..), InstDecl(..), TyClDecl(..), HsType(..),
			  MonoBinds(..), HsExpr(..),  HsLit(..), Sig(..), HsTyVarBndr(..),
			  andMonoBindList, collectMonoBinders, isClassDecl, toHsType
			)
import RnHsSyn		( RenamedHsBinds, RenamedInstDecl, RenamedHsDecl, 
			  RenamedMonoBinds, RenamedTyClDecl, RenamedHsType, 
			  extractHsTyVars, maybeGenericMatch
			)
import TcHsSyn		( TcMonoBinds, mkHsConApp )
import TcBinds		( tcSpecSigs )
import TcClassDcl	( tcMethodBind, badMethodErr )
import TcMonad       
import TcType		( tcInstType )
import Inst		( InstOrigin(..),
			  newDicts, instToId,
			  LIE, mkLIE, emptyLIE, plusLIE, plusLIEs )
import TcDeriv		( tcDeriving )
import TcEnv		( TcEnv, tcExtendGlobalValEnv, 
			  tcExtendTyVarEnvForMeths, 
			  tcAddImportedIdInfo, tcLookupClass,
 			  InstInfo(..), pprInstInfo, simpleInstInfoTyCon, 
			  simpleInstInfoTy, newDFunName, tcExtendTyVarEnv,
			  isLocalThing,
			)
import InstEnv		( InstEnv, extendInstEnv )
import TcMonoType	( tcHsTyVars, tcHsSigType, kcHsSigType, checkSigTyVars )
import TcSimplify	( tcSimplifyCheck )
import HscTypes		( HomeSymbolTable, DFunId,
			  ModDetails(..), PackageInstEnv, PersistentRenamerState
			)

import DataCon		( classDataCon )
import Class		( Class, DefMeth(..), classBigSig )
import Var		( idName, idType )
import VarSet		( emptyVarSet )
import Maybes 		( maybeToBool )
import MkId		( mkDictFunId )
import FunDeps		( checkInstFDs )
import Generics		( validGenericInstanceType )
import Module		( Module, foldModuleEnv )
import Name		( getSrcLoc )
import NameSet		( unitNameSet, nameSetToList )
import PrelInfo		( eRROR_ID )
import PprType		( pprClassPred, pprPred )
import TyCon		( TyCon, isSynTyCon )
import Type		( splitDFunTy, isTyVarTy,
			  splitTyConApp_maybe, splitDictTy,
			  splitForAllTys,
			  tyVarsOfTypes, mkClassPred, mkTyVarTy,
			  isTyVarClassPred, inheritablePred
			)
import Subst		( mkTopTyVarSubst, substTheta )
import VarSet		( varSetElems )
import TysWiredIn	( genericTyCons, isFFIArgumentTy, isFFIImportResultTy )
import ForeignCall	( Safety(..) )
import PrelNames	( cCallableClassKey, cReturnableClassKey, hasKey )
import Name             ( Name )
import SrcLoc           ( SrcLoc )
import VarSet           ( varSetElems )
import Unique		( Uniquable(..) )
import BasicTypes	( NewOrData(..), Fixity )
import ErrUtils		( dumpIfSet_dyn )
import ListSetOps	( Assoc, emptyAssoc, plusAssoc_C, mapAssoc, 
			  assocElts, extendAssoc_C,
			  equivClassesByUniq, minusList
			)
import List             ( partition )
import Outputable
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


%************************************************************************
%*									*
\subsection{Extracting instance decls}
%*									*
%************************************************************************

Gather up the instance declarations from their various sources

\begin{code}
tcInstDecls1 :: PackageInstEnv
	     -> PersistentRenamerState	
	     -> HomeSymbolTable		-- Contains instances
	     -> TcEnv 			-- Contains IdInfo for dfun ids
	     -> (Name -> Maybe Fixity)	-- for deriving Show and Read
	     -> Module			-- Module for deriving
	     -> [RenamedHsDecl]
	     -> TcM (PackageInstEnv, InstEnv, [InstInfo], RenamedHsBinds)

tcInstDecls1 inst_env0 prs hst unf_env get_fixity this_mod decls
  = let
	inst_decls = [inst_decl | InstD inst_decl <- decls]	
	tycl_decls = [decl      | TyClD decl <- decls]
	clas_decls = filter isClassDecl tycl_decls
    in
   	-- (1) Do the ordinary instance declarations
    mapNF_Tc tcInstDecl1 inst_decls		`thenNF_Tc` \ inst_infos ->

	-- (2) Instances from generic class declarations
    getGenericInstances clas_decls		`thenTc` \ generic_inst_info -> 

	-- Next, construct the instance environment so far, consisting of
	--	a) cached non-home-package InstEnv (gotten from pcs)	pcs_insts pcs
	--	b) imported instance decls (not in the home package)	inst_env1
	--	c) other modules in this package (gotten from hst)	inst_env2
	--	d) local instance decls					inst_env3
	--	e) generic instances					inst_env4
	-- The result of (b) replaces the cached InstEnv in the PCS
    let
	(local_inst_info, imported_inst_info) 
		= partition (isLocalThing this_mod . iDFunId) (concat inst_infos)

	imported_dfuns	 = map (tcAddImportedIdInfo unf_env . iDFunId) 
			       imported_inst_info
	hst_dfuns	 = foldModuleEnv ((++) . md_insts) [] hst
    in
    addInstDFuns inst_env0 imported_dfuns	`thenNF_Tc` \ inst_env1 ->
    addInstDFuns inst_env1 hst_dfuns		`thenNF_Tc` \ inst_env2 ->
    addInstInfos inst_env2 local_inst_info	`thenNF_Tc` \ inst_env3 ->
    addInstInfos inst_env3 generic_inst_info	`thenNF_Tc` \ inst_env4 ->

	-- (3) Compute instances from "deriving" clauses; 
	--     note that we only do derivings for things in this module; 
	--     we ignore deriving decls from interfaces!
	-- This stuff computes a context for the derived instance decl, so it
	-- needs to know about all the instances possible; hecne inst_env4
    tcDeriving prs this_mod inst_env4 get_fixity tycl_decls
					`thenTc` \ (deriv_inst_info, deriv_binds) ->
    addInstInfos inst_env4 deriv_inst_info		`thenNF_Tc` \ final_inst_env ->

    returnTc (inst_env1, 
	      final_inst_env, 
	      generic_inst_info ++ deriv_inst_info ++ local_inst_info,
	      deriv_binds)

addInstInfos :: InstEnv -> [InstInfo] -> NF_TcM InstEnv
addInstInfos inst_env infos = addInstDFuns inst_env (map iDFunId infos)

addInstDFuns :: InstEnv -> [DFunId] -> NF_TcM InstEnv
addInstDFuns dfuns infos
  = getDOptsTc				`thenTc` \ dflags ->
    let
	(inst_env', errs) = extendInstEnv dflags dfuns infos
    in
    addErrsTc errs			`thenNF_Tc_` 
    returnTc inst_env'
\end{code} 

\begin{code}
tcInstDecl1 :: RenamedInstDecl -> NF_TcM [InstInfo]
-- Deal with a single instance declaration
tcInstDecl1 decl@(InstDecl poly_ty binds uprags maybe_dfun_name src_loc)
  = 	-- Prime error recovery, set source location
    recoverNF_Tc (returnNF_Tc [])	$
    tcAddSrcLoc src_loc			$

	-- Type-check all the stuff before the "where"
    tcAddErrCtxt (instDeclCtxt poly_ty)	(
 	tcHsSigType poly_ty
    )					`thenTc` \ poly_ty' ->
    let
	(tyvars, theta, clas, inst_tys) = splitDFunTy poly_ty'
    in

    (case maybe_dfun_name of
	Nothing ->	-- A source-file instance declaration

		-- Check for respectable instance type, and context
		-- but only do this for non-imported instance decls.
		-- Imported ones should have been checked already, and may indeed
		-- contain something illegal in normal Haskell, notably
		--	instance CCallable [Char] 
	    getDOptsTc						`thenTc` \ dflags -> 
    	    checkInstValidity dflags theta clas inst_tys	`thenTc_`

		-- Make the dfun id and return it
	    newDFunName clas inst_tys src_loc		`thenNF_Tc` \ dfun_name ->
	    returnNF_Tc (True, dfun_name)

	Just dfun_name -> 	-- An interface-file instance declaration
    		-- Make the dfun id
	    returnNF_Tc (False, dfun_name)
    )						`thenNF_Tc` \ (is_local, dfun_name) ->

    let
	dfun_id = mkDictFunId dfun_name clas tyvars inst_tys theta
    in
    returnTc [InstInfo { iDFunId = dfun_id, 
			 iBinds = binds,    iPrags = uprags }]
\end{code}


%************************************************************************
%*									*
\subsection{Extracting generic instance declaration from class declarations}
%*									*
%************************************************************************

@getGenericInstances@ extracts the generic instance declarations from a class
declaration.  For exmaple

	class C a where
	  op :: a -> a
	
	  op{ x+y } (Inl v)   = ...
	  op{ x+y } (Inr v)   = ...
	  op{ x*y } (v :*: w) = ...
	  op{ 1   } Unit      = ...

gives rise to the instance declarations

	instance C (x+y) where
	  op (Inl v)   = ...
	  op (Inr v)   = ...
	
	instance C (x*y) where
	  op (v :*: w) = ...

	instance C 1 where
	  op Unit      = ...


\begin{code}
getGenericInstances :: [RenamedTyClDecl] -> TcM [InstInfo] 
getGenericInstances class_decls
  = mapTc get_generics class_decls		`thenTc` \ gen_inst_infos ->
    let
	gen_inst_info = concat gen_inst_infos
    in
    if null gen_inst_info then
	returnTc []
    else
    getDOptsTc						`thenTc`  \ dflags ->
    ioToTc (dumpIfSet_dyn dflags Opt_D_dump_deriv "Generic instances" 
		      (vcat (map pprInstInfo gen_inst_info)))	
							`thenNF_Tc_`
    returnTc gen_inst_info

get_generics decl@(ClassDecl {tcdMeths = Nothing})
  = returnTc []	-- Imported class decls

get_generics decl@(ClassDecl {tcdName = class_name, tcdMeths = Just def_methods, tcdLoc = loc})
  | null groups		
  = returnTc [] -- The comon case: no generic default methods

  | otherwise	-- A local class decl with generic default methods
  = recoverNF_Tc (returnNF_Tc [])				$
    tcAddDeclCtxt decl						$
    tcLookupClass class_name					`thenTc` \ clas ->

	-- Make an InstInfo out of each group
    mapTc (mkGenericInstance clas loc) groups		`thenTc` \ inst_infos ->

	-- Check that there is only one InstInfo for each type constructor
  	-- The main way this can fail is if you write
	--	f {| a+b |} ... = ...
	--	f {| x+y |} ... = ...
	-- Then at this point we'll have an InstInfo for each
    let
	tc_inst_infos :: [(TyCon, InstInfo)]
	tc_inst_infos = [(simpleInstInfoTyCon i, i) | i <- inst_infos]

	bad_groups = [group | group <- equivClassesByUniq get_uniq tc_inst_infos,
			      length group > 1]
	get_uniq (tc,_) = getUnique tc
    in
    mapTc (addErrTc . dupGenericInsts) bad_groups	`thenTc_`

	-- Check that there is an InstInfo for each generic type constructor
    let
	missing = genericTyCons `minusList` [tc | (tc,_) <- tc_inst_infos]
    in
    checkTc (null missing) (missingGenericInstances missing)	`thenTc_`

    returnTc inst_infos

  where
	-- Group the declarations by type pattern
	groups :: [(RenamedHsType, RenamedMonoBinds)]
	groups = assocElts (getGenericBinds def_methods)


---------------------------------
getGenericBinds :: RenamedMonoBinds -> Assoc RenamedHsType RenamedMonoBinds
  -- Takes a group of method bindings, finds the generic ones, and returns
  -- them in finite map indexed by the type parameter in the definition.

getGenericBinds EmptyMonoBinds    = emptyAssoc
getGenericBinds (AndMonoBinds m1 m2) 
  = plusAssoc_C AndMonoBinds (getGenericBinds m1) (getGenericBinds m2)

getGenericBinds (FunMonoBind id infixop matches loc)
  = mapAssoc wrap (foldl add emptyAssoc matches)
	-- Using foldl not foldr is vital, else
	-- we reverse the order of the bindings!
  where
    add env match = case maybeGenericMatch match of
		      Nothing		-> env
		      Just (ty, match') -> extendAssoc_C (++) env (ty, [match'])

    wrap ms = FunMonoBind id infixop ms loc

---------------------------------
mkGenericInstance :: Class -> SrcLoc
		  -> (RenamedHsType, RenamedMonoBinds)
		  -> TcM InstInfo

mkGenericInstance clas loc (hs_ty, binds)
  -- Make a generic instance declaration
  -- For example:	instance (C a, C b) => C (a+b) where { binds }

  = 	-- Extract the universally quantified type variables
    let
	sig_tvs = map UserTyVar (nameSetToList (extractHsTyVars hs_ty))
    in
    tcHsTyVars sig_tvs (kcHsSigType hs_ty)	$ \ tyvars ->

	-- Type-check the instance type, and check its form
    tcHsSigType hs_ty				`thenTc` \ inst_ty ->
    checkTc (validGenericInstanceType inst_ty)
	    (badGenericInstanceType binds)	`thenTc_`

	-- Make the dictionary function.
    newDFunName clas [inst_ty] loc		`thenNF_Tc` \ dfun_name ->
    let
	inst_theta = [mkClassPred clas [mkTyVarTy tv] | tv <- tyvars]
	inst_tys   = [inst_ty]
	dfun_id    = mkDictFunId dfun_name clas tyvars inst_tys inst_theta
    in

    returnTc (InstInfo { iDFunId = dfun_id, 
		  	 iBinds = binds, iPrags = [] })
\end{code}


%************************************************************************
%*									*
\subsection{Type-checking instance declarations, pass 2}
%*									*
%************************************************************************

\begin{code}
tcInstDecls2 :: [InstInfo]
	     -> NF_TcM (LIE, TcMonoBinds)

tcInstDecls2 inst_decls
--  = foldBag combine tcInstDecl2 (returnNF_Tc (emptyLIE, EmptyMonoBinds)) inst_decls
  = foldr combine (returnNF_Tc (emptyLIE, EmptyMonoBinds)) 
          (map tcInstDecl2 inst_decls)
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
tcInstDecl2 :: InstInfo -> NF_TcM (LIE, TcMonoBinds)
-- tcInstDecl2 is called *only* on InstInfos 

tcInstDecl2 (InstInfo { iDFunId = dfun_id, 
			iBinds = monobinds, iPrags = uprags })
  =	 -- Prime error recovery
    recoverNF_Tc (returnNF_Tc (emptyLIE, EmptyMonoBinds))	$
    tcAddSrcLoc (getSrcLoc dfun_id)			   	$
    tcAddErrCtxt (instDeclCtxt (toHsType (idType dfun_id)))	$

	-- Instantiate the instance decl with tc-style type variables
    tcInstType (idType dfun_id)		`thenNF_Tc` \ (inst_tyvars', dfun_theta', dict_ty') ->
    let
	(clas, inst_tys') = splitDictTy dict_ty'
	origin		  = InstanceDeclOrigin

        (class_tyvars, sc_theta, _, op_items) = classBigSig clas

	dm_ids	  = [dm_id | (_, DefMeth dm_id) <- op_items]
	sel_names = [idName sel_id | (sel_id, _) <- op_items]

        -- Instantiate the super-class context with inst_tys
	sc_theta' = substTheta (mkTopTyVarSubst class_tyvars inst_tys') sc_theta

	-- Find any definitions in monobinds that aren't from the class
	bad_bndrs = collectMonoBinders monobinds `minusList` sel_names

	-- The type variable from the dict fun actually scope 
	-- over the bindings.  They were gotten from
	-- the original instance declaration
	(inst_tyvars, _) = splitForAllTys (idType dfun_id)
    in
	 -- Check that all the method bindings come from this class
    mapTc (addErrTc . badMethodErr clas) bad_bndrs		`thenNF_Tc_`

	 -- Create dictionary Ids from the specified instance contexts.
    newDicts origin sc_theta'			 `thenNF_Tc` \ sc_dicts ->
    newDicts origin dfun_theta'			 `thenNF_Tc` \ dfun_arg_dicts ->
    newDicts origin [mkClassPred clas inst_tys'] `thenNF_Tc` \ [this_dict] ->

    tcExtendTyVarEnvForMeths inst_tyvars inst_tyvars' (
 	tcExtendGlobalValEnv dm_ids (
		-- Default-method Ids may be mentioned in synthesised RHSs 

	mapAndUnzip3Tc (tcMethodBind clas origin inst_tyvars' inst_tys'
				     dfun_theta'
				     monobinds uprags True)
		       op_items
    ))		 	`thenTc` \ (method_binds_s, insts_needed_s, meth_insts) ->

	-- Deal with SPECIALISE instance pragmas by making them
	-- look like SPECIALISE pragmas for the dfun
    let
	dfun_prags = [SpecSig (idName dfun_id) ty loc | SpecInstSig ty loc <- uprags]
    in
    tcExtendGlobalValEnv [dfun_id] (
	tcSpecSigs dfun_prags
    )					`thenTc` \ (prag_binds, prag_lie) ->

	-- Check the overloading constraints of the methods and superclasses
    let
		 -- These insts are in scope; quite a few, eh?
	avail_insts = [this_dict] ++
		      dfun_arg_dicts ++
		      sc_dicts ++
		      meth_insts

        methods_lie    = plusLIEs insts_needed_s
    in

	-- Simplify the constraints from methods
    tcAddErrCtxt methodCtxt (
      tcSimplifyCheck
		 (ptext SLIT("instance declaration context"))
		 inst_tyvars'
		 avail_insts
		 methods_lie
    )						 `thenTc` \ (const_lie1, lie_binds1) ->
    
	-- Figure out bindings for the superclass context
    tcAddErrCtxt superClassCtxt (
      tcSimplifyCheck
		 (ptext SLIT("instance declaration context"))
		 inst_tyvars'
		 dfun_arg_dicts		-- NB! Don't include this_dict here, else the sc_dicts
					-- get bound by just selecting from this_dict!!
		 (mkLIE sc_dicts)
    )						`thenTc` \ (const_lie2, lie_binds2) ->

    checkSigTyVars inst_tyvars' emptyVarSet	`thenNF_Tc` \ zonked_inst_tyvars ->

	-- Create the result bindings
    let
        dict_constr   = classDataCon clas
	scs_and_meths = map instToId (sc_dicts ++ meth_insts)
	this_dict_id  = instToId this_dict
	inlines       = unitNameSet (idName dfun_id)
		-- Always inline the dfun; this is an experimental decision
		-- because it makes a big performance difference sometimes.
		-- Often it means we can do the method selection, and then
		-- inline the method as well.  Marcin's idea; see comments below.

	dict_rhs
	  | null scs_and_meths
	  = 	-- Blatant special case for CCallable, CReturnable
		-- If the dictionary is empty then we should never
		-- select anything from it, so we make its RHS just
		-- emit an error message.  This in turn means that we don't
		-- mention the constructor, which doesn't exist for CCallable, CReturnable
		-- Hardly beautiful, but only three extra lines.
	    HsApp (TyApp (HsVar eRROR_ID) [idType this_dict_id])
		  (HsLit (HsString msg))

	  | otherwise	-- The common case
	  = mkHsConApp dict_constr inst_tys' (map HsVar scs_and_meths)
		-- We don't produce a binding for the dict_constr; instead we
		-- rely on the simplifier to unfold this saturated application
		-- We do this rather than generate an HsCon directly, because
		-- it means that the special cases (e.g. dictionary with only one
		-- member) are dealt with by the common MkId.mkDataConWrapId code rather
		-- than needing to be repeated here.

	  where
	    msg = _PK_ ("Compiler error: bad dictionary " ++ showSDoc (ppr clas))

	dict_bind    = VarMonoBind this_dict_id dict_rhs
	method_binds = andMonoBindList method_binds_s

	main_bind
	  = AbsBinds
		 zonked_inst_tyvars
		 (map instToId dfun_arg_dicts)
		 [(inst_tyvars', dfun_id, this_dict_id)] 
		 inlines
		 (lie_binds1	`AndMonoBinds` 
		  lie_binds2	`AndMonoBinds`
		  method_binds	`AndMonoBinds`
		  dict_bind)
    in
    returnTc (const_lie1 `plusLIE` const_lie2 `plusLIE` prag_lie,
	      main_bind `AndMonoBinds` prag_binds)
\end{code}

		------------------------------
		Inlining dfuns unconditionally
		------------------------------

The code above unconditionally inlines dict funs.  Here's why.
Consider this program:

    test :: Int -> Int -> Bool
    test x y = (x,y) == (y,x) || test y x
    -- Recursive to avoid making it inline.

This needs the (Eq (Int,Int)) instance.  If we inline that dfun
the code we end up with is good:

    Test.$wtest =
	\r -> case ==# [ww ww1] of wild {
		PrelBase.False -> Test.$wtest ww1 ww;
		PrelBase.True ->
		  case ==# [ww1 ww] of wild1 {
		    PrelBase.False -> Test.$wtest ww1 ww;
		    PrelBase.True -> PrelBase.True [];
		  };
	    };
    Test.test = \r [w w1]
	    case w of w2 {
	      PrelBase.I# ww ->
		  case w1 of w3 { PrelBase.I# ww1 -> Test.$wtest ww ww1; };
	    };

If we don't inline the dfun, the code is not nearly as good:

    (==) = case PrelTup.$fEq(,) PrelBase.$fEqInt PrelBase.$fEqInt of tpl {
	      PrelBase.:DEq tpl1 tpl2 -> tpl2;
	    };
    
    Test.$wtest =
	\r [ww ww1]
	    let { y = PrelBase.I#! [ww1]; } in
	    let { x = PrelBase.I#! [ww]; } in
	    let { sat_slx = PrelTup.(,)! [y x]; } in
	    let { sat_sly = PrelTup.(,)! [x y];
	    } in
	      case == sat_sly sat_slx of wild {
		PrelBase.False -> Test.$wtest ww1 ww;
		PrelBase.True -> PrelBase.True [];
	      };
    
    Test.test =
	\r [w w1]
	    case w of w2 {
	      PrelBase.I# ww ->
		  case w1 of w3 { PrelBase.I# ww1 -> Test.$wtest ww ww1; };
	    };

Why doesn't GHC inline $fEq?  Because it looks big:

    PrelTup.zdfEqZ1T{-rcX-}
	= \ @ a{-reT-} :: * @ b{-reS-} :: *
            zddEq{-rf6-} _Ks :: {PrelBase.Eq{-23-} a{-reT-}}
            zddEq1{-rf7-} _Ks :: {PrelBase.Eq{-23-} b{-reS-}} ->
            let {
              zeze{-rf0-} _Kl :: (b{-reS-} -> b{-reS-} -> PrelBase.Bool{-3c-})
              zeze{-rf0-} = PrelBase.zeze{-01L-}@ b{-reS-} zddEq1{-rf7-} } in
            let {
              zeze1{-rf3-} _Kl :: (a{-reT-} -> a{-reT-} -> PrelBase.Bool{-3c-})
              zeze1{-rf3-} = PrelBase.zeze{-01L-} @ a{-reT-} zddEq{-rf6-} } in
            let {
              zeze2{-reN-} :: ((a{-reT-}, b{-reS-}) -> (a{-reT-}, b{-reS-})-> PrelBase.Bool{-3c-})
              zeze2{-reN-} = \ ds{-rf5-} _Ks :: (a{-reT-}, b{-reS-})
		               ds1{-rf4-} _Ks :: (a{-reT-}, b{-reS-}) ->
                  	     case ds{-rf5-}
                  	     of wild{-reW-} _Kd { (a1{-rf2-} _Ks, a2{-reZ-} _Ks) ->
                  	     case ds1{-rf4-}
                  	     of wild1{-reX-} _Kd { (b1{-rf1-} _Ks, b2{-reY-} _Ks) ->
                  	     PrelBase.zaza{-r4e-}
                  	       (zeze1{-rf3-} a1{-rf2-} b1{-rf1-})
                  	       (zeze{-rf0-} a2{-reZ-} b2{-reY-})
                  	     }
                  	     } } in     
            let {
              a1{-reR-} :: ((a{-reT-}, b{-reS-})-> (a{-reT-}, b{-reS-})-> PrelBase.Bool{-3c-})
              a1{-reR-} = \ a2{-reV-} _Ks :: (a{-reT-}, b{-reS-})
			    b1{-reU-} _Ks :: (a{-reT-}, b{-reS-}) ->
                    	  PrelBase.not{-r6I-} (zeze2{-reN-} a2{-reV-} b1{-reU-})
            } in
              PrelBase.zdwZCDEq{-r8J-} @ (a{-reT-}, b{-reS-}) a1{-reR-} zeze2{-reN-})

and it's not as bad as it seems, because it's further dramatically
simplified: only zeze2 is extracted and its body is simplified.


%************************************************************************
%*									*
\subsection{Checking for a decent instance type}
%*									*
%************************************************************************

@scrutiniseInstanceHead@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere). In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.

\begin{code}
checkInstValidity dflags theta clas inst_tys
  | null errs = returnTc ()
  | otherwise = addErrsTc errs `thenNF_Tc_` failTc
  where
    errs = checkInstHead dflags theta clas inst_tys ++
	   [err | pred <- theta, err <- checkInstConstraint dflags pred]

checkInstConstraint dflags pred
	-- Checks whether a predicate is legal in the
	-- context of an instance declaration
  | ok 	       = []
  | otherwise  = [instConstraintErr pred]
  where
    ok = inheritablePred pred &&
	 (isTyVarClassPred pred || arbitrary_preds_ok)

    arbitrary_preds_ok = dopt Opt_AllowUndecidableInstances dflags


checkInstHead dflags theta clas inst_taus
  |	-- CCALL CHECK
	-- A user declaration of a CCallable/CReturnable instance
	-- must be for a "boxed primitive" type.
        (clas `hasKey` cCallableClassKey   
            && not (ccallable_type dflags first_inst_tau)) 
        ||
        (clas `hasKey` cReturnableClassKey 
            && not (creturnable_type first_inst_tau))
  = [nonBoxedPrimCCallErr clas first_inst_tau]

	-- If GlasgowExts then check at least one isn't a type variable
  | dopt Opt_GlasgowExts dflags
  = 	-- GlasgowExts case
    check_tyvars dflags clas inst_taus ++ check_fundeps dflags theta clas inst_taus

	-- WITH HASKELL 1.4, MUST HAVE C (T a b c)
  | not (length inst_taus == 1 &&
         maybeToBool maybe_tycon_app &&	-- Yes, there's a type constuctor
         not (isSynTyCon tycon) &&		-- ...but not a synonym
         all isTyVarTy arg_tys && 		-- Applied to type variables
	 length (varSetElems (tyVarsOfTypes arg_tys)) == length arg_tys
          -- This last condition checks that all the type variables are distinct
        )
  = [instTypeErr clas inst_taus
		 (text "the instance type must be of form (T a b c)" $$
		  text "where T is not a synonym, and a,b,c are distinct type variables")]

  | otherwise
  = []

  where
    (first_inst_tau : _)       = inst_taus

	-- Stuff for algebraic or -> type
    maybe_tycon_app	  = splitTyConApp_maybe first_inst_tau
    Just (tycon, arg_tys) = maybe_tycon_app

    ccallable_type   dflags ty = isFFIArgumentTy dflags PlayRisky ty
    creturnable_type        ty = isFFIImportResultTy dflags ty
	
check_tyvars dflags clas inst_taus
   	-- Check that at least one isn't a type variable
	-- unless -fallow-undecideable-instances
  | dopt Opt_AllowUndecidableInstances dflags = []
  | not (all isTyVarTy inst_taus)	      = []
  | otherwise 				      = [the_err]
  where
    the_err = instTypeErr clas inst_taus msg
    msg     =  ptext SLIT("There must be at least one non-type-variable in the instance head")
	    $$ ptext SLIT("Use -fallow-undecidable-instances to lift this restriction")

check_fundeps dflags theta clas inst_taus
  | checkInstFDs theta clas inst_taus = []
  | otherwise			      = [the_err]
  where
    the_err = instTypeErr clas inst_taus msg
    msg  = ptext SLIT("the instance types do not agree with the functional dependencies of the class")
\end{code}


%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
tcAddDeclCtxt decl thing_inside
  = tcAddSrcLoc (tcdLoc decl) 	$
    tcAddErrCtxt ctxt 	$
    thing_inside
  where
     thing = case decl of
	   	ClassDecl {}		  -> "class"
		TySynonym {}		  -> "type synonym"
		TyData {tcdND = NewType}  -> "newtype"
		TyData {tcdND = DataType} -> "data type"

     ctxt = hsep [ptext SLIT("In the"), text thing, 
		  ptext SLIT("declaration for"), quotes (ppr (tcdName decl))]

instDeclCtxt inst_ty = ptext SLIT("In the instance declaration for") <+> quotes doc
		     where
			doc = case inst_ty of
				HsForAllTy _ _ (HsPredTy pred) -> ppr pred
				HsPredTy pred	      	       -> ppr pred
				other			       -> ppr inst_ty	-- Don't expect this
\end{code}

\begin{code}
instConstraintErr pred
  = hang (ptext SLIT("Illegal constraint") <+> 
	  quotes (pprPred pred) <+> 
	  ptext SLIT("in instance context"))
	 4 (ptext SLIT("(Instance contexts must constrain only type variables)"))
	
badGenericInstanceType binds
  = vcat [ptext SLIT("Illegal type pattern in the generic bindings"),
	  nest 4 (ppr binds)]

missingGenericInstances missing
  = ptext SLIT("Missing type patterns for") <+> pprQuotedList missing
	  


dupGenericInsts tc_inst_infos
  = vcat [ptext SLIT("More than one type pattern for a single generic type constructor:"),
	  nest 4 (vcat (map ppr_inst_ty tc_inst_infos)),
	  ptext SLIT("All the type patterns for a generic type constructor must be identical")
    ]
  where 
    ppr_inst_ty (tc,inst) = ppr (simpleInstInfoTy inst)

instTypeErr clas tys msg
  = sep [ptext SLIT("Illegal instance declaration for") <+> 
		quotes (pprClassPred clas tys),
	 nest 4 (parens msg)
    ]

nonBoxedPrimCCallErr clas inst_ty
  = hang (ptext SLIT("Unacceptable instance type for ccall-ish class"))
	 4 (pprClassPred clas [inst_ty])

methodCtxt     = ptext SLIT("When checking the methods of an instance declaration")
superClassCtxt = ptext SLIT("When checking the super-classes of an instance declaration")
\end{code}
