%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcInstDecls]{Typechecking instance declarations}

\begin{code}
module TcInstDcls ( tcInstDecls1, tcIfaceInstDecls1, addInstDFuns, 
		    tcInstDecls2, initInstEnv, tcAddDeclCtxt ) where

#include "HsVersions.h"


import CmdLineOpts	( DynFlag(..) )

import HsSyn		( InstDecl(..), TyClDecl(..), HsType(..),
			  MonoBinds(..), HsExpr(..),  HsLit(..), Sig(..), HsTyVarBndr(..),
			  andMonoBindList, collectMonoBinders, 
			  isClassDecl, toHsType
			)
import RnHsSyn		( RenamedHsBinds, RenamedInstDecl, 
			  RenamedMonoBinds, RenamedTyClDecl, RenamedHsType, 
			  extractHsTyVars, maybeGenericMatch
			)
import TcHsSyn		( TcMonoBinds, mkHsConApp )
import TcBinds		( tcSpecSigs )
import TcClassDcl	( tcMethodBind, mkMethodBind, badMethodErr )
import TcMonad       
import TcMType		( tcInstType, checkValidTheta, checkValidInstHead, instTypeErr, 
			  UserTypeCtxt(..), SourceTyCtxt(..) )
import TcType		( mkClassPred, mkTyVarTy, tcSplitForAllTys,
			  tcSplitSigmaTy, getClassPredTys, tcSplitPredTy_maybe,
			  TyVarDetails(..)
			)
import Inst		( InstOrigin(..), newDicts, instToId,
			  LIE, mkLIE, emptyLIE, plusLIE, plusLIEs )
import TcDeriv		( tcDeriving )
import TcEnv		( tcExtendGlobalValEnv, tcExtendLocalValEnv2,
			  tcLookupId, tcLookupClass, tcExtendTyVarEnv2,
 			  InstInfo(..), pprInstInfo, simpleInstInfoTyCon, 
			  simpleInstInfoTy, newDFunName
			)
import InstEnv		( InstEnv, extendInstEnv )
import PprType		( pprClassPred )
import TcMonoType	( tcSigPolyId, tcHsTyVars, kcHsSigType, tcHsType, tcHsSigType )
import TcUnify		( checkSigTyVars )
import TcSimplify	( tcSimplifyCheck )
import HscTypes		( HomeSymbolTable, DFunId, FixityEnv,
			  PersistentCompilerState(..), PersistentRenamerState,
			  ModDetails(..)
			)
import Subst		( mkTyVarSubst, substTheta )
import DataCon		( classDataCon )
import Class		( Class, classBigSig )
import Var		( idName, idType )
import Id		( setIdLocalExported )
import MkId		( mkDictFunId, unsafeCoerceId, eRROR_ID )
import FunDeps		( checkInstFDs )
import Generics		( validGenericInstanceType )
import Module		( Module, foldModuleEnv )
import Name		( getSrcLoc )
import NameSet		( unitNameSet, emptyNameSet, nameSetToList )
import TyCon		( TyCon )
import TysWiredIn	( genericTyCons )
import SrcLoc           ( SrcLoc )
import Unique		( Uniquable(..) )
import Util             ( lengthExceeds, isSingleton )
import BasicTypes	( NewOrData(..) )
import ErrUtils		( dumpIfSet_dyn )
import ListSetOps	( Assoc, emptyAssoc, plusAssoc_C, mapAssoc, 
			  assocElts, extendAssoc_C, equivClassesByUniq, minusList
			)
import Maybe		( catMaybes )
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
tcInstDecls1	-- Deal with source-code instance decls
   :: PersistentRenamerState	
   -> InstEnv 			-- Imported instance envt
   -> FixityEnv			-- for deriving Show and Read
   -> Module			-- Module for deriving
   -> [RenamedTyClDecl]		-- For deriving stuff
   -> [RenamedInstDecl]		-- Source code instance decls
   -> TcM (InstEnv,		-- the full inst env
	   [InstInfo],		-- instance decls to process; contains all dfuns
				-- for this module
	   RenamedHsBinds)	-- derived instances

tcInstDecls1 prs inst_env get_fixity this_mod 
	     tycl_decls inst_decls
-- The incoming inst_env includes all the imported instances already
  = checkNoErrsTc $
	-- Stop if addInstInfos etc discovers any errors
	-- (they recover, so that we get more than one error each round)
   	-- (1) Do the ordinary instance declarations
    mapNF_Tc tcLocalInstDecl1 inst_decls	`thenNF_Tc` \ local_inst_infos ->

    let
	local_inst_info = catMaybes local_inst_infos
	clas_decls	= filter isClassDecl tycl_decls
    in
	-- (2) Instances from generic class declarations
    getGenericInstances clas_decls		`thenTc` \ generic_inst_info -> 

	-- Next, construct the instance environment so far, consisting of
	--      a) imported instance decls (from this module)	     inst_env1
	--	b) local instance decls				     inst_env2
	--	c) generic instances				     final_inst_env
    addInstInfos inst_env local_inst_info	`thenNF_Tc` \ inst_env1 ->
    addInstInfos inst_env1 generic_inst_info	`thenNF_Tc` \ inst_env2 ->

	-- (3) Compute instances from "deriving" clauses; 
	--     note that we only do derivings for things in this module; 
	--     we ignore deriving decls from interfaces!
	-- This stuff computes a context for the derived instance decl, so it
	-- needs to know about all the instances possible; hence inst_env4
    tcDeriving prs this_mod inst_env2 
	       get_fixity tycl_decls		`thenTc` \ (deriv_inst_info, deriv_binds) ->
    addInstInfos inst_env2 deriv_inst_info	`thenNF_Tc` \ final_inst_env ->

    returnTc (final_inst_env, 
	      generic_inst_info ++ deriv_inst_info ++ local_inst_info,
	      deriv_binds)

initInstEnv :: PersistentCompilerState -> HomeSymbolTable -> NF_TcM InstEnv
-- Initialise the instance environment from the 
-- persistent compiler state and the home symbol table
initInstEnv pcs hst
  = let
	pkg_inst_env = pcs_insts pcs
	hst_dfuns    = foldModuleEnv ((++) . md_insts) [] hst
    in
    addInstDFuns pkg_inst_env hst_dfuns

addInstInfos :: InstEnv -> [InstInfo] -> NF_TcM InstEnv
addInstInfos inst_env infos = addInstDFuns inst_env (map iDFunId infos)

addInstDFuns :: InstEnv -> [DFunId] -> NF_TcM InstEnv
addInstDFuns inst_env dfuns
  = getDOptsTc				`thenNF_Tc` \ dflags ->
    let
	(inst_env', errs) = extendInstEnv dflags inst_env dfuns
    in
    addErrsTc errs			`thenNF_Tc_` 
    traceTc (text "Adding instances:" <+> vcat (map pp dfuns))	`thenTc_`
    returnTc inst_env'
  where
    pp dfun = ppr dfun <+> dcolon <+> ppr (idType dfun)
\end{code} 

\begin{code}
tcIfaceInstDecls1 :: [RenamedInstDecl] -> NF_TcM [DFunId]
tcIfaceInstDecls1 decls = mapNF_Tc tcIfaceInstDecl1 decls

tcIfaceInstDecl1 :: RenamedInstDecl -> NF_TcM DFunId
	-- An interface-file instance declaration
	-- Should be in scope by now, because we should
	-- have sucked in its interface-file definition
	-- So it will be replete with its unfolding etc
tcIfaceInstDecl1 decl@(InstDecl poly_ty binds uprags (Just dfun_name) src_loc)
  = tcLookupId dfun_name


tcLocalInstDecl1 :: RenamedInstDecl 
		 -> NF_TcM (Maybe InstInfo)	-- Nothing if there was an error
	-- A source-file instance declaration
	-- Type-check all the stuff before the "where"
	--
	-- We check for respectable instance type, and context
	-- but only do this for non-imported instance decls.
	-- Imported ones should have been checked already, and may indeed
	-- contain something illegal in normal Haskell, notably
	--	instance CCallable [Char] 
tcLocalInstDecl1 decl@(InstDecl poly_ty binds uprags Nothing src_loc)
  =	-- Prime error recovery, set source location
    recoverNF_Tc (returnNF_Tc Nothing)	$
    tcAddSrcLoc src_loc			$
    tcAddErrCtxt (instDeclCtxt poly_ty)	$

	-- Typecheck the instance type itself.  We can't use 
	-- tcHsSigType, because it's not a valid user type.
    kcHsSigType poly_ty			`thenTc_`
    tcHsType poly_ty			`thenTc` \ poly_ty' ->
    let
	(tyvars, theta, tau) = tcSplitSigmaTy poly_ty'
    in
    checkValidTheta InstThetaCtxt theta		`thenTc_`
    checkValidInstHead tau			`thenTc` \ (clas,inst_tys) ->
    checkTc (checkInstFDs theta clas inst_tys)
	    (instTypeErr (pprClassPred clas inst_tys) msg)	`thenTc_`
    newDFunName clas inst_tys src_loc				`thenNF_Tc` \ dfun_name ->
    returnTc (Just (InstInfo { iDFunId = mkDictFunId dfun_name clas tyvars inst_tys theta,
			       iBinds = binds, iPrags = uprags }))
  where
    msg  = parens (ptext SLIT("the instance types do not agree with the functional dependencies of the class"))
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
    getDOptsTc						`thenNF_Tc`  \ dflags ->
    ioToTc (dumpIfSet_dyn dflags Opt_D_dump_deriv "Generic instances" 
		      (vcat (map pprInstInfo gen_inst_info)))	
							`thenNF_Tc_`
    returnTc gen_inst_info

get_generics decl@(ClassDecl {tcdMeths = Nothing})
  = returnTc []	-- Imported class decls

get_generics decl@(ClassDecl {tcdName = class_name, tcdMeths = Just def_methods, tcdLoc = loc})
  | null groups		
  = returnTc [] -- The comon case: no generic default methods

  | otherwise	-- A source class decl with generic default methods
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
			      group `lengthExceeds` 1]
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
    tcHsSigType GenPatCtxt hs_ty		`thenTc` \ inst_ty ->
    checkTc (validGenericInstanceType inst_ty)
	    (badGenericInstanceType binds)	`thenTc_`

	-- Make the dictionary function.
    newDFunName clas [inst_ty] loc		`thenNF_Tc` \ dfun_name ->
    let
	inst_theta = [mkClassPred clas [mkTyVarTy tv] | tv <- tyvars]
	dfun_id    = mkDictFunId dfun_name clas tyvars [inst_ty] inst_theta
    in

    returnTc (InstInfo { iDFunId = dfun_id, iBinds = binds, iPrags = [] })
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
tcInstDecl2 :: InstInfo -> TcM (LIE, TcMonoBinds)

tcInstDecl2 (NewTypeDerived { iDFunId = dfun_id })
  = tcInstType InstTv (idType dfun_id)		`thenNF_Tc` \ (inst_tyvars', dfun_theta', inst_head') ->
    newDicts InstanceDeclOrigin dfun_theta'	`thenNF_Tc` \ rep_dicts ->
    let
	rep_dict_id = ASSERT( isSingleton rep_dicts )
		      instToId (head rep_dicts)		-- Derived newtypes have just one dict arg

	body = TyLam inst_tyvars'    $
	       DictLam [rep_dict_id] $
	        (HsVar unsafeCoerceId `TyApp` [idType rep_dict_id, inst_head'])
			  `HsApp` 
		(HsVar rep_dict_id)
	-- You might wonder why we have the 'coerce'.  It's because the
	-- type equality mechanism isn't clever enough; see comments with Type.eqType.
	-- So Lint complains if we don't have this. 
    in
    returnTc (emptyLIE, VarMonoBind dfun_id body)

tcInstDecl2 (InstInfo { iDFunId = dfun_id, iBinds = monobinds, iPrags = uprags })
  =	 -- Prime error recovery
    recoverNF_Tc (returnNF_Tc (emptyLIE, EmptyMonoBinds))	$
    tcAddSrcLoc (getSrcLoc dfun_id)			   	$
    tcAddErrCtxt (instDeclCtxt (toHsType (idType dfun_id)))	$
    let
	inst_ty = idType dfun_id
	(inst_tyvars, _) = tcSplitForAllTys inst_ty
		-- The tyvars of the instance decl scope over the 'where' part
		-- Those tyvars are inside the dfun_id's type, which is a bit
		-- bizarre, but OK so long as you realise it!
    in

	-- Instantiate the instance decl with tc-style type variables
    tcInstType InstTv inst_ty		`thenNF_Tc` \ (inst_tyvars', dfun_theta', inst_head') ->
    let
	Just pred         = tcSplitPredTy_maybe inst_head'
	(clas, inst_tys') = getClassPredTys pred
        (class_tyvars, sc_theta, _, op_items) = classBigSig clas

	sel_names = [idName sel_id | (sel_id, _) <- op_items]

        -- Instantiate the super-class context with inst_tys
	sc_theta' = substTheta (mkTyVarSubst class_tyvars inst_tys') sc_theta

	-- Find any definitions in monobinds that aren't from the class
	bad_bndrs = collectMonoBinders monobinds `minusList` sel_names
	origin	  = InstanceDeclOrigin
    in
	 -- Check that all the method bindings come from this class
    mapTc (addErrTc . badMethodErr clas) bad_bndrs		`thenNF_Tc_`

	 -- Create dictionary Ids from the specified instance contexts.
    newDicts origin sc_theta'			 `thenNF_Tc` \ sc_dicts ->
    newDicts origin dfun_theta'			 `thenNF_Tc` \ dfun_arg_dicts ->
    newDicts origin [mkClassPred clas inst_tys'] `thenNF_Tc` \ [this_dict] ->
		-- Default-method Ids may be mentioned in synthesised RHSs,
		-- but they'll already be in the environment.

    mapAndUnzipTc (mkMethodBind origin clas inst_tys' monobinds) 
		  op_items  `thenTc` \ (meth_insts, meth_infos) ->

    let		
		 -- These insts are in scope; quite a few, eh?
	avail_insts = [this_dict] ++
		      dfun_arg_dicts ++
		      sc_dicts ++
		      meth_insts

	xtve    = inst_tyvars `zip` inst_tyvars'
	tc_meth = tcMethodBind xtve inst_tyvars' dfun_theta' avail_insts
    in
    mapAndUnzipTc tc_meth meth_infos 	`thenTc` \ (meth_binds_s, meth_lie_s) ->

	-- Figure out bindings for the superclass context
    tcAddErrCtxt superClassCtxt 	$
    tcSimplifyCheck
		 (ptext SLIT("instance declaration superclass context"))
		 inst_tyvars'
		 dfun_arg_dicts		-- NB! Don't include this_dict here, else the sc_dicts
					-- get bound by just selecting from this_dict!!
		 (mkLIE sc_dicts)
						`thenTc` \ (sc_lie, sc_binds) ->
	-- It's possible that the superclass stuff might have done unification
    checkSigTyVars inst_tyvars' 	`thenNF_Tc` \ zonked_inst_tyvars ->

	-- Deal with SPECIALISE instance pragmas by making them
	-- look like SPECIALISE pragmas for the dfun
    let
	mk_prag (SpecInstSig ty loc) = SpecSig (idName dfun_id) ty loc
	mk_prag prag 		     = prag

	all_prags = map mk_prag uprags
    in
     
    tcExtendGlobalValEnv [dfun_id] (
	tcExtendTyVarEnv2 xtve					$
	tcExtendLocalValEnv2 [(idName sel_id, tcSigPolyId sig) 
			     | (sel_id, sig, _) <- meth_infos]	$
		-- Map sel_id to the local method name we are using
	tcSpecSigs all_prags
    )					`thenTc` \ (prag_binds, prag_lie) ->

	-- Create the result bindings
    let
	local_dfun_id = setIdLocalExported dfun_id
		-- Reason for setIdLocalExported: see notes with MkId.mkDictFunId

        dict_constr   = classDataCon clas
	scs_and_meths = map instToId (sc_dicts ++ meth_insts)
	this_dict_id  = instToId this_dict
	inlines       | null dfun_arg_dicts = emptyNameSet
		      | otherwise	    = unitNameSet (idName dfun_id)
		-- Always inline the dfun; this is an experimental decision
		-- because it makes a big performance difference sometimes.
		-- Often it means we can do the method selection, and then
		-- inline the method as well.  Marcin's idea; see comments below.
		--
		-- BUT: don't inline it if it's a constant dictionary;
		-- we'll get all the benefit without inlining, and we get
		-- a **lot** of code duplication if we inline it

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

	dict_bind  = VarMonoBind this_dict_id dict_rhs
	meth_binds = andMonoBindList meth_binds_s
	all_binds  = sc_binds `AndMonoBinds` meth_binds	`AndMonoBinds` dict_bind

	main_bind = AbsBinds
		 	 zonked_inst_tyvars
		 	 (map instToId dfun_arg_dicts)
		 	 [(inst_tyvars', local_dfun_id, this_dict_id)] 
		 	 inlines all_binds
    in
    returnTc (plusLIEs meth_lie_s `plusLIE` sc_lie `plusLIE` prag_lie,
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

methodCtxt     = ptext SLIT("When checking the methods of an instance declaration")
superClassCtxt = ptext SLIT("When checking the super-classes of an instance declaration")
\end{code}
