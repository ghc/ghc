%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcInstDecls]{Typechecking instance declarations}

\begin{code}
module TcInstDcls ( tcInstDecls1, tcIfaceInstDecls, 
		    tcInstDecls2, tcAddDeclCtxt ) where

#include "HsVersions.h"


import CmdLineOpts	( DynFlag(..) )

import HsSyn		( InstDecl(..), TyClDecl(..), HsType(..),
			  MonoBinds(..), HsExpr(..),  HsLit(..), Sig(..), HsTyVarBndr(..),
			  andMonoBindList, collectMonoBinders, 
			  isClassDecl, isSourceInstDecl, toHsType
			)
import RnHsSyn		( RenamedHsBinds, RenamedInstDecl, 
			  RenamedMonoBinds, RenamedTyClDecl, RenamedHsType, 
			  extractHsTyVars, maybeGenericMatch
			)
import TcHsSyn		( TcMonoBinds, mkHsConApp )
import TcBinds		( tcSpecSigs )
import TcClassDcl	( tcMethodBind, mkMethodBind, badMethodErr )
import TcRnMonad       
import TcMType		( tcInstType, checkValidTheta, checkValidInstHead, instTypeErr, 
			  checkAmbiguity, UserTypeCtxt(..), SourceTyCtxt(..) )
import TcType		( mkClassPred, mkTyVarTy, tcSplitForAllTys, tyVarsOfType,
			  tcSplitSigmaTy, getClassPredTys, tcSplitPredTy_maybe, mkTyVarTys,
			  TyVarDetails(..)
			)
import Inst		( InstOrigin(..), tcInstClassOp, newDicts, instToId, showLIE )
import TcDeriv		( tcDeriving )
import TcEnv		( tcExtendGlobalValEnv, 
			  tcLookupClass, tcExtendTyVarEnv2,
			  tcExtendInstEnv, tcExtendLocalInstEnv, tcLookupGlobalId,
 			  InstInfo(..), InstBindings(..), pprInstInfo, simpleInstInfoTyCon, 
			  simpleInstInfoTy, newDFunName
			)
import PprType		( pprClassPred )
import TcMonoType	( tcHsTyVars, kcHsSigType, tcHsType, tcHsSigType )
import TcUnify		( checkSigTyVars )
import TcSimplify	( tcSimplifyCheck, tcSimplifyTop )
import HscTypes		( DFunId )
import Subst		( mkTyVarSubst, substTheta, substTy )
import DataCon		( classDataCon )
import Class		( Class, classBigSig )
import Var		( idName, idType )
import NameSet		
import MkId		( mkDictFunId, rUNTIME_ERROR_ID )
import FunDeps		( checkInstFDs )
import Generics		( validGenericInstanceType )
import Name		( getSrcLoc )
import NameSet		( unitNameSet, emptyNameSet, nameSetToList )
import TyCon		( TyCon )
import TysWiredIn	( genericTyCons )
import SrcLoc           ( SrcLoc )
import Unique		( Uniquable(..) )
import Util             ( lengthExceeds )
import BasicTypes	( NewOrData(..) )
import UnicodeUtil	( stringToUtf8 )
import ErrUtils		( dumpIfSet_dyn )
import ListSetOps	( Assoc, emptyAssoc, plusAssoc_C, mapAssoc, 
			  assocElts, extendAssoc_C, equivClassesByUniq, minusList
			)
import Maybe		( catMaybes )
import List		( partition )
import Outputable
import FastString
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
tcInstDecls1	-- Deal with both source-code and imported instance decls
   :: [RenamedTyClDecl]		-- For deriving stuff
   -> [RenamedInstDecl]		-- Source code instance decls
   -> TcM (TcGblEnv,		-- The full inst env
	   [InstInfo],		-- Source-code instance decls to process; 
				-- contains all dfuns for this module
	   RenamedHsBinds,	-- Supporting bindings for derived instances
	   FreeVars)		-- And the free vars of the derived code

tcInstDecls1 tycl_decls inst_decls
  = checkNoErrs $
	-- Stop if addInstInfos etc discovers any errors
	-- (they recover, so that we get more than one error each round)
    let
      (src_inst_decls, iface_inst_decls) = partition isSourceInstDecl inst_decls
    in

	-- (0) Deal with the imported instance decls
    tcIfaceInstDecls iface_inst_decls	`thenM` \ imp_dfuns ->
    tcExtendInstEnv imp_dfuns		$

   	-- (1) Do the ordinary instance declarations
    mappM tcLocalInstDecl1 src_inst_decls    `thenM` \ local_inst_infos ->

    let
	local_inst_info = catMaybes local_inst_infos
	clas_decls	= filter isClassDecl tycl_decls
    in
	-- (2) Instances from generic class declarations
    getGenericInstances clas_decls		`thenM` \ generic_inst_info -> 

	-- Next, construct the instance environment so far, consisting of
	--      a) imported instance decls (from this module)
	--	b) local instance decls
	--	c) generic instances
    tcExtendLocalInstEnv local_inst_info	$
    tcExtendLocalInstEnv generic_inst_info	$

	-- (3) Compute instances from "deriving" clauses; 
	--     note that we only do derivings for things in this module; 
	--     we ignore deriving decls from interfaces!
	-- This stuff computes a context for the derived instance decl, so it
	-- needs to know about all the instances possible; hence inst_env4
    tcDeriving tycl_decls			`thenM` \ (deriv_inst_info, deriv_binds, fvs) ->
    tcExtendLocalInstEnv deriv_inst_info	$

    getGblEnv					`thenM` \ gbl_env ->
    returnM (gbl_env, 
	     generic_inst_info ++ deriv_inst_info ++ local_inst_info,
	     deriv_binds, fvs)
\end{code} 

\begin{code}
tcLocalInstDecl1 :: RenamedInstDecl 
		 -> TcM (Maybe InstInfo)	-- Nothing if there was an error
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
    recoverM (returnM Nothing)		$
    addSrcLoc src_loc			$
    addErrCtxt (instDeclCtxt poly_ty)	$

	-- Typecheck the instance type itself.  We can't use 
	-- tcHsSigType, because it's not a valid user type.
    kcHsSigType poly_ty			`thenM_`
    tcHsType poly_ty			`thenM` \ poly_ty' ->
    let
	(tyvars, theta, tau) = tcSplitSigmaTy poly_ty'
    in
    checkValidTheta InstThetaCtxt theta			`thenM_`
    checkAmbiguity tyvars theta (tyVarsOfType tau)	`thenM_`
    checkValidInstHead tau				`thenM` \ (clas,inst_tys) ->
    checkTc (checkInstFDs theta clas inst_tys)
	    (instTypeErr (pprClassPred clas inst_tys) msg)	`thenM_`
    newDFunName clas inst_tys src_loc				`thenM` \ dfun_name ->
    returnM (Just (InstInfo { iDFunId = mkDictFunId dfun_name tyvars theta clas inst_tys,
			      iBinds = VanillaInst binds uprags }))
  where
    msg  = parens (ptext SLIT("the instance types do not agree with the functional dependencies of the class"))
\end{code}

Imported instance declarations

\begin{code}
tcIfaceInstDecls :: [RenamedInstDecl] -> TcM [DFunId]
-- Deal with the instance decls, 
tcIfaceInstDecls decls = mappM tcIfaceInstDecl decls

tcIfaceInstDecl :: RenamedInstDecl -> TcM DFunId
	-- An interface-file instance declaration
	-- Should be in scope by now, because we should
	-- have sucked in its interface-file definition
	-- So it will be replete with its unfolding etc
tcIfaceInstDecl decl@(InstDecl poly_ty binds uprags (Just dfun_name) src_loc)
  = tcLookupGlobalId dfun_name
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
  = mappM get_generics class_decls		`thenM` \ gen_inst_infos ->
    let
	gen_inst_info = concat gen_inst_infos
    in
    if null gen_inst_info then
	returnM []
    else
    getDOpts						`thenM`  \ dflags ->
    ioToTcRn (dumpIfSet_dyn dflags Opt_D_dump_deriv "Generic instances" 
	 	    (vcat (map pprInstInfo gen_inst_info)))	
							`thenM_`
    returnM gen_inst_info

get_generics decl@(ClassDecl {tcdMeths = Nothing})
  = returnM []	-- Imported class decls

get_generics decl@(ClassDecl {tcdName = class_name, tcdMeths = Just def_methods, tcdLoc = loc})
  | null groups		
  = returnM [] -- The comon case: no generic default methods

  | otherwise	-- A source class decl with generic default methods
  = recoverM (returnM [])				$
    tcAddDeclCtxt decl					$
    tcLookupClass class_name				`thenM` \ clas ->

	-- Make an InstInfo out of each group
    mappM (mkGenericInstance clas loc) groups		`thenM` \ inst_infos ->

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
    mappM (addErrTc . dupGenericInsts) bad_groups	`thenM_`

	-- Check that there is an InstInfo for each generic type constructor
    let
	missing = genericTyCons `minusList` [tc | (tc,_) <- tc_inst_infos]
    in
    checkTc (null missing) (missingGenericInstances missing)	`thenM_`

    returnM inst_infos

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
    tcHsSigType GenPatCtxt hs_ty		`thenM` \ inst_ty ->
    checkTc (validGenericInstanceType inst_ty)
	    (badGenericInstanceType binds)	`thenM_`

	-- Make the dictionary function.
    newDFunName clas [inst_ty] loc		`thenM` \ dfun_name ->
    let
	inst_theta = [mkClassPred clas [mkTyVarTy tv] | tv <- tyvars]
	dfun_id    = mkDictFunId dfun_name tyvars inst_theta clas [inst_ty]
    in

    returnM (InstInfo { iDFunId = dfun_id, iBinds = VanillaInst binds [] })
\end{code}


%************************************************************************
%*									*
\subsection{Type-checking instance declarations, pass 2}
%*									*
%************************************************************************

\begin{code}
tcInstDecls2 :: [InstInfo] -> TcM TcMonoBinds
tcInstDecls2 inst_decls
  = mappM tcInstDecl2 inst_decls	`thenM` \ binds_s ->
    returnM (andMonoBindList binds_s)
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
tcInstDecl2 :: InstInfo -> TcM TcMonoBinds

tcInstDecl2 (InstInfo { iDFunId = dfun_id, iBinds = binds })
  =	 -- Prime error recovery
    recoverM (returnM EmptyMonoBinds)	$
    addSrcLoc (getSrcLoc dfun_id)			   	$
    addErrCtxt (instDeclCtxt (toHsType (idType dfun_id)))	$
    let
	inst_ty 	 = idType dfun_id
	(inst_tyvars, _) = tcSplitForAllTys inst_ty
		-- The tyvars of the instance decl scope over the 'where' part
		-- Those tyvars are inside the dfun_id's type, which is a bit
		-- bizarre, but OK so long as you realise it!
    in

	-- Instantiate the instance decl with tc-style type variables
    tcInstType InstTv inst_ty		`thenM` \ (inst_tyvars', dfun_theta', inst_head') ->
    let
	Just pred         = tcSplitPredTy_maybe inst_head'
	(clas, inst_tys') = getClassPredTys pred
        (class_tyvars, sc_theta, _, op_items) = classBigSig clas

        -- Instantiate the super-class context with inst_tys
	sc_theta' = substTheta (mkTyVarSubst class_tyvars inst_tys') sc_theta
	origin	  = InstanceDeclOrigin
    in
	 -- Create dictionary Ids from the specified instance contexts.
    newDicts origin sc_theta'		`thenM` \ sc_dicts ->
    newDicts origin dfun_theta'		`thenM` \ dfun_arg_dicts ->
    newDicts origin [pred] 		`thenM` \ [this_dict] ->
		-- Default-method Ids may be mentioned in synthesised RHSs,
		-- but they'll already be in the environment.

	------------------
	-- Typecheck the methods
    let		-- These insts are in scope; quite a few, eh?
	avail_insts = [this_dict] ++ dfun_arg_dicts ++ sc_dicts
    in
    tcMethods clas inst_tyvars inst_tyvars' 
	      dfun_theta' inst_tys' avail_insts 
	      op_items binds		`thenM` \ (meth_ids, meth_binds) ->

	-- Figure out bindings for the superclass context
    tcSuperClasses inst_tyvars' dfun_arg_dicts sc_dicts	
		`thenM` \ (zonked_inst_tyvars, sc_binds_inner, sc_binds_outer) ->

	-- Deal with 'SPECIALISE instance' pragmas by making them
	-- look like SPECIALISE pragmas for the dfun
    let
	uprags = case binds of
		       VanillaInst _ uprags -> uprags
		       other		    -> []
	spec_prags = [ SpecSig (idName dfun_id) ty loc
		     | SpecInstSig ty loc <- uprags ]
	xtve = inst_tyvars `zip` inst_tyvars'
    in
    tcExtendGlobalValEnv [dfun_id] (
	tcExtendTyVarEnv2 xtve		$
	tcSpecSigs spec_prags
    )					`thenM` \ prag_binds ->

	-- Create the result bindings
    let
        dict_constr   = classDataCon clas
	scs_and_meths = map instToId sc_dicts ++ meth_ids
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
		--
		--	See Note [Inline dfuns] below

	dict_rhs
	  | null scs_and_meths
	  = 	-- Blatant special case for CCallable, CReturnable
		-- If the dictionary is empty then we should never
		-- select anything from it, so we make its RHS just
		-- emit an error message.  This in turn means that we don't
		-- mention the constructor, which doesn't exist for CCallable, CReturnable
		-- Hardly beautiful, but only three extra lines.
	    HsApp (TyApp (HsVar rUNTIME_ERROR_ID) [idType this_dict_id])
		  (HsLit (HsStringPrim (mkFastString (stringToUtf8 msg))))

	  | otherwise	-- The common case
	  = mkHsConApp dict_constr inst_tys' (map HsVar scs_and_meths)
		-- We don't produce a binding for the dict_constr; instead we
		-- rely on the simplifier to unfold this saturated application
		-- We do this rather than generate an HsCon directly, because
		-- it means that the special cases (e.g. dictionary with only one
		-- member) are dealt with by the common MkId.mkDataConWrapId code rather
		-- than needing to be repeated here.

	  where
	    msg = "Compiler error: bad dictionary " ++ showSDoc (ppr clas)

	dict_bind  = VarMonoBind this_dict_id dict_rhs
	all_binds  = sc_binds_inner `AndMonoBinds` meth_binds `AndMonoBinds` dict_bind

	main_bind = AbsBinds
		 	 zonked_inst_tyvars
		 	 (map instToId dfun_arg_dicts)
		 	 [(inst_tyvars', dfun_id, this_dict_id)] 
		 	 inlines all_binds
    in
    showLIE (text "instance") 		`thenM_`
    returnM (main_bind `AndMonoBinds` prag_binds `AndMonoBinds` sc_binds_outer)


tcMethods clas inst_tyvars inst_tyvars' dfun_theta' inst_tys' 
	  avail_insts op_items (VanillaInst monobinds uprags)
  = 	-- Check that all the method bindings come from this class
    let
	sel_names = [idName sel_id | (sel_id, _) <- op_items]
	bad_bndrs = collectMonoBinders monobinds `minusList` sel_names
    in
    mappM (addErrTc . badMethodErr clas) bad_bndrs	`thenM_`

	-- Make the method bindings
    let
	mk_method_bind = mkMethodBind InstanceDeclOrigin clas inst_tys' monobinds
    in
    mapAndUnzipM mk_method_bind op_items 	`thenM` \ (meth_insts, meth_infos) ->

	-- And type check them
	-- It's really worth making meth_insts available to the tcMethodBind
	-- Consider	instance Monad (ST s) where
	--		  {-# INLINE (>>) #-}
	--		  (>>) = ...(>>=)...
	-- If we don't include meth_insts, we end up with bindings like this:
	--	rec { dict = MkD then bind ...
	--	      then = inline_me (... (GHC.Base.>>= dict) ...)
	--	      bind = ... }
	-- The trouble is that (a) 'then' and 'dict' are mutually recursive, 
	-- and (b) the inline_me prevents us inlining the >>= selector, which
	-- would unravel the loop.  Result: (>>) ends up as a loop breaker, and
	-- is not inlined across modules. Rather ironic since this does not
	-- happen without the INLINE pragma!  
	--
	-- Solution: make meth_insts available, so that 'then' refers directly
	-- 	     to the local 'bind' rather than going via the dictionary.
	--
	-- BUT WATCH OUT!  If the method type mentions the class variable, then
	-- this optimisation is not right.  Consider
	--	class C a where
	--	  op :: Eq a => a
	--
	--	instance C Int where
	--	  op = op
	-- The occurrence of 'op' on the rhs gives rise to a constraint
	--	op at Int
	-- The trouble is that the 'meth_inst' for op, which is 'available', also
	-- looks like 'op at Int'.  But they are not the same.
    let
	all_insts      = avail_insts ++ catMaybes meth_insts
	xtve	       = inst_tyvars `zip` inst_tyvars'
	tc_method_bind = tcMethodBind xtve inst_tyvars' dfun_theta' all_insts uprags 
    in
    mapM tc_method_bind meth_infos		`thenM` \ meth_binds_s ->
   
    returnM ([meth_id | (_,meth_id,_) <- meth_infos], 
	     andMonoBindList meth_binds_s)


-- Derived newtype instances
tcMethods clas inst_tyvars inst_tyvars' dfun_theta' inst_tys' 
	  avail_insts op_items (NewTypeDerived rep_tys)
  = getInstLoc InstanceDeclOrigin		`thenM` \ inst_loc ->
    mapAndUnzip3M (do_one inst_loc) op_items	`thenM` \ (meth_ids, meth_binds, rhs_insts) ->
    
    tcSimplifyCheck
	 (ptext SLIT("newtype derived instance"))
	 inst_tyvars' avail_insts rhs_insts	`thenM` \ lie_binds ->

	-- I don't think we have to do the checkSigTyVars thing

    returnM (meth_ids, lie_binds `AndMonoBinds` andMonoBindList meth_binds)

  where
    do_one inst_loc (sel_id, _)
	= -- The binding is like "op @ NewTy = op @ RepTy"
		-- Make the *binder*, like in mkMethodBind
	  tcInstClassOp inst_loc sel_id inst_tys'	`thenM` \ meth_inst ->

		-- Make the *occurrence on the rhs*
	  tcInstClassOp inst_loc sel_id rep_tys'	`thenM` \ rhs_inst ->
	  let
	     meth_id = instToId meth_inst
	  in
	  return (meth_id, VarMonoBind meth_id (HsVar (instToId rhs_inst)), rhs_inst)

	-- Instantiate rep_tys with the relevant type variables
    rep_tys' = map (substTy subst) rep_tys
    subst    = mkTyVarSubst inst_tyvars (mkTyVarTys inst_tyvars')
\end{code}

Note: [Superclass loops]
~~~~~~~~~~~~~~~~~~~~~~~~~
We have to be very, very careful when generating superclasses, lest we
accidentally build a loop. Here's an example:

  class S a

  class S a => C a where { opc :: a -> a }
  class S b => D b where { opd :: b -> b }
  
  instance C Int where
     opc = opd
  
  instance D Int where
     opd = opc

From (instance C Int) we get the constraint set {ds1:S Int, dd:D Int}
Simplifying, we may well get:
	$dfCInt = :C ds1 (opd dd)
	dd  = $dfDInt
	ds1 = $p1 dd
Notice that we spot that we can extract ds1 from dd.  

Alas!  Alack! We can do the same for (instance D Int):

	$dfDInt = :D ds2 (opc dc)
	dc  = $dfCInt
	ds2 = $p1 dc

And now we've defined the superclass in terms of itself.


Solution: treat the superclass context separately, and simplify it
all the way down to nothing on its own.  Don't toss any 'free' parts
out to be simplified together with other bits of context.
Hence the tcSimplifyTop below.

At a more basic level, don't include this_dict in the context wrt
which we simplify sc_dicts, else sc_dicts get bound by just selecting
from this_dict!!

\begin{code}
tcSuperClasses inst_tyvars' dfun_arg_dicts sc_dicts
  = addErrCtxt superClassCtxt 	$
    getLIE (tcSimplifyCheck doc inst_tyvars'
			    dfun_arg_dicts
			    sc_dicts)		`thenM` \ (sc_binds1, sc_lie) ->

	-- It's possible that the superclass stuff might have done unification
    checkSigTyVars inst_tyvars' 	`thenM` \ zonked_inst_tyvars ->

	-- We must simplify this all the way down 
	-- lest we build superclass loops
	-- See Note [Superclass loops] above
    tcSimplifyTop sc_lie		`thenM` \ sc_binds2 ->

    returnM (zonked_inst_tyvars, sc_binds1, sc_binds2)

  where
    doc = ptext SLIT("instance declaration superclass context")
\end{code}


		------------------------------
	[Inline dfuns] Inlining dfuns unconditionally
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
  = addSrcLoc (tcdLoc decl) 	$
    addErrCtxt ctxt 	$
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
