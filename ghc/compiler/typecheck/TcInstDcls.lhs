%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcInstDecls]{Typechecking instance declarations}

\begin{code}
#include "HsVersions.h"

module TcInstDcls (
	tcInstDecls1, tcInstDecls2,
	tcSpecInstSigs,
	buildInstanceEnvs, processInstBinds,
	mkInstanceRelatedIds,
	InstInfo(..)
    ) where

IMPORT_Trace		-- ToDo:rm debugging
import Outputable
import Pretty

import TcMonad		-- typechecking monad machinery
import TcMonadFns	( newDicts, newMethod, newLocalWithGivenTy,
			  newClassOpLocals, copyTyVars,
			  applyTcSubstAndCollectTyVars
			)
import AbsSyn		-- the stuff being typechecked
import AbsPrel		( pAT_ERROR_ID )
import AbsUniType
import BackSubst	( applyTcSubstToBinds )
import Bag		( emptyBag, unitBag, unionBags, bagToList )
import CE		( lookupCE, CE(..) )
import CmdLineOpts	( GlobalSwitch(..) )
import GenSpecEtc	( checkSigTyVars, SignatureInfo )
import E		( mkE, getE_CE, getE_TCE, growE_LVE, tvOfE, LVE(..), E )
import Errors		( dupInstErr, derivingWhenInstanceExistsErr,
			  preludeInstanceErr, nonBoxedPrimCCallErr,
			  specInstUnspecInstNotFoundErr,
			  Error(..), UnifyErrContext(..)
			)
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import Id		-- lots of things
import IdInfo		-- ditto
import Inst		( Inst, InstOrigin(..) )
import InstEnv
import Maybes		( catMaybes, mkLookupFun, maybeToBool, Maybe(..) )
import Name		( getTagFromClassOpName )
import NameTypes	( fromPrelude )
import PlainCore	( escErrorMsg )
import LIE		( nullLIE, mkLIE, unMkLIE, plusLIE, LIE )
import ListSetOps	( minusList )
import TCE		( TCE(..), UniqFM )
import TVE		( mkTVE, TVE(..) )
import Spec		( specTy )
import TcContext	( tcContext )
import TcBinds		( tcSigs, doSpecPragma )
import TcGRHSs		( tcGRHSsAndBinds )
import TcMatches	( tcMatchesFun )
import TcMonoType	( tcInstanceType )
import TcPragmas	( tcDictFunPragmas, tcGenPragmas )
import TcSimplify	( tcSimplifyAndCheck, tcSimplifyThetas )
import Unify		( unifyTauTy )
import Unique		( cCallableClassKey, cReturnableClassKey )
import Util
\end{code}

Typechecking instance declarations is done in two passes. The first
pass, made by @tcInstDecls1@,
collects information to be used in the second pass.

This pre-processed info includes the as-yet-unprocessed bindings
inside the instance declaration.  These are type-checked in the second
pass, when the class-instance envs and GVE contain all the info from
all the instance and value decls.  Indeed that's the reason we need
two passes over the instance decls.

    instance c => k (t tvs) where b

\begin{code}
data InstInfo
  = InstInfo
      Class	        -- Class, k
      [TyVarTemplate]	-- Type variables, tvs
      UniType		-- The type at which the class is being
			--   instantiated
      ThetaType		-- inst_decl_theta: the original context from the
			--   instance declaration.  It constrains (some of)
			--   the TyVarTemplates above
      ThetaType		-- dfun_theta: the inst_decl_theta, plus one
			--   element for each superclass; the "Mark
			--   Jones optimisation"
      Id		-- The dfun id
      [Id]		-- Constant methods (either all or none)
      RenamedMonoBinds	-- Bindings, b
      Bool		-- True <=> local instance decl
      FAST_STRING	-- Name of module where this instance was
			-- defined.
      SrcLoc		-- Source location assoc'd with this instance's defn
      [RenamedSig]	-- User pragmas recorded for generating specialised methods
\end{code}


Here is the overall algorithm.	Assume that

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
tcInstDecls1 :: E -> CE -> TCE -> [RenamedInstDecl] -> NF_TcM (Bag InstInfo)

tcInstDecls1 e ce tce [] = returnNF_Tc emptyBag

tcInstDecls1 e ce tce (inst_decl : rest)
  = tc_inst_1 inst_decl 	`thenNF_Tc` \ infos1 ->
    tcInstDecls1 e ce tce rest	`thenNF_Tc` \ infos2 ->
    returnNF_Tc (infos1 `unionBags` infos2)
  where
    tc_inst_1 (InstDecl context class_name ty binds from_here modname imod uprags pragmas src_loc)
      =
	    -- Prime error recovery and substitution pruning
	recoverTc emptyBag			(
	addSrcLocTc src_loc			(

	let
	    clas = lookupCE ce class_name -- Renamer ensures this can't fail

	    for_ccallable_or_creturnable
	      = class_name == cCallableClass || class_name == cReturnableClass
	      where
	       cCallableClass   = PreludeClass cCallableClassKey   bottom
	       cReturnableClass = PreludeClass cReturnableClassKey bottom
	       bottom		= panic "for_ccallable_etc"

	    -- Make some new type variables, named as in the instance type
	    ty_names	    	= extractMonoTyNames (==) ty
	    (tve,inst_tyvars,_) = mkTVE ty_names
	in
	    -- Check the instance type, including its syntactic constraints
	babyTcMtoTcM (tcInstanceType ce tce tve from_here src_loc ty)
		`thenTc` \ inst_ty ->

	    -- DEAL WITH THE INSTANCE CONTEXT
	babyTcMtoTcM (tcContext ce tce tve context) `thenTc` \ theta ->

	    -- SOME BORING AND TURGID CHECKING:
	let
	    inst_for_function_type = isFunType inst_ty
		-- sigh; it happens; must avoid tickling inst_tycon

	    inst_tycon_maybe = getUniDataTyCon_maybe inst_ty

	    inst_tycon = case inst_tycon_maybe of
			   Just (xx,_,_) -> xx
			   Nothing 	 -> panic "tcInstDecls1:inst_tycon"
	in
	    -------------------------------------------------------------
	    -- It is illegal for a normal user's module to declare an
	    -- instance for a Prelude-class/Prelude-type instance:
	checkTc (from_here	    	      -- really an inst decl in this module
		 && fromPreludeCore clas      -- prelude class
		 && (inst_for_function_type   -- prelude type
		     || fromPreludeCore inst_tycon)
		 && not (fromPrelude modname) -- we aren't compiling a Prelude mod
		)
		(preludeInstanceErr clas inst_ty src_loc) `thenTc_`

	    -------------------------------------------------------------
	    -- It is obviously illegal to have an explicit instance
	    -- for something that we are also planning to `derive'.
	    -- Note that an instance decl coming in from outside
	    -- is probably just telling us about the derived instance
	    -- (ToDo: actually check, if possible), so we mustn't flag
	    -- it as an error.
	checkTc (from_here
		 && not inst_for_function_type
		 && clas `derivedFor` inst_tycon)
		(derivingWhenInstanceExistsErr clas inst_tycon) `thenTc_`

	    -------------------------------------------------------------
	    -- A user declaration of a _CCallable/_CReturnable instance
	    -- must be for a "boxed primitive" type.
        getSwitchCheckerTc	`thenNF_Tc` \ sw_chkr ->
	checkTc (for_ccallable_or_creturnable
		 && from_here			    -- instance defined here
		 && not (sw_chkr CompilingPrelude)  -- which allows anything
		 && (inst_for_function_type ||	    -- a *function*??? hah!
		  not (maybeToBool (maybeBoxedPrimType inst_ty))))   -- naughty, naughty
		(nonBoxedPrimCCallErr clas inst_ty src_loc) `thenTc_`

	    -- END OF TURGIDITY; back to real fun
	    -------------------------------------------------------------

	if (not inst_for_function_type && clas `derivedFor` inst_tycon) then
	    -- Don't use this InstDecl; tcDeriv will make the
	    -- InstInfo to be used in later processing.
	    returnTc emptyBag

	else
		-- Make the dfun id and constant-method ids
	    mkInstanceRelatedIds e
			from_here modname pragmas src_loc
			clas inst_tyvars inst_ty theta uprags
				`thenTc` \ (dfun_id, dfun_theta, const_meth_ids) ->

	    returnTc ( unitBag (
	      InstInfo clas inst_tyvars inst_ty theta
		       dfun_theta dfun_id const_meth_ids 
		       binds from_here modname src_loc uprags
	    ))
	))
\end{code}


Common bit of code shared with @tcDeriving@:
\begin{code}
mkInstanceRelatedIds e
		from_here modname inst_pragmas locn
		clas 
		inst_tyvars inst_ty inst_decl_theta uprags
  = getUniqueTc 			`thenNF_Tc` \ uniq -> 
    let     
	(class_tyvar, super_classes, _, class_ops, _, _) = getClassBigSig clas

	super_class_theta = super_classes `zip` (repeat inst_ty)


	dfun_theta = case inst_decl_theta of

			[]    -> []	-- If inst_decl_theta is empty, then we don't
					-- want to have any dict arguments, so that we can
					-- expose the constant methods.

			other -> inst_decl_theta ++ super_class_theta
					-- Otherwise we pass the superclass dictionaries to 
					-- the dictionary function; the Mark Jones optimisation.

	dfun_ty = mkSigmaTy inst_tyvars dfun_theta (mkDictTy clas inst_ty)
    in
    fixNF_Tc ( \ rec_dfun_id ->
	babyTcMtoNF_TcM (
	    tcDictFunPragmas e dfun_ty rec_dfun_id inst_pragmas
	)			`thenNF_Tc` \ dfun_pragma_info ->
	let
	    dfun_specenv = mkInstSpecEnv clas inst_ty inst_tyvars dfun_theta
	    dfun_info = dfun_pragma_info `addInfo` dfun_specenv
	in
	returnNF_Tc (mkDictFunId uniq clas inst_ty dfun_ty from_here modname dfun_info)
    ) `thenNF_Tc` \ dfun_id ->

	-- Make the constant-method ids, if there are no type variables involved
    (if not (null inst_tyvars)	-- ToDo: could also do this if theta is null...
     then
	returnNF_Tc []
     else
	let
	    inline_mes = [ getTagFromClassOpName v | (InlineSig v _ _) <- uprags ]

            mk_const_meth op uniq
              = mkConstMethodId 
                        uniq
                        clas op inst_ty
                        meth_ty from_here modname info
              where
		is_elem = isIn "mkInstanceRelatedIds"

		info	= if tag `is_elem` inline_mes
			  then noIdInfo `addInfo_UF` (iWantToBeINLINEd UnfoldAlways)
			  else noIdInfo

                tenv    = [(class_tyvar, inst_ty)]
		tag 	= getClassOpTag op
                op_ty   = getClassOpLocalType op
                meth_ty = instantiateTy tenv op_ty
                          -- If you move to a null-theta version, you need a 
                          -- mkForallTy inst_tyvars here

	    mk_constm_w_info (op, u, (name, prags)) -- ToDo: chk name?
	      = fixNF_Tc ( \ rec_constm_id ->

		    babyTcMtoNF_TcM (tcGenPragmas e (Just meth_ty) rec_constm_id prags)
				`thenNF_Tc` \ id_info ->

		    returnNF_Tc (mkConstMethodId u clas op inst_ty meth_ty
					from_here modname id_info)
		)
	      where
		tenv    = [(class_tyvar, inst_ty)]
		op_ty   = getClassOpLocalType op
		meth_ty = instantiateTy tenv op_ty

	in
	getUniquesTc (length class_ops)	`thenNF_Tc` \ constm_uniqs ->
	(case inst_pragmas of
	   ConstantInstancePragma _ name_pragma_pairs ->
	     mapNF_Tc mk_constm_w_info (zip3 class_ops constm_uniqs name_pragma_pairs)

	   other_inst_pragmas ->
	     returnNF_Tc (zipWith mk_const_meth class_ops constm_uniqs)
	)
    )		`thenNF_Tc` \ const_meth_ids ->

    returnTc (dfun_id, dfun_theta, const_meth_ids)
\end{code}


%************************************************************************
%*									*
\subsection{Converting instance info into suitable InstEnvs}
%*									*
%************************************************************************

\begin{code}
buildInstanceEnvs :: Bag InstInfo 
	          -> TcM InstanceMapper

buildInstanceEnvs info
  = let
    	cmp :: InstInfo -> InstInfo -> TAG_
    	(InstInfo c1 _ _ _ _ _ _ _ _ _ _ _) `cmp` (InstInfo c2 _ _ _ _ _ _ _ _ _ _ _)
	  = if c1 == c2 then EQ_ else if c1 < c2 then LT_ else GT_

	info_by_class = equivClasses cmp (bagToList info)
    in
    mapTc buildInstanceEnv info_by_class    `thenTc` \ inst_env_entries ->
    let
	class_lookup_maybe_fn
	    :: Class
	    -> Maybe (ClassInstEnv, (ClassOp -> SpecEnv))
	class_lookup_fn
	    :: InstanceMapper

	class_lookup_maybe_fn = mkLookupFun (==) inst_env_entries

    	class_lookup_fn c
	  = case class_lookup_maybe_fn c of
	      Nothing -> (nullMEnv, \ o -> nullSpecEnv)
	      Just xx -> xx
    in
    returnTc class_lookup_fn
\end{code}

\begin{code}
buildInstanceEnv :: [InstInfo]		-- Non-empty, and all for same class
	         -> TcM (Class, (ClassInstEnv, (ClassOp -> SpecEnv)))

buildInstanceEnv inst_infos@(info_for_one@(InstInfo clas _ _ _ _ _ _ _ _ _ _ _) : rest)
  = let
	ops	  = getClassOps clas
	no_of_ops = length ops
    in
    foldlTc addClassInstance
	    (nullMEnv, nOfThem no_of_ops nullSpecEnv)
	    inst_infos	    `thenTc` \ (class_inst_env, op_inst_envs) ->
    let
	class_op_maybe_fn :: ClassOp -> Maybe SpecEnv
	class_op_fn	  :: ClassOp -> SpecEnv

	class_op_maybe_fn = mkLookupFun (==) (ops `zip` op_inst_envs)
			-- They compare by ClassOp tags
    	class_op_fn op
	  = case class_op_maybe_fn op of
	      Nothing -> nullSpecEnv
	      Just xx -> xx
    in
    returnTc (clas, (class_inst_env, class_op_fn))
\end{code}

\begin{code}
addClassInstance
    :: (ClassInstEnv, [SpecEnv])
    -> InstInfo
    -> TcM (ClassInstEnv, [SpecEnv])	-- One SpecEnv for each class op

addClassInstance
    (class_inst_env, op_spec_envs) 
    (InstInfo clas inst_tyvars inst_ty inst_decl_theta dfun_theta dfun_id const_meth_ids _ _ _ src_loc _)
  = getSwitchCheckerTc		`thenNF_Tc` \ sw_chkr ->
	-- We anly add specialised/overlapped instances
	-- if we are specialising the overloading
--
-- ToDo ... This causes getConstMethodId errors!
--
--    if is_plain_instance inst_ty || sw_chkr SpecialiseOverloaded
--    then

	-- Insert into the class_inst_env first
	checkMaybeErrTc (addClassInst clas class_inst_env inst_ty dfun_id inst_tyvars dfun_theta src_loc)
		        dupInstErr		`thenTc` \ class_inst_env' ->
	let 
		-- Adding the classop instances can't fail if the class instance itself didn't
	    op_spec_envs' = case const_meth_ids of
			      []    -> op_spec_envs
			      other -> zipWith add_const_meth op_spec_envs const_meth_ids
    	in
    	returnTc (class_inst_env', op_spec_envs')

--    else
--	-- Drop this specialised/overlapped instance
--	returnTc (class_inst_env, op_spec_envs) 

  where
    add_const_meth spec_env meth_id
      = addOneToSpecEnv spec_env (SpecInfo (Just inst_ty:nothings) 1 meth_id)
      where
	(const_meth_tyvars,_) = splitForalls (getIdUniType meth_id)
	nothings = [Nothing | _ <- const_meth_tyvars]
	-- This only works if the constant method id only has its local polymorphism.
	-- If you want to have constant methods for
	-- 				instance Foo (a,b,c) where
	--					op x = ...
	-- then the constant method will be polymorphic in a,b,c, and
	-- the SpecInfo will need to be elaborated.

\end{code}

%************************************************************************
%*									*
\subsection{Type-checking instance declarations, pass 2}
%*									*
%************************************************************************

\begin{code}
tcInstDecls2 :: E 
	     -> Bag InstInfo
	     -> NF_TcM (LIE, TypecheckedBinds)

tcInstDecls2 e inst_decls 
  = let
	-- Get type variables free in environment. Sadly, there may be
	-- some, because of the dreaded monomorphism restriction
	free_tyvars = tvOfE e
    in
    tcInstDecls2_help e free_tyvars (bagToList inst_decls)

tcInstDecls2_help e free_tyvars [] = returnNF_Tc (nullLIE, EmptyBinds)

tcInstDecls2_help e free_tyvars (inst_decl:inst_decls)
 = tcInstDecl2       e free_tyvars inst_decl	`thenNF_Tc` \ (lie1, binds1) ->
   tcInstDecls2_help e free_tyvars inst_decls	`thenNF_Tc` \ (lie2, binds2) ->
   returnNF_Tc (lie1 `plusLIE` lie2, binds1 `ThenBinds` binds2)
\end{code}


======= New documentation starts here (Sept 92)	 ==============

The main purpose of @tcInstDecl2@ is to return a @Binds@ which defines
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

HOWEVER, if the instance decl has no type variables, then it returns a
bigger @Binds@ with declarations for each method.  For example
\begin{verbatim}
	instance Foo Int where
		op1 x = ...
		op2 y = ...
\end{verbatim}
might produce
\begin{verbatim}
	dfun.Foo.Int = Dict [Foo.op1.Int, Foo.op2.Int]
	Foo.op1.Int x = ...
	Foo.op2.Int y = ...
\end{verbatim}
This group may be mutually recursive, because (for example) there may
be no method supplied for op2 in which case we'll get
\begin{verbatim}
	Foo.op2.Int = default.Foo.op2 dfun.Foo.Int
\end{verbatim}
that is, the default method applied to the dictionary at this type.

\begin{code}
tcInstDecl2 :: E
	    -> [TyVar]		-- Free in the environment
	    -> InstInfo 
	    -> NF_TcM (LIE, TypecheckedBinds)
\end{code}

First comes the easy case of a non-local instance decl.

\begin{code}
tcInstDecl2 e free_tyvars (InstInfo _ _ _ _ _ _ _ _ False{-not this module-} _ _ _)
  = returnNF_Tc (nullLIE, EmptyBinds)
\end{code}

Now the case of a general local instance.  For an instance declaration, say,

	instance (C1 a, C2 b) => C (T a b) where
		...

where the {\em immediate} superclasses of C are D1, D2, we build a dictionary
function whose type is

	(C1 a, C2 b, D1 (T a b), D2 (T a b)) => C (T a b)

Notice that we pass it the superclass dictionaries at the instance type; this
is the ``Mark Jones optimisation''.  The stuff before the "=>" here
is the @dfun_theta@ below.

\begin{code}
tcInstDecl2
    e free_tyvars 
    (InstInfo clas template_tyvars inst_ty_tmpl inst_decl_theta dfun_theta
	      dfun_id const_meth_ids monobinds True{-from here-} inst_mod locn uprags)
  = let
	origin = InstanceDeclOrigin locn
    in
    recoverTc (nullLIE, EmptyBinds) 	(
    addSrcLocTc locn			(
    pruneSubstTc free_tyvars 		(

	-- Get the class signature
    let (class_tyvar, 
	 super_classes, sc_sel_ids,
	 class_ops, op_sel_ids, defm_ids) = getClassBigSig clas
    in
	 -- Prime error recovery and substitution pruning. Instantiate
	 -- dictionaries from the specified instance context. These
	 -- dicts will be passed into the dictionary-construction
	 -- function.
    copyTyVars template_tyvars	`thenNF_Tc` \ (inst_env, inst_tyvars, inst_tyvar_tys) ->
    let 
	inst_ty 	 = instantiateTy inst_env inst_ty_tmpl

	inst_decl_theta' = instantiateThetaTy inst_env inst_decl_theta
	dfun_theta'	 = instantiateThetaTy inst_env dfun_theta
	sc_theta' 	 = super_classes `zip` (repeat inst_ty)
    in
    newDicts origin sc_theta'			`thenNF_Tc` \ sc_dicts' ->
    newDicts origin dfun_theta'			`thenNF_Tc` \ dfun_arg_dicts' ->
    newDicts origin inst_decl_theta'		`thenNF_Tc` \ inst_decl_dicts' ->
    let
	sc_dicts'_ids       = map mkInstId sc_dicts'
	dfun_arg_dicts'_ids = map mkInstId dfun_arg_dicts'
    in
	-- Instantiate the dictionary being constructed 
	-- and the dictionary-construction function
    newDicts origin [(clas,inst_ty)]		`thenNF_Tc` \ [this_dict] ->
    let
	this_dict_id = mkInstId this_dict
    in
	 -- Instantiate method variables
    listNF_Tc [ newMethodId sel_id inst_ty origin locn
	      |	sel_id <- op_sel_ids
	      ]					`thenNF_Tc` \ method_ids ->
    let 
	method_insts = catMaybes (map isInstId_maybe method_ids)
	-- Extract Insts from those method ids which have them (most do)
	-- See notes on newMethodId
    in
	 -- Collect available dictionaries
    let avail_insts =	 -- These insts are in scope; quite a few, eh?
	    [this_dict]		++
	    method_insts	++
	    dfun_arg_dicts'
    in
    getSwitchCheckerTc			`thenNF_Tc` \ sw_chkr ->
    let
	mk_method_expr
	  = if sw_chkr OmitDefaultInstanceMethods then
		makeInstanceDeclNoDefaultExpr origin clas method_ids defm_ids inst_mod inst_ty
	    else
		makeInstanceDeclDefaultMethodExpr origin this_dict_id class_ops defm_ids inst_ty
    in
    processInstBinds e free_tyvars mk_method_expr
	inst_tyvars avail_insts method_ids monobinds
					 `thenTc` \ (insts_needed, method_mbinds) ->
    let
	-- Create the dict and method binds
	dict_bind
	    = VarMonoBind this_dict_id (Dictionary sc_dicts'_ids method_ids)

	dict_and_method_binds
	    = dict_bind `AndMonoBinds` method_mbinds
    in
	-- Check the overloading constraints of the methods and superclasses
	-- The global tyvars must be a fixed point of the substitution
    applyTcSubstAndCollectTyVars free_tyvars  `thenNF_Tc` \ real_free_tyvars ->
    tcSimplifyAndCheck
		 True				-- Top level
		 real_free_tyvars		-- Global tyvars
		 inst_tyvars			-- Local tyvars
		 avail_insts
		 (sc_dicts' ++ insts_needed)	-- Need to get defns for all these
		 (BindSigCtxt method_ids)
					 `thenTc` \ (const_insts, super_binds) ->

	-- Check that we *could* construct the superclass dictionaries,
	-- even though we are *actually* going to pass the superclass dicts in;
	-- the check ensures that the caller will never have a problem building
	-- them.
    tcSimplifyAndCheck
		 False				-- Doesn't matter; more efficient this way
		 real_free_tyvars		-- Global tyvars
		 inst_tyvars			-- Local tyvars
		 inst_decl_dicts'		-- The instance dictionaries available
		 sc_dicts'			-- The superclass dicationaries reqd
		 SuperClassSigCtxt
						 `thenTc_`
						-- Ignore the result; we're only doing
						-- this to make sure it can be done.

	-- Now process any SPECIALIZE pragmas for the methods
    let
	spec_sigs = [ s | s@(SpecSig _ _ _ _) <- uprags ]

	get_const_method_id name
	  = const_meth_ids !! ((getTagFromClassOpName name) - 1)
    in
    tcSigs e [] spec_sigs		`thenTc` \ sig_info ->

    mapAndUnzipTc (doSpecPragma e get_const_method_id) sig_info
					`thenTc` \ (spec_binds_s, spec_lie_s) ->
    let 
	spec_lie   = foldr plusLIE nullLIE spec_lie_s
	spec_binds = foldr AndMonoBinds EmptyMonoBinds spec_binds_s

	-- Complete the binding group, adding any spec_binds
        inst_binds
	  = AbsBinds 
		 inst_tyvars
		 dfun_arg_dicts'_ids
		 ((this_dict_id,dfun_id) : (method_ids `zip` const_meth_ids))
			-- const_meth_ids will often be empty
		 super_binds
		 (RecBind dict_and_method_binds)
	    
	    `ThenBinds`
	    SingleBind (NonRecBind spec_binds)
    in
	 -- Back-substitute
    applyTcSubstToBinds inst_binds `thenNF_Tc` \ final_inst_binds ->

    returnTc (mkLIE const_insts `plusLIE` spec_lie,
	      final_inst_binds)
    )))
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
  = let (sel_tyvars,sel_theta,sel_tau) = splitType (getIdUniType sel_id)
	(_:meth_theta) = sel_theta	-- The local theta is all except the
					-- first element of the context
    in 
       case sel_tyvars of
	-- Ah! a selector for a class op with no local polymorphism
	-- Build an Inst for this
	[clas_tyvar] -> newMethod origin sel_id [inst_ty]	`thenNF_Tc` \ inst ->
			returnNF_Tc (mkInstId inst)

	-- Ho! a selector for a class op with local polymorphism.
	-- Just make a suitably typed local id for this
	(clas_tyvar:local_tyvars) -> 
		let
		    method_ty = instantiateTy [(clas_tyvar,inst_ty)]
				    (mkSigmaTy local_tyvars meth_theta sel_tau)
		in
		getUniqueTc		`thenNF_Tc` \ uniq -> 
		returnNF_Tc (mkUserLocal (getOccurrenceName sel_id) uniq method_ty loc)
\end{code}

This function makes a default method which calls the global default method, at
the appropriate instance type.

See the notes under default decls in TcClassDcl.lhs.

\begin{code}
makeInstanceDeclDefaultMethodExpr
	:: InstOrigin
	-> Id
	-> [ClassOp]
	-> [Id]
	-> UniType
	-> Int
	-> NF_TcM TypecheckedExpr
	
makeInstanceDeclDefaultMethodExpr origin this_dict_id class_ops defm_ids inst_ty tag
  = let
	(tyvar_tmpls, local_theta, _) = splitType (getClassOpLocalType class_op)
    in
    copyTyVars tyvar_tmpls	`thenNF_Tc` \ (inst_env, tyvars, tys) ->
    let
	inst_theta = instantiateThetaTy inst_env local_theta
    in
    newDicts origin inst_theta	`thenNF_Tc` \ local_dict_insts ->
    let
	local_dicts = map mkInstId local_dict_insts
    in
    returnNF_Tc (
      mkTyLam tyvars (
	mkDictLam local_dicts (
	  mkDictApp (mkTyApp (Var defm_id)
			     (inst_ty : tys))
		    (this_dict_id:local_dicts)))
    )
 where
    idx	     = tag - 1
    class_op = class_ops !! idx
    defm_id  = defm_ids  !! idx


makeInstanceDeclNoDefaultExpr
	:: InstOrigin
	-> Class
	-> [Id]
	-> [Id]
	-> FAST_STRING
	-> UniType
	-> Int
	-> NF_TcM TypecheckedExpr
	
makeInstanceDeclNoDefaultExpr origin clas method_ids defm_ids inst_mod inst_ty tag
  = specTy origin (getIdUniType method_id) `thenNF_Tc` \ (tyvars, dicts, tau) ->

    (if not err_defm then
	 pprTrace "Warning: "
	 (ppCat [ppStr "Omitted default method for",
		 ppr PprForUser clas_op, ppStr "in instance",
		 ppPStr clas_name, pprParendUniType PprForUser inst_ty])
    else id) (

    returnNF_Tc (mkTyLam tyvars (
		 mkDictLam (map mkInstId dicts) (
		 App (mkTyApp (Var pAT_ERROR_ID) [tau])
		     (Lit (StringLit (_PK_ error_msg))))))
    )
  where
    idx	      = tag - 1
    clas_op   = (getClassOps clas) !! idx
    method_id = method_ids  !! idx
    defm_id   = defm_ids  !! idx

    Just (_, _, err_defm) = isDefaultMethodId_maybe defm_id

    error_msg = "%E" 	-- => No explicit method for \"
	     	++ escErrorMsg error_str

    error_str = _UNPK_ inst_mod ++ "." ++ _UNPK_ clas_name ++ "."
	     	++ (ppShow 80 (ppr PprForUser inst_ty)) ++ "."
	     	++ (ppShow 80 (ppr PprForUser clas_op))	++ "\""

    (_, clas_name) = getOrigName clas
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
	:: E
	-> [TyVar]			   -- Free in envt

	-> (Int -> NF_TcM TypecheckedExpr) -- Function to make
					   -- default method

	-> [TyVar]			   -- Tyvars for this instance decl

	-> [Inst]			   -- available Insts

	-> [Id]				   -- Local method ids 
					   --	(instance tyvars are free 
					   --	in their types),
					   --	in tag order
	-> RenamedMonoBinds

	-> TcM ([Inst],			-- These are required
		TypecheckedMonoBinds)

processInstBinds e free_tyvars mk_method_expr inst_tyvars
		 avail_insts method_ids monobinds
  = 
	 -- Process the explicitly-given method bindings
    processInstBinds1 e free_tyvars inst_tyvars avail_insts method_ids monobinds
	 `thenTc` (\ (tags, insts_needed_in_methods, method_binds) ->

	 -- Find the methods not handled, and make default method bindings for them.
    let unmentioned_tags = [1.. length method_ids] `minusList` tags
    in
    makeDefaultMethods mk_method_expr unmentioned_tags method_ids
					 `thenNF_Tc`	(\ default_monobinds ->

    returnTc (insts_needed_in_methods, 
	      method_binds `AndMonoBinds` default_monobinds)
    ))
\end{code}

\begin{code}
processInstBinds1
	:: E
	-> [TyVar]		-- Global free tyvars
	-> [TyVar]		-- Tyvars for this instance decl
	-> [Inst]		-- available Insts
	-> [Id]			-- Local method ids (instance tyvars are free),
				--	in tag order
	-> RenamedMonoBinds 
	-> TcM ([Int],		-- Class-op tags accounted for
		[Inst],		-- These are required
		TypecheckedMonoBinds)

processInstBinds1 e free_tyvars inst_tyvars avail_insts method_ids EmptyMonoBinds
  = returnTc ([], [], EmptyMonoBinds)

processInstBinds1 e free_tyvars inst_tyvars avail_insts method_ids (AndMonoBinds mb1 mb2)
  = processInstBinds1 e free_tyvars inst_tyvars avail_insts method_ids mb1
				 `thenTc`	\ (op_tags1,dicts1,method_binds1) ->
    processInstBinds1 e free_tyvars inst_tyvars avail_insts method_ids mb2
				 `thenTc`	\ (op_tags2,dicts2,method_binds2) ->
    returnTc (op_tags1 ++ op_tags2,
	      dicts1 ++ dicts2,
	      AndMonoBinds method_binds1 method_binds2)
\end{code}

\begin{code}
processInstBinds1 e free_tyvars inst_tyvars avail_insts method_ids mbind
  = 
    -- Find what class op is being defined here.  The complication is
    -- that we could have a PatMonoBind or a FunMonoBind.  If the
    -- former, it should only bind a single variable, or else we're in
    -- trouble (I'm not sure what the static semantics of methods
    -- defined in a pattern binding with multiple patterns is!)
    -- Renamer has reduced us to these two cases.
    let
	(op,locn) = case mbind of
		      FunMonoBind op _ locn	       -> (op, locn)
		      PatMonoBind (VarPatIn op) _ locn -> (op, locn)
    
	origin = InstanceDeclOrigin locn
    in
    addSrcLocTc locn			 (

    -- Make a method id for the method
    let tag       = getTagFromClassOpName op
        method_id = method_ids !! (tag-1)
	method_ty = getIdUniType method_id
    in
    specTy origin method_ty  `thenNF_Tc` \ (method_tyvars, method_dicts, method_tau) ->

	-- Build the result
    case (method_tyvars, method_dicts) of

      ([],[]) -> -- The simple case; no local polymorphism or overloading in the method

		-- Type check the method itself
	tcMethodBind e method_id method_tau mbind    `thenTc` \ (mbind', lieIop) ->

		-- Make sure that the instance tyvars havn't been
		-- unified with each other or with the method tyvars.
		-- The global tyvars must be a fixed point of the substitution
	applyTcSubstAndCollectTyVars free_tyvars `thenNF_Tc` \ real_free_tyvars ->
	checkSigTyVars real_free_tyvars inst_tyvars method_tau method_tau
			      (MethodSigCtxt op method_tau) `thenTc_`

	returnTc ([tag], unMkLIE lieIop, mbind')

      other ->	-- It's a locally-polymorphic and/or overloaded method; UGH!

		 -- Make a new id for (a) the local, non-overloaded method
		 -- and		      (b) the locally-overloaded method
		 -- The latter is needed just so we can return an AbsBinds wrapped
		 -- up inside a MonoBinds.
	newLocalWithGivenTy op method_tau	`thenNF_Tc` \ local_meth_id ->
	newLocalWithGivenTy op method_ty	`thenNF_Tc` \ copy_meth_id ->

		-- Typecheck the method
	tcMethodBind e local_meth_id method_tau mbind `thenTc` \ (mbind', lieIop) ->

		-- Make sure that the instance tyvars haven't been
		-- unified with each other or with the method tyvars.
		-- The global tyvars must be a fixed point of the substitution
	applyTcSubstAndCollectTyVars free_tyvars `thenNF_Tc` \ real_free_tyvars ->
	checkSigTyVars real_free_tyvars (method_tyvars ++ inst_tyvars) method_tau method_tau
			      (MethodSigCtxt op method_tau) `thenTc_`

		-- Check the overloading part of the signature.
		-- Simplify everything fully, even though some
		-- constraints could "really" be left to the next
		-- level out. The case which forces this is
		--
		-- 	class Foo a where { op :: Bar a => a -> a }
		--
		-- Here we must simplify constraints on "a" to catch all
		-- the Bar-ish things.
	tcSimplifyAndCheck
		False			-- Not top level
		real_free_tyvars 
		(inst_tyvars ++ method_tyvars)
		(method_dicts ++ avail_insts)
		(unMkLIE lieIop)	
		(MethodSigCtxt op method_ty) 	`thenTc` \ (f_dicts, dict_binds) ->

	returnTc ([tag],
		  f_dicts,
		  VarMonoBind method_id
			 (Let
			     (AbsBinds
				method_tyvars
				(map mkInstId method_dicts)
				[(local_meth_id, copy_meth_id)]
				dict_binds
				(NonRecBind mbind'))
			     (Var copy_meth_id)))
    )
\end{code}

\begin{code}
tcMethodBind :: E -> Id -> UniType -> RenamedMonoBinds 
	    -> TcM (TypecheckedMonoBinds, LIE)

tcMethodBind e meth_id meth_ty (FunMonoBind name matches locn)
  = addSrcLocTc locn				 (
    tcMatchesFun e name meth_ty matches `thenTc` \ (rhs', lie) ->
    returnTc (FunMonoBind meth_id rhs' locn, lie)
    )

tcMethodBind e meth_id meth_ty (PatMonoBind pat grhss_and_binds locn)
  -- pat is sure to be a (VarPatIn op)
  = addSrcLocTc locn				 (
    tcGRHSsAndBinds e grhss_and_binds 	`thenTc` \ (grhss_and_binds', lie, rhs_ty) ->
    unifyTauTy meth_ty rhs_ty (PatMonoBindsCtxt pat grhss_and_binds) `thenTc_`
    returnTc (PatMonoBind (VarPat meth_id) grhss_and_binds' locn, lie)
    )
\end{code}


Creates bindings for the default methods, being the application of the
appropriate global default method to the type of this instance decl.

\begin{code}
makeDefaultMethods 
	:: (Int -> NF_TcM TypecheckedExpr)	-- Function to make
						-- default method
	-> [Int]				-- Tags for methods required
	-> [Id]					-- Method names to bind, in tag order
	-> NF_TcM TypecheckedMonoBinds

	
makeDefaultMethods mk_method_expr [] method_ids
  = returnNF_Tc EmptyMonoBinds

makeDefaultMethods mk_method_expr (tag:tags) method_ids
  = mk_method_expr tag				      `thenNF_Tc` \ rhs ->
    makeDefaultMethods mk_method_expr tags method_ids `thenNF_Tc` \ meth_binds ->

    returnNF_Tc ((VarMonoBind method_id rhs) `AndMonoBinds` meth_binds)
  where
    method_id = method_ids !! (tag-1)
\end{code}

%************************************************************************
%*									*
\subsection{Type-checking specialise instance pragmas}
%*									*
%************************************************************************

\begin{code}
tcSpecInstSigs :: E -> CE -> TCE
	       -> Bag InstInfo				-- inst decls seen (declared and derived)
	       -> [RenamedSpecialisedInstanceSig]	-- specialise instance upragmas
	       -> TcM (Bag InstInfo)			-- new, overlapped, inst decls

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
	      -> RenamedSpecialisedInstanceSig
	      -> NF_TcM (Bag InstInfo)

tcSpecInstSig e ce tce inst_infos inst_mapper (InstSpecSig class_name ty src_loc)
  = recoverTc emptyBag			(
    addSrcLocTc src_loc			(
    let
	clas = lookupCE ce class_name -- Renamer ensures this can't fail

	-- Make some new type variables, named as in the specialised instance type
	ty_names 			  = extractMonoTyNames (==) ty
	(tmpl_e,inst_tmpls,inst_tmpl_tys) = mkTVE ty_names
    in
    babyTcMtoTcM (tcInstanceType ce tce tmpl_e True src_loc ty)
				`thenTc` \ inst_ty ->
    let
	maybe_tycon = case getUniDataTyCon_maybe inst_ty of 
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
			  pprParendUniType PprDebug inst_ty],
		   ppCat [ppStr "        derived from:",
			  if null unspec_theta then ppNil else ppr PprDebug unspec_theta,
			  if null unspec_theta then ppNil else ppStr "=>",
			  ppr PprDebug clas,
			  pprParendUniType PprDebug unspec_inst_ty]])
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

    match_tycon tycon inst_ty = case (getUniDataTyCon_maybe inst_ty) of
	  Just (inst_tc,_,_) -> tycon == inst_tc
	  Nothing            -> False

    match_fun inst_ty = isFunType inst_ty


is_plain_instance inst_ty
  = case (getUniDataTyCon_maybe inst_ty) of
      Just (_,tys,_) -> all isTyVarTemplateTy tys
      Nothing	     -> case maybeUnpackFunTy inst_ty of
			  Just (arg, res) -> isTyVarTemplateTy arg && isTyVarTemplateTy res
			  Nothing	  -> error "TcInstDecls:is_plain_instance"
\end{code}
