%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcInstDecls]{Typechecking instance declarations}

\begin{code}
module TcInstDcls ( tcInstDecls1, tcInstDecls2 ) where

#include "HsVersions.h"

import HsSyn
import TcBinds		( tcSpecSigs )
import TcClassDcl	( tcMethodBind, mkMethodBind, badMethodErr, 
			  tcClassDecl2, getGenericInstances )
import TcRnMonad       
import TcMType		( tcSkolSigType, checkValidTheta, checkValidInstHead, instTypeErr, 
			  checkAmbiguity, SourceTyCtxt(..) )
import TcType		( mkClassPred, tyVarsOfType, 
			  tcSplitSigmaTy, tcSplitDFunHead, mkTyVarTys,
			  SkolemInfo(InstSkol), tcSplitDFunTy, pprClassPred )
import Inst		( tcInstClassOp, newDicts, instToId, showLIE, 
			  getOverlapFlag, tcExtendLocalInstEnv )
import InstEnv		( mkLocalInstance, instanceDFunId )
import TcDeriv		( tcDeriving )
import TcEnv		( tcExtendGlobalValEnv, tcExtendTyVarEnv,
 			  InstInfo(..), InstBindings(..), 
			  newDFunName, tcExtendIdEnv
			)
import TcHsType		( kcHsSigType, tcHsKindedType )
import TcUnify		( checkSigTyVars )
import TcSimplify	( tcSimplifyCheck, tcSimplifySuperClasses )
import Type		( zipOpenTvSubst, substTheta, substTys )
import DataCon		( classDataCon )
import Class		( classBigSig )
import Var		( Id, idName, idType )
import MkId		( mkDictFunId, rUNTIME_ERROR_ID )
import FunDeps		( checkInstFDs )
import Name		( Name, getSrcLoc )
import NameSet		( unitNameSet, emptyNameSet )
import UnicodeUtil	( stringToUtf8 )
import Maybe		( catMaybes )
import SrcLoc		( srcLocSpan, unLoc, noLoc, Located(..), srcSpanStart )
import ListSetOps	( minusList )
import Outputable
import Bag
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
   :: [LTyClDecl Name]		-- For deriving stuff
   -> [LInstDecl Name]		-- Source code instance decls
   -> TcM (TcGblEnv,		-- The full inst env
	   [InstInfo],		-- Source-code instance decls to process; 
				-- contains all dfuns for this module
	   [HsBindGroup Name])	-- Supporting bindings for derived instances

tcInstDecls1 tycl_decls inst_decls
  = checkNoErrs $
	-- Stop if addInstInfos etc discovers any errors
	-- (they recover, so that we get more than one error each round)

   	-- (1) Do the ordinary instance declarations
    mappM tcLocalInstDecl1 inst_decls    `thenM` \ local_inst_infos ->

    let
	local_inst_info = catMaybes local_inst_infos
	clas_decls	= filter (isClassDecl.unLoc) tycl_decls
    in
	-- (2) Instances from generic class declarations
    getGenericInstances clas_decls	`thenM` \ generic_inst_info -> 

	-- Next, construct the instance environment so far, consisting of
	--	a) local instance decls
	--	b) generic instances
    addInsts local_inst_info	$
    addInsts generic_inst_info	$

	-- (3) Compute instances from "deriving" clauses; 
	-- This stuff computes a context for the derived instance decl, so it
	-- needs to know about all the instances possible; hence inst_env4
    tcDeriving tycl_decls	`thenM` \ (deriv_inst_info, deriv_binds) ->
    addInsts deriv_inst_info	$

    getGblEnv			`thenM` \ gbl_env ->
    returnM (gbl_env, 
	     generic_inst_info ++ deriv_inst_info ++ local_inst_info,
	     deriv_binds)

addInsts :: [InstInfo] -> TcM a -> TcM a
addInsts infos thing_inside
  = tcExtendLocalInstEnv (map iSpec infos) thing_inside
\end{code} 

\begin{code}
tcLocalInstDecl1 :: LInstDecl Name 
		 -> TcM (Maybe InstInfo)	-- Nothing if there was an error
	-- A source-file instance declaration
	-- Type-check all the stuff before the "where"
	--
	-- We check for respectable instance type, and context
	-- but only do this for non-imported instance decls.
	-- Imported ones should have been checked already, and may indeed
	-- contain something illegal in normal Haskell, notably
	--	instance CCallable [Char] 
tcLocalInstDecl1 decl@(L loc (InstDecl poly_ty binds uprags))
  =	-- Prime error recovery, set source location
    recoverM (returnM Nothing)		$
    setSrcSpan loc			$
    addErrCtxt (instDeclCtxt1 poly_ty)	$

	-- Typecheck the instance type itself.  We can't use 
	-- tcHsSigType, because it's not a valid user type.
    kcHsSigType poly_ty			`thenM` \ kinded_ty ->
    tcHsKindedType kinded_ty		`thenM` \ poly_ty' ->
    let
	(tyvars, theta, tau) = tcSplitSigmaTy poly_ty'
    in
    checkValidTheta InstThetaCtxt theta			`thenM_`
    checkAmbiguity tyvars theta (tyVarsOfType tau)	`thenM_`
    checkValidInstHead tau				`thenM` \ (clas,inst_tys) ->
    checkTc (checkInstFDs theta clas inst_tys)
	    (instTypeErr (pprClassPred clas inst_tys) msg)	`thenM_`
    newDFunName clas inst_tys (srcSpanStart loc)		`thenM` \ dfun_name ->
    getOverlapFlag						`thenM` \ overlap_flag ->
    let dfun  = mkDictFunId dfun_name tyvars theta clas inst_tys
	ispec = mkLocalInstance dfun overlap_flag
    in
    returnM (Just (InstInfo { iSpec = ispec, iBinds = VanillaInst binds uprags }))
  where
    msg  = parens (ptext SLIT("the instance types do not agree with the functional dependencies of the class"))
\end{code}


%************************************************************************
%*									*
\subsection{Type-checking instance declarations, pass 2}
%*									*
%************************************************************************

\begin{code}
tcInstDecls2 :: [LTyClDecl Name] -> [InstInfo] 
	     -> TcM (TcLclEnv, LHsBinds Id)
-- (a) From each class declaration, 
--	generate any default-method bindings
-- (b) From each instance decl
--	generate the dfun binding

tcInstDecls2 tycl_decls inst_decls
  = do	{	-- (a) Default methods from class decls
	  (dm_binds_s, dm_ids_s) <- mapAndUnzipM tcClassDecl2 $
			            filter (isClassDecl.unLoc) tycl_decls
	; tcExtendIdEnv (concat dm_ids_s) 	$ do 
    
	 	-- (b) instance declarations
	; inst_binds_s <- mappM tcInstDecl2 inst_decls

		-- Done
	; tcl_env <- getLclEnv
	; returnM (tcl_env, unionManyBags dm_binds_s	`unionBags`
			    unionManyBags inst_binds_s) }
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
tcInstDecl2 :: InstInfo -> TcM (LHsBinds Id)

tcInstDecl2 (InstInfo { iSpec = ispec, iBinds = binds })
  = let 
	dfun_id    = instanceDFunId ispec
	rigid_info = InstSkol dfun_id
	inst_ty    = idType dfun_id
    in
	 -- Prime error recovery
    recoverM (returnM emptyLHsBinds)		$
    setSrcSpan (srcLocSpan (getSrcLoc dfun_id))	$
    addErrCtxt (instDeclCtxt2 (idType dfun_id))	$

	-- Instantiate the instance decl with skolem constants 
    tcSkolSigType rigid_info inst_ty	`thenM` \ (inst_tyvars', dfun_theta', inst_head') ->
		-- These inst_tyvars' scope over the 'where' part
		-- Those tyvars are inside the dfun_id's type, which is a bit
		-- bizarre, but OK so long as you realise it!
    let
	(clas, inst_tys') = tcSplitDFunHead inst_head'
        (class_tyvars, sc_theta, _, op_items) = classBigSig clas

        -- Instantiate the super-class context with inst_tys
	sc_theta' = substTheta (zipOpenTvSubst class_tyvars inst_tys') sc_theta
	origin	  = SigOrigin rigid_info
    in
	 -- Create dictionary Ids from the specified instance contexts.
    newDicts InstScOrigin sc_theta'			`thenM` \ sc_dicts ->
    newDicts origin dfun_theta'				`thenM` \ dfun_arg_dicts ->
    newDicts origin [mkClassPred clas inst_tys'] 	`thenM` \ [this_dict] ->
		-- Default-method Ids may be mentioned in synthesised RHSs,
		-- but they'll already be in the environment.

	-- Typecheck the methods
    let		-- These insts are in scope; quite a few, eh?
	avail_insts = [this_dict] ++ dfun_arg_dicts ++ sc_dicts
    in
    tcMethods origin clas inst_tyvars' 
	      dfun_theta' inst_tys' avail_insts 
	      op_items binds		`thenM` \ (meth_ids, meth_binds) ->

	-- Figure out bindings for the superclass context
	-- Don't include this_dict in the 'givens', else
	-- sc_dicts get bound by just selecting  from this_dict!!
    addErrCtxt superClassCtxt
	(tcSimplifySuperClasses inst_tyvars'
			 dfun_arg_dicts
			 sc_dicts)	`thenM` \ sc_binds ->

	-- It's possible that the superclass stuff might unified one
	-- of the inst_tyavars' with something in the envt
    checkSigTyVars inst_tyvars' 	`thenM_`

	-- Deal with 'SPECIALISE instance' pragmas by making them
	-- look like SPECIALISE pragmas for the dfun
    let
	uprags = case binds of
		       VanillaInst _ uprags -> uprags
		       other		    -> []
	spec_prags = [ L loc (SpecSig (L loc (idName dfun_id)) ty)
		     | L loc (SpecInstSig ty) <- uprags ]
    in
    tcExtendGlobalValEnv [dfun_id] (
	tcExtendTyVarEnv inst_tyvars'	$
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
	    nlHsApp (noLoc $ TyApp (nlHsVar rUNTIME_ERROR_ID) 
				   [idType this_dict_id])
		  (nlHsLit (HsStringPrim (mkFastString (stringToUtf8 msg))))

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

	dict_bind  = noLoc (VarBind this_dict_id dict_rhs)
	all_binds  = dict_bind `consBag` (sc_binds `unionBags` meth_binds)

	main_bind = noLoc $ AbsBinds
		 	    inst_tyvars'
		 	    (map instToId dfun_arg_dicts)
		 	    [(inst_tyvars', dfun_id, this_dict_id)] 
		 	    inlines all_binds
    in
    showLIE (text "instance") 		`thenM_`
    returnM (unitBag main_bind `unionBags` 
	     prag_binds )


tcMethods origin clas inst_tyvars' dfun_theta' inst_tys' 
	  avail_insts op_items (VanillaInst monobinds uprags)
  = 	-- Check that all the method bindings come from this class
    let
	sel_names = [idName sel_id | (sel_id, _) <- op_items]
	bad_bndrs = collectHsBindBinders monobinds `minusList` sel_names
    in
    mappM (addErrTc . badMethodErr clas) bad_bndrs	`thenM_`

	-- Make the method bindings
    let
	mk_method_bind = mkMethodBind origin clas inst_tys' monobinds
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
	tc_method_bind = tcMethodBind inst_tyvars' dfun_theta' all_insts uprags 
	meth_ids       = [meth_id | (_,meth_id,_) <- meth_infos]
    in

    mapM tc_method_bind meth_infos		`thenM` \ meth_binds_s ->
   
    returnM (meth_ids, unionManyBags meth_binds_s)


-- Derived newtype instances
tcMethods origin clas inst_tyvars' dfun_theta' inst_tys' 
	  avail_insts op_items (NewTypeDerived rep_tys)
  = getInstLoc origin				`thenM` \ inst_loc ->
    mapAndUnzip3M (do_one inst_loc) op_items	`thenM` \ (meth_ids, meth_binds, rhs_insts) ->
    
    tcSimplifyCheck
	 (ptext SLIT("newtype derived instance"))
	 inst_tyvars' avail_insts rhs_insts	`thenM` \ lie_binds ->

	-- I don't think we have to do the checkSigTyVars thing

    returnM (meth_ids, lie_binds `unionBags` listToBag meth_binds)

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
	  return (meth_id, noLoc (VarBind meth_id (nlHsVar (instToId rhs_inst))), rhs_inst)

	-- Instantiate rep_tys with the relevant type variables
	-- This looks a bit odd, because inst_tyvars' are the skolemised version
	-- of the type variables in the instance declaration; but rep_tys doesn't
	-- have the skolemised version, so we substitute them in here
    rep_tys' = substTys subst rep_tys
    subst    = zipOpenTvSubst inst_tyvars' (mkTyVarTys inst_tyvars')
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
instDeclCtxt1 hs_inst_ty 
  = inst_decl_ctxt (case unLoc hs_inst_ty of
			HsForAllTy _ _ _ (L _ (HsPredTy pred)) -> ppr pred
			HsPredTy pred	                 -> ppr pred
			other			         -> ppr hs_inst_ty)	-- Don't expect this
instDeclCtxt2 dfun_ty
  = inst_decl_ctxt (ppr (mkClassPred cls tys))
  where
    (_,_,cls,tys) = tcSplitDFunTy dfun_ty

inst_decl_ctxt doc = ptext SLIT("In the instance declaration for") <+> quotes doc

superClassCtxt = ptext SLIT("When checking the super-classes of an instance declaration")
\end{code}
