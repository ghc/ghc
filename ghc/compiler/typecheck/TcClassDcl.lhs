%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcClassDcl]{Typechecking class declarations}

\begin{code}
module TcClassDcl ( tcClassDecl1, tcClassDecls2, 
		    MethodSpec, tcMethodBind, mkMethodBind, badMethodErr
		  ) where

#include "HsVersions.h"

import HsSyn		( TyClDecl(..), Sig(..), MonoBinds(..),
			  HsExpr(..), HsLit(..), Pat(WildPat),
			  mkSimpleMatch, andMonoBinds, andMonoBindList, 
			  isClassOpSig, isPragSig, 
			  placeHolderType
			)
import BasicTypes	( RecFlag(..) )
import RnHsSyn		( RenamedTyClDecl, RenamedSig,
			  RenamedClassOpSig, RenamedMonoBinds,
			  maybeGenericMatch
			)
import RnEnv		( lookupSysName )
import TcHsSyn		( TcMonoBinds )

import Inst		( Inst, InstOrigin(..), instToId, newDicts, newMethod )
import TcEnv		( TyThingDetails(..), 
			  tcLookupClass, tcExtendLocalValEnv2,
			  tcExtendTyVarEnv2, tcExtendTyVarEnv
			)
import TcTyDecls	( tcMkDataCon )
import TcBinds		( tcMonoBinds, tcSpecSigs )
import TcMonoType	( TcSigInfo(..), tcHsType, tcHsTheta, mkTcSig )
import TcSimplify	( tcSimplifyCheck, bindInstsOfLocalFuns )
import TcUnify		( checkSigTyVars, sigCtxt )
import TcMType		( tcInstTyVars )
import TcType		( Type, TyVarDetails(..), TcType, TcThetaType, TcTyVar, 
			  mkTyVarTys, mkPredTys, mkClassPred, tcSplitSigmaTy, tcSplitFunTys,
			  tcIsTyVarTy, tcSplitTyConApp_maybe, tcSplitForAllTys, tcSplitPhiTy,
			  getClassPredTys_maybe, mkPhiTy
			)
import TcRnMonad
import Generics		( mkGenericRhs )
import PrelInfo		( nO_METHOD_BINDING_ERROR_ID )
import Class		( classTyVars, classBigSig, classTyCon, 
			  Class, ClassOpItem, DefMeth (..) )
import TyCon		( tyConGenInfo )
import Subst		( substTyWith )
import MkId		( mkDictSelId, mkDefaultMethodId )
import Id		( Id, idType, idName, mkUserLocal, setInlinePragma )
import Name		( Name, NamedThing(..) )
import NameEnv		( NameEnv, lookupNameEnv, emptyNameEnv, unitNameEnv, plusNameEnv )
import NameSet		( emptyNameSet, unitNameSet )
import OccName		( mkClassTyConOcc, mkClassDataConOcc, mkSuperDictSelOcc, reportIfUnused )
import Outputable
import Var		( TyVar )
import CmdLineOpts
import UnicodeUtil	( stringToUtf8 )
import ErrUtils		( dumpIfSet )
import Util		( count, lengthIs, isSingleton )
import Maybes		( seqMaybe )
import Maybe		( isJust )
import FastString
\end{code}



Dictionary handling
~~~~~~~~~~~~~~~~~~~
Every class implicitly declares a new data type, corresponding to dictionaries
of that class. So, for example:

	class (D a) => C a where
	  op1 :: a -> a
	  op2 :: forall b. Ord b => a -> b -> b

would implicitly declare

	data CDict a = CDict (D a)	
			     (a -> a)
			     (forall b. Ord b => a -> b -> b)

(We could use a record decl, but that means changing more of the existing apparatus.
One step at at time!)

For classes with just one superclass+method, we use a newtype decl instead:

	class C a where
	  op :: forallb. a -> b -> b

generates

	newtype CDict a = CDict (forall b. a -> b -> b)

Now DictTy in Type is just a form of type synomym: 
	DictTy c t = TyConTy CDict `AppTy` t

Death to "ExpandingDicts".


%************************************************************************
%*									*
\subsection{Type checking}
%*									*
%************************************************************************

\begin{code}

tcClassDecl1 :: RenamedTyClDecl -> TcM (Name, TyThingDetails)
tcClassDecl1 (ClassDecl {tcdCtxt = context, tcdName = class_name,
			 tcdTyVars = tyvar_names, tcdFDs = fundeps,
			 tcdSigs = class_sigs, tcdMeths = def_methods,
			 tcdLoc = src_loc})
  = 	-- LOOK THINGS UP IN THE ENVIRONMENT
    tcLookupClass class_name				`thenM` \ clas ->
    let
	tyvars     = classTyVars clas
	op_sigs    = filter isClassOpSig class_sigs
	op_names   = [n | ClassOpSig n _ _ _ <- op_sigs]
    in
    tcExtendTyVarEnv tyvars				$ 

    checkDefaultBinds clas op_names def_methods		`thenM` \ mb_dm_env ->
	
	-- CHECK THE CONTEXT
	-- The renamer has already checked that the context mentions
	-- only the type variable of the class decl.
	-- Context is already kind-checked
    tcHsTheta context					`thenM` \ sc_theta ->

	-- CHECK THE CLASS SIGNATURES,
    mappM (tcClassSig clas tyvars mb_dm_env) op_sigs	`thenM` \ sig_stuff ->

	-- MAKE THE CLASS DETAILS
    lookupSysName class_name mkClassTyConOcc 		`thenM` \ tycon_name ->
    lookupSysName class_name mkClassDataConOcc	 	`thenM` \ datacon_name ->
    mapM (lookupSysName class_name . mkSuperDictSelOcc) 
	 [1..length context]				`thenM` \ sc_sel_names ->
      -- We number off the superclass selectors, 1, 2, 3 etc so that we 
      -- can construct names for the selectors.  Thus
      --      class (C a, C b) => D a b where ...
      -- gives superclass selectors
      --      D_sc1, D_sc2
      -- (We used to call them D_C, but now we can have two different
      --  superclasses both called C!)
    let
	(op_tys, op_items) = unzip sig_stuff
        sc_tys		   = mkPredTys sc_theta
	dict_component_tys = sc_tys ++ op_tys
        sc_sel_ids	   = [mkDictSelId sc_name clas | sc_name <- sc_sel_names]
    in
    tcMkDataCon datacon_name
		[{- No strictness -}]
		[{- No labelled fields -}]
		tyvars [{-No context-}]
		[{-No existential tyvars-}] [{-Or context-}]
		dict_component_tys
		(classTyCon clas)			`thenM` \ dict_con ->

    returnM (class_name, ClassDetails sc_theta sc_sel_ids op_items dict_con tycon_name)
\end{code}

\begin{code}
checkDefaultBinds :: Class -> [Name] -> Maybe RenamedMonoBinds
		  -> TcM (Maybe (NameEnv Bool))
	-- The returned environment says
	--	x not in env => no default method
	--	x -> True    => generic default method
	--	x -> False   => polymorphic default method

  -- Check default bindings
  -- 	a) must be for a class op for this class
  --	b) must be all generic or all non-generic
  -- and return a mapping from class-op to DefMeth info

  -- But do all this only for source binds

checkDefaultBinds clas ops Nothing
  = returnM Nothing

checkDefaultBinds clas ops (Just mbs)
  = go mbs	`thenM` \ dm_env ->
    returnM (Just dm_env)
  where
    go EmptyMonoBinds = returnM emptyNameEnv

    go (AndMonoBinds b1 b2)
      = go b1	`thenM` \ dm_info1 ->
        go b2	`thenM` \ dm_info2 ->
        returnM (dm_info1 `plusNameEnv` dm_info2)

    go (FunMonoBind op _ matches loc)
      = addSrcLoc loc					$

  	-- Check that the op is from this class
	checkTc (op `elem` ops) (badMethodErr clas op)		`thenM_`

   	-- Check that all the defns ar generic, or none are
	checkTc (all_generic || none_generic) (mixedGenericErr op)	`thenM_`

	returnM (unitNameEnv op all_generic)
      where
	n_generic    = count (isJust . maybeGenericMatch) matches
	none_generic = n_generic == 0
	all_generic  = matches `lengthIs` n_generic
\end{code}


\begin{code}
tcClassSig :: Class	    		-- ...ditto...
	   -> [TyVar]		 	-- The class type variable, used for error check only
	   -> Maybe (NameEnv Bool)	-- Info about default methods; 
					--	Nothing => imported class defn with no method binds
	   -> RenamedClassOpSig
	   -> TcM (Type,		-- Type of the method
		     ClassOpItem)	-- Selector Id, default-method Id, True if explicit default binding

-- This warrants an explanation: we need to separate generic
-- default methods and default methods later on in the compiler
-- so we distinguish them in checkDefaultBinds, and pass this knowledge in the
-- Class.DefMeth data structure. 

tcClassSig clas clas_tyvars maybe_dm_env
	   (ClassOpSig op_name sig_dm op_ty src_loc)
  = addSrcLoc src_loc $

	-- Check the type signature.  NB that the envt *already has*
	-- bindings for the type variables; see comments in TcTyAndClassDcls.
    tcHsType op_ty			`thenM` \ local_ty ->

    let
	theta = [mkClassPred clas (mkTyVarTys clas_tyvars)]

	-- Build the selector id and default method id
	sel_id = mkDictSelId op_name clas
	DefMeth dm_name = sig_dm

	dm_info = case maybe_dm_env of
		    Nothing     -> sig_dm
		    Just dm_env -> mk_src_dm_info dm_env

	mk_src_dm_info dm_env = case lookupNameEnv dm_env op_name of
				   Nothing    -> NoDefMeth
				   Just True  -> GenDefMeth
				   Just False -> DefMeth dm_name
    in
    returnM (local_ty, (sel_id, dm_info))
\end{code}


%************************************************************************
%*									*
\subsection[Default methods]{Default methods}
%*									*
%************************************************************************

The default methods for a class are each passed a dictionary for the
class, so that they get access to the other methods at the same type.
So, given the class decl
\begin{verbatim}
class Foo a where
	op1 :: a -> Bool
	op2 :: Ord b => a -> b -> b -> b

	op1 x = True
	op2 x y z = if (op1 x) && (y < z) then y else z
\end{verbatim}
we get the default methods:
\begin{verbatim}
defm.Foo.op1 :: forall a. Foo a => a -> Bool
defm.Foo.op1 = /\a -> \dfoo -> \x -> True

defm.Foo.op2 :: forall a. Foo a => forall b. Ord b => a -> b -> b -> b
defm.Foo.op2 = /\ a -> \ dfoo -> /\ b -> \ dord -> \x y z ->
		  if (op1 a dfoo x) && (< b dord y z) then y else z
\end{verbatim}

When we come across an instance decl, we may need to use the default
methods:
\begin{verbatim}
instance Foo Int where {}
\end{verbatim}
gives
\begin{verbatim}
const.Foo.Int.op1 :: Int -> Bool
const.Foo.Int.op1 = defm.Foo.op1 Int dfun.Foo.Int

const.Foo.Int.op2 :: forall b. Ord b => Int -> b -> b -> b
const.Foo.Int.op2 = defm.Foo.op2 Int dfun.Foo.Int

dfun.Foo.Int :: Foo Int
dfun.Foo.Int = (const.Foo.Int.op1, const.Foo.Int.op2)
\end{verbatim}
Notice that, as with method selectors above, we assume that dictionary
application is curried, so there's no need to mention the Ord dictionary
in const.Foo.Int.op2 (or the type variable).

\begin{verbatim}
instance Foo a => Foo [a] where {}

dfun.Foo.List :: forall a. Foo a -> Foo [a]
dfun.Foo.List
  = /\ a -> \ dfoo_a ->
    let rec
	op1 = defm.Foo.op1 [a] dfoo_list
	op2 = defm.Foo.op2 [a] dfoo_list
	dfoo_list = (op1, op2)
    in
	dfoo_list
\end{verbatim}

The function @tcClassDecls2@ just arranges to apply @tcClassDecl2@ to
each local class decl.

\begin{code}
tcClassDecls2 :: [RenamedTyClDecl] -> TcM (TcMonoBinds, [Id])

tcClassDecls2 decls
  = foldr combine
	  (returnM (EmptyMonoBinds, []))
	  [tcClassDecl2 cls_decl | cls_decl@(ClassDecl {tcdMeths = Just _}) <- decls] 
		-- The 'Just' picks out source ClassDecls
  where
    combine tc1 tc2 = tc1 `thenM` \ (binds1, ids1) ->
		      tc2 `thenM` \ (binds2, ids2) ->
		      returnM (binds1 `AndMonoBinds` binds2,
			       ids1 ++ ids2)
\end{code}

@tcClassDecl2@ generates bindings for polymorphic default methods
(generic default methods have by now turned into instance declarations)

\begin{code}
tcClassDecl2 :: RenamedTyClDecl		-- The class declaration
	     -> TcM (TcMonoBinds, [Id])

tcClassDecl2 (ClassDecl {tcdName = class_name, tcdSigs = sigs, 
			 tcdMeths = Just default_binds, tcdLoc = src_loc})
  = 	-- The 'Just' picks out source ClassDecls
    recoverM (returnM (EmptyMonoBinds, []))	$ 
    addSrcLoc src_loc		   			$
    tcLookupClass class_name				`thenM` \ clas ->

	-- We make a separate binding for each default method.
	-- At one time I used a single AbsBinds for all of them, thus
	-- AbsBind [d] [dm1, dm2, dm3] { dm1 = ...; dm2 = ...; dm3 = ... }
	-- But that desugars into
	--	ds = \d -> (..., ..., ...)
	--	dm1 = \d -> case ds d of (a,b,c) -> a
	-- And since ds is big, it doesn't get inlined, so we don't get good
	-- default methods.  Better to make separate AbsBinds for each
    let
	(tyvars, _, _, op_items) = classBigSig clas
	prags 			 = filter isPragSig sigs
	tc_dm			 = tcDefMeth clas tyvars default_binds prags
    in
    mapAndUnzipM tc_dm op_items	`thenM` \ (defm_binds, dm_ids_s) ->

    returnM (andMonoBindList defm_binds, concat dm_ids_s)
    

tcDefMeth clas tyvars binds_in prags (_, NoDefMeth)  = returnM (EmptyMonoBinds, [])
tcDefMeth clas tyvars binds_in prags (_, GenDefMeth) = returnM (EmptyMonoBinds, [])
	-- Generate code for polymorphic default methods only
	-- (Generic default methods have turned into instance decls by now.)
	-- This is incompatible with Hugs, which expects a polymorphic 
	-- default method for every class op, regardless of whether or not 
	-- the programmer supplied an explicit default decl for the class.  
	-- (If necessary we can fix that, but we don't have a convenient Id to hand.)

tcDefMeth clas tyvars binds_in prags op_item@(sel_id, DefMeth dm_name)
  = tcInstTyVars ClsTv tyvars		`thenM` \ (clas_tyvars, inst_tys, _) ->
    let
	dm_ty       = idType sel_id	-- Same as dict selector!
        theta       = [mkClassPred clas inst_tys]
	local_dm_id = mkDefaultMethodId dm_name dm_ty
	xtve 	    = tyvars `zip` clas_tyvars
    in
    newDicts origin theta 				`thenM` \ [this_dict] ->

    mkMethodBind origin clas inst_tys binds_in op_item	`thenM` \ (_, meth_info) ->
    getLIE (tcMethodBind xtve clas_tyvars theta 
			 [this_dict] prags meth_info)	`thenM` \ (defm_bind, insts_needed) ->
    
    addErrCtxt (defltMethCtxt clas) $
    
        -- Check the context
    tcSimplifyCheck
        (ptext SLIT("class") <+> ppr clas)
	clas_tyvars
        [this_dict]
        insts_needed			`thenM` \ dict_binds ->

	-- Simplification can do unification
    checkSigTyVars clas_tyvars		`thenM` \ clas_tyvars' ->
    
    let
	(_,dm_inst_id,_) = meth_info
        full_bind = AbsBinds
    		    clas_tyvars'
    		    [instToId this_dict]
    		    [(clas_tyvars', local_dm_id, dm_inst_id)]
    		    emptyNameSet	-- No inlines (yet)
    		    (dict_binds `andMonoBinds` defm_bind)
    in
    returnM (full_bind, [local_dm_id])
  where
    origin = ClassDeclOrigin
\end{code}

    

%************************************************************************
%*									*
\subsection{Typechecking a method}
%*									*
%************************************************************************

@tcMethodBind@ is used to type-check both default-method and
instance-decl method declarations.  We must type-check methods one at a
time, because their signatures may have different contexts and
tyvar sets.

\begin{code}
type MethodSpec = (Id, 			-- Global selector Id
		   Id, 			-- Local Id (class tyvars instantiated)
		   RenamedMonoBinds)	-- Binding for the method

tcMethodBind 
	:: [(TyVar,TcTyVar)]	-- Bindings for type environment
	-> [TcTyVar]		-- Instantiated type variables for the
				--  	enclosing class/instance decl. 
				--  	They'll be signature tyvars, and we
				--  	want to check that they don't get bound
				-- Always equal the range of the type envt
	-> TcThetaType		-- Available theta; it's just used for the error message
	-> [Inst]		-- Available from context, used to simplify constraints 
				-- 	from the method body
	-> [RenamedSig]		-- Pragmas (e.g. inline pragmas)
	-> MethodSpec		-- Details of this method
	-> TcM TcMonoBinds

tcMethodBind xtve inst_tyvars inst_theta avail_insts prags
	     (sel_id, meth_id, meth_bind)
  =  	-- Check the bindings; first adding inst_tyvars to the envt
	-- so that we don't quantify over them in nested places
     mkTcSig meth_id 				`thenM` \ meth_sig ->

     tcExtendTyVarEnv2 xtve (
	addErrCtxt (methodCtxt sel_id)			$
	getLIE 						$
	tcMonoBinds meth_bind [meth_sig] NonRecursive
     )							`thenM` \ ((meth_bind,_), meth_lie) ->

	-- Now do context reduction.   We simplify wrt both the local tyvars
	-- and the ones of the class/instance decl, so that there is
	-- no problem with
	--	class C a where
	--	  op :: Eq a => a -> b -> a
	--
	-- We do this for each method independently to localise error messages

     let
	TySigInfo meth_id meth_tvs meth_theta _ local_meth_id _ _ = meth_sig
     in
     addErrCtxtM (sigCtxt sel_id inst_tyvars inst_theta (idType meth_id))	$
     newDicts SignatureOrigin meth_theta	`thenM` \ meth_dicts ->
     let
	all_tyvars = meth_tvs ++ inst_tyvars
	all_insts  = avail_insts ++ meth_dicts
     in
     tcSimplifyCheck
	 (ptext SLIT("class or instance method") <+> quotes (ppr sel_id))
	 all_tyvars all_insts meth_lie		`thenM` \ lie_binds ->

     checkSigTyVars all_tyvars			`thenM` \ all_tyvars' ->

     let
	sel_name = idName sel_id
	inline_prags  = [ (is_inl, phase)
		        | InlineSig is_inl name phase _ <- prags, 
		          name == sel_name ]
	spec_prags = [ prag 
		     | prag@(SpecSig name _ _) <- prags, 
		       name == sel_name]
	
		-- Attach inline pragmas as appropriate
	(final_meth_id, inlines) 
	   | ((is_inline, phase) : _) <- inline_prags
	   = (meth_id `setInlinePragma` phase,
	      if is_inline then unitNameSet (idName meth_id) else emptyNameSet)
	   | otherwise
	   = (meth_id, emptyNameSet)

	meth_tvs'      = take (length meth_tvs) all_tyvars'
	poly_meth_bind = AbsBinds meth_tvs'
				  (map instToId meth_dicts)
     				  [(meth_tvs', final_meth_id, local_meth_id)]
				  inlines
				  (lie_binds `andMonoBinds` meth_bind)

     in
	-- Deal with specialisation pragmas
	-- The sel_name is what appears in the pragma
     tcExtendLocalValEnv2 [(sel_name, final_meth_id)] (
	getLIE (tcSpecSigs spec_prags)			`thenM` \ (spec_binds1, prag_lie) ->
     
	     -- The prag_lie for a SPECIALISE pragma will mention the function itself, 
	     -- so we have to simplify them away right now lest they float outwards!
	bindInstsOfLocalFuns prag_lie [final_meth_id]	`thenM` \ spec_binds2 ->
	returnM (spec_binds1 `andMonoBinds` spec_binds2)
     )							`thenM` \ spec_binds ->

     returnM (poly_meth_bind `andMonoBinds` spec_binds)


mkMethodBind :: InstOrigin
	     -> Class -> [TcType]	-- Class and instance types
	     -> RenamedMonoBinds	-- Method binding (pick the right one from in here)
	     -> ClassOpItem
	     -> TcM (Maybe Inst,		-- Method inst
		     MethodSpec)
-- Find the binding for the specified method, or make
-- up a suitable default method if it isn't there

mkMethodBind origin clas inst_tys meth_binds (sel_id, dm_info)
  = mkMethId origin clas sel_id inst_tys		`thenM` \ (mb_inst, meth_id) ->
    let
	meth_name  = idName meth_id
    in
	-- Figure out what method binding to use
	-- If the user suppplied one, use it, else construct a default one
    getSrcLocM					`thenM` \ loc -> 
    (case find_bind (idName sel_id) meth_name meth_binds of
	Just user_bind -> returnM user_bind 
	Nothing	       -> mkDefMethRhs origin clas inst_tys sel_id loc dm_info	`thenM` \ rhs ->
			  returnM (FunMonoBind meth_name False	-- Not infix decl
				               [mkSimpleMatch [] rhs placeHolderType loc] loc)
    )								`thenM` \ meth_bind ->

    returnM (mb_inst, (sel_id, meth_id, meth_bind))

mkMethId :: InstOrigin -> Class 
	 -> Id -> [TcType]	-- Selector, and instance types
	 -> TcM (Maybe Inst, Id)
	     
-- mkMethId instantiates the selector Id at the specified types
mkMethId origin clas sel_id inst_tys
  = let
	(tyvars,rho) = tcSplitForAllTys (idType sel_id)
	rho_ty	     = ASSERT( length tyvars == length inst_tys )
		       substTyWith tyvars inst_tys rho
	(preds,tau)  = tcSplitPhiTy rho_ty
        first_pred   = head preds
    in
	-- The first predicate should be of form (C a b)
	-- where C is the class in question
    ASSERT( not (null preds) && 
	    case getClassPredTys_maybe first_pred of
		{ Just (clas1,tys) -> clas == clas1 ; Nothing -> False }
    )
    if isSingleton preds then
	-- If it's the only one, make a 'method'
	getInstLoc origin				`thenM` \ inst_loc ->
    	newMethod inst_loc sel_id inst_tys preds tau	`thenM` \ meth_inst ->
	returnM (Just meth_inst, instToId meth_inst)
    else
	-- If it's not the only one we need to be careful
	-- For example, given 'op' defined thus:
	--	class Foo a where
	--	  op :: (?x :: String) => a -> a
	-- (mkMethId op T) should return an Inst with type
	--	(?x :: String) => T -> T
	-- That is, the class-op's context is still there.  
	-- BUT: it can't be a Method any more, because it breaks
	-- 	INVARIANT 2 of methods.  (See the data decl for Inst.)
	newUnique			`thenM` \ uniq ->
	getSrcLocM			`thenM` \ loc ->
	let 
	    real_tau = mkPhiTy (tail preds) tau
	    meth_id  = mkUserLocal (getOccName sel_id) uniq real_tau loc
	in
	returnM (Nothing, meth_id)

     -- The user didn't supply a method binding, 
     -- so we have to make up a default binding
     -- The RHS of a default method depends on the default-method info
mkDefMethRhs origin clas inst_tys sel_id loc (DefMeth dm_name)
  =  -- An polymorphic default method
    traceRn (text "mkDefMeth" <+> ppr dm_name) 	`thenM_`
    returnM (HsVar dm_name)

mkDefMethRhs origin clas inst_tys sel_id loc NoDefMeth
  =  	-- No default method
	-- Warn only if -fwarn-missing-methods
    doptM Opt_WarnMissingMethods 		`thenM` \ warn -> 
    warnTc (isInstDecl origin
	   && warn
	   && reportIfUnused (getOccName sel_id))
   	   (omittedMethodWarn sel_id)		`thenM_`
    returnM error_rhs
  where
    error_rhs  = HsLam (mkSimpleMatch wild_pats simple_rhs placeHolderType loc)
    simple_rhs = HsApp (HsVar (getName nO_METHOD_BINDING_ERROR_ID)) 
	    	       (HsLit (HsStringPrim (mkFastString (stringToUtf8 error_msg))))
    error_msg = showSDoc (hcat [ppr loc, text "|", ppr sel_id ])

	-- When the type is of form t1 -> t2 -> t3
	-- make a default method like (\ _ _ -> noMethBind "blah")
	-- rather than simply        (noMethBind "blah")
	-- Reason: if t1 or t2 are higher-ranked types we get n
	--	   silly ambiguity messages.
	-- Example:	f :: (forall a. Eq a => a -> a) -> Int
	--		f = error "urk"
	-- Here, tcSub tries to force (error "urk") to have the right type,
	-- thus:	f = \(x::forall a. Eq a => a->a) -> error "urk" (x t)
	-- where 't' is fresh ty var.  This leads directly to "ambiguous t".
	-- 
	-- NB: technically this changes the meaning of the default-default
	--     method slightly, because `seq` can see the lambdas.  Oh well.
    (_,_,tau1)    = tcSplitSigmaTy (idType sel_id)
    (_,_,tau2)    = tcSplitSigmaTy tau1
	-- Need two splits because the  selector can have a type like
	-- 	forall a. Foo a => forall b. Eq b => ...
    (arg_tys, _) = tcSplitFunTys tau2
    wild_pats	 = [WildPat placeHolderType | ty <- arg_tys]

mkDefMethRhs origin clas inst_tys sel_id loc GenDefMeth 
  =  	-- A generic default method
	-- If the method is defined generically, we can only do the job if the
	-- instance declaration is for a single-parameter type class with
	-- a type constructor applied to type arguments in the instance decl
	-- 	(checkTc, so False provokes the error)
     ASSERT( isInstDecl origin )	-- We never get here from a class decl

     checkTc (isJust maybe_tycon)
	     (badGenericInstance sel_id (notSimple inst_tys))		`thenM_`
     checkTc (isJust (tyConGenInfo tycon))
	     (badGenericInstance sel_id (notGeneric tycon))		`thenM_`

     ioToTcRn (dumpIfSet opt_PprStyle_Debug "Generic RHS" stuff)	`thenM_`
     returnM rhs
  where
    rhs = mkGenericRhs sel_id clas_tyvar tycon

    stuff = vcat [ppr clas <+> ppr inst_tys,
		  nest 4 (ppr sel_id <+> equals <+> ppr rhs)]

	  -- The tycon is only used in the generic case, and in that
	  -- case we require that the instance decl is for a single-parameter
	  -- type class with type variable arguments:
	  --	instance (...) => C (T a b)
    clas_tyvar    = head (classTyVars clas)
    Just tycon	  = maybe_tycon
    maybe_tycon   = case inst_tys of 
			[ty] -> case tcSplitTyConApp_maybe ty of
				  Just (tycon, arg_tys) | all tcIsTyVarTy arg_tys -> Just tycon
				  other						  -> Nothing
			other -> Nothing

isInstDecl InstanceDeclOrigin = True
isInstDecl ClassDeclOrigin    = False
\end{code}


\begin{code}
-- The renamer just puts the selector ID as the binder in the method binding
-- but we must use the method name; so we substitute it here.  Crude but simple.
find_bind sel_name meth_name (FunMonoBind op_name fix matches loc)
    | op_name == sel_name = Just (FunMonoBind meth_name fix matches loc)
find_bind sel_name meth_name (AndMonoBinds b1 b2)
    = find_bind sel_name meth_name b1 `seqMaybe` find_bind sel_name meth_name b2
find_bind sel_name meth_name other  = Nothing	-- Default case

 -- Find the prags for this method, and replace the
 -- selector name with the method name
find_prags sel_name meth_name [] = []
find_prags sel_name meth_name (SpecSig name ty loc : prags) 
     | name == sel_name = SpecSig meth_name ty loc : find_prags sel_name meth_name prags
find_prags sel_name meth_name (InlineSig sense name phase loc : prags)
   | name == sel_name = InlineSig sense meth_name phase loc : find_prags sel_name meth_name prags
find_prags sel_name meth_name (prag:prags) = find_prags sel_name meth_name prags
\end{code}


Contexts and errors
~~~~~~~~~~~~~~~~~~~
\begin{code}
defltMethCtxt clas
  = ptext SLIT("When checking the default methods for class") <+> quotes (ppr clas)

methodCtxt sel_id
  = ptext SLIT("In the definition for method") <+> quotes (ppr sel_id)

badMethodErr clas op
  = hsep [ptext SLIT("Class"), quotes (ppr clas), 
	  ptext SLIT("does not have a method"), quotes (ppr op)]

omittedMethodWarn sel_id
  = ptext SLIT("No explicit method nor default method for") <+> quotes (ppr sel_id)

badGenericInstance sel_id because
  = sep [ptext SLIT("Can't derive generic code for") <+> quotes (ppr sel_id),
	 because]

notSimple inst_tys
  = vcat [ptext SLIT("because the instance type(s)"), 
	  nest 2 (ppr inst_tys),
	  ptext SLIT("is not a simple type of form (T a b c)")]

notGeneric tycon
  = vcat [ptext SLIT("because the instance type constructor") <+> quotes (ppr tycon) <+> 
	  ptext SLIT("was not compiled with -fgenerics")]

mixedGenericErr op
  = ptext SLIT("Can't mix generic and non-generic equations for class method") <+> quotes (ppr op)
\end{code}
