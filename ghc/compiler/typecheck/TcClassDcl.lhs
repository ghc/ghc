%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcClassDcl]{Typechecking class declarations}

\begin{code}
module TcClassDcl ( tcClassDecl1, tcClassDecls2, mkImplicitClassBinds,
		    tcMethodBind, badMethodErr
		  ) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), TyClDecl(..), Sig(..), MonoBinds(..),
			  HsExpr(..), HsLit(..), HsType(..), HsPred(..),
			  mkSimpleMatch, andMonoBinds, andMonoBindList, 
			  isClassDecl, isClassOpSig, isPragSig,
			  fromClassDeclNameList, tyClDeclName
			)
import BasicTypes	( NewOrData(..), TopLevelFlag(..), RecFlag(..), EP(..) )
import RnHsSyn		( RenamedTyClDecl, 
			  RenamedClassOpSig, RenamedMonoBinds,
			  RenamedContext, RenamedHsDecl, RenamedSig, 
			  RenamedHsExpr, maybeGenericMatch
			)
import TcHsSyn		( TcMonoBinds, idsToMonoBinds )

import Inst		( InstOrigin(..), LIE, emptyLIE, plusLIE, plusLIEs, newDicts, newMethod )
import TcEnv		( TcId, TcEnv, TyThing(..), TyThingDetails(..), tcAddImportedIdInfo,
			  tcLookupClass, tcExtendTyVarEnvForMeths, tcExtendGlobalTyVars,
			  tcExtendLocalValEnv, tcExtendTyVarEnv, newDefaultMethodName
			)
import TcBinds		( tcBindWithSigs, tcSpecSigs )
import TcMonoType	( tcHsSigType, tcClassContext, checkSigTyVars, sigCtxt, mkTcSig )
import TcSimplify	( tcSimplifyAndCheck, bindInstsOfLocalFuns )
import TcType		( TcType, TcTyVar, tcInstTyVars, zonkTcSigTyVars )
import TcMonad
import Generics		( mkGenericRhs, validGenericMethodType )
import PrelInfo		( nO_METHOD_BINDING_ERROR_ID )
import Class		( classTyVars, classBigSig, classSelIds, classTyCon, Class, ClassOpItem,
			  DefMeth (..) )
import Bag		( bagToList )
import MkId		( mkDictSelId, mkDataConId, mkDataConWrapId, mkDefaultMethodId )
import DataCon		( mkDataCon, notMarkedStrict )
import Id		( Id, idType, idName )
import Name		( Name, nameOccName, isLocallyDefined, NamedThing(..), mkSysLocalName,
			  NameEnv, lookupNameEnv, emptyNameEnv, unitNameEnv, plusNameEnv, nameEnvElts )
import NameSet		( NameSet, mkNameSet, elemNameSet, emptyNameSet )
import Outputable
import Type		( Type, ClassContext, mkTyVarTys, mkDictTys, mkSigmaTy, mkClassPred,
			  splitTyConApp_maybe, isTyVarTy
			)
import Var		( TyVar )
import VarSet		( mkVarSet, emptyVarSet )
import CmdLineOpts
import ErrUtils		( dumpIfSet )
import Util		( count )
import Maybes		( seqMaybe, maybeToBool, orElse )
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
tcClassDecl1 :: TcEnv -> RenamedTyClDecl -> TcM (Name, TyThingDetails)
tcClassDecl1 rec_env
      	     (ClassDecl context class_name
			tyvar_names fundeps class_sigs def_methods pragmas 
			sys_names src_loc)
  = 	-- CHECK ARITY 1 FOR HASKELL 1.4
    doptsTc Opt_GlasgowExts				`thenTc` \ glaExts ->
    checkTc (glaExts || length tyvar_names == 1)
	    (classArityErr class_name)			`thenTc_`

	-- LOOK THINGS UP IN THE ENVIRONMENT
    tcLookupClass class_name				`thenTc` \ clas ->
    let
	tyvars   = classTyVars clas
	op_sigs  = filter isClassOpSig class_sigs
	op_names = [n | ClassOpSig n _ _ _ <- op_sigs]
	(_, datacon_name, datacon_wkr_name, sc_sel_names) = fromClassDeclNameList sys_names
    in
    tcExtendTyVarEnv tyvars				$ 

	-- CHECK THAT THE DEFAULT BINDINGS ARE LEGAL
    checkDefaultBinds clas op_names def_methods		`thenTc` \ dm_info ->
    checkGenericClassIsUnary clas dm_info		`thenTc_`
	
	-- CHECK THE CONTEXT
    tcSuperClasses clas context sc_sel_names	`thenTc` \ (sc_theta, sc_sel_ids) ->

	-- CHECK THE CLASS SIGNATURES,
    mapTc (tcClassSig rec_env clas tyvars dm_info) op_sigs	`thenTc` \ sig_stuff ->

	-- MAKE THE CLASS DETAILS
    let
	(op_tys, op_items) = unzip sig_stuff
        sc_tys		   = mkDictTys sc_theta
	dict_component_tys = sc_tys ++ op_tys

        dict_con = mkDataCon datacon_name
			     [notMarkedStrict | _ <- dict_component_tys]
			     [{- No labelled fields -}]
		      	     tyvars
		      	     [{-No context-}]
			     [{-No existential tyvars-}] [{-Or context-}]
			     dict_component_tys
		      	     (classTyCon clas)
			     dict_con_id dict_wrap_id

	dict_con_id  = mkDataConId datacon_wkr_name dict_con
	dict_wrap_id = mkDataConWrapId dict_con
    in
    returnTc (class_name, ClassDetails sc_theta sc_sel_ids op_items dict_con)
\end{code}

\begin{code}
checkDefaultBinds :: Class -> [Name] -> RenamedMonoBinds -> TcM (NameEnv (DefMeth Name))
  -- Check default bindings
  -- 	a) must be for a class op for this class
  --	b) must be all generic or all non-generic
  -- and return a mapping from class-op to DefMeth info

checkDefaultBinds clas ops EmptyMonoBinds = returnTc emptyNameEnv

checkDefaultBinds clas ops (AndMonoBinds b1 b2)
  = checkDefaultBinds clas ops b1	`thenTc` \ dm_info1 ->
    checkDefaultBinds clas ops b2	`thenTc` \ dm_info2 ->
    returnTc (dm_info1 `plusNameEnv` dm_info2)

checkDefaultBinds clas ops (FunMonoBind op _ matches loc)
  = tcAddSrcLoc loc					$

  	-- Check that the op is from this class
    checkTc (op `elem` ops) (badMethodErr clas op)		`thenTc_`

   	-- Check that all the defns ar generic, or none are
    checkTc (all_generic || none_generic) (mixedGenericErr op)	`thenTc_`

	-- Make up the right dm_info
    if all_generic then
	returnTc (unitNameEnv op GenDefMeth)
    else
	-- An explicit non-generic default method
	newDefaultMethodName op loc	`thenNF_Tc` \ dm_name ->
	returnTc (unitNameEnv op (DefMeth dm_name))

  where
    n_generic    = count (maybeToBool . maybeGenericMatch) matches
    none_generic = n_generic == 0
    all_generic  = n_generic == length matches

checkGenericClassIsUnary clas dm_info
  = -- Check that if the class has generic methods, then the
    -- class has only one parameter.  We can't do generic
    -- multi-parameter type classes!
    checkTc (unary || no_generics) (genericMultiParamErr clas)
  where
    unary 	= length (classTyVars clas) == 1
    no_generics = null [() | GenDefMeth <- nameEnvElts dm_info]
\end{code}


\begin{code}
tcSuperClasses :: Class
	       -> RenamedContext 	-- class context
	       -> [Name]		-- Names for superclass selectors
	       -> TcM (ClassContext,	-- the superclass context
		         [Id])  	-- superclass selector Ids

tcSuperClasses clas context sc_sel_names
  = 	-- Check the context.
	-- The renamer has already checked that the context mentions
	-- only the type variable of the class decl.

	-- For std Haskell check that the context constrains only tyvars
    doptsTc Opt_GlasgowExts			`thenTc` \ glaExts ->
    (if glaExts then
	returnTc ()
     else
	mapTc_ check_constraint context
    )						`thenTc_`

	-- Context is already kind-checked
    tcClassContext context			`thenTc` \ sc_theta ->
    let
       sc_sel_ids = [mkDictSelId sc_name clas | sc_name <- sc_sel_names]
    in
	-- Done
    returnTc (sc_theta, sc_sel_ids)

  where
    check_constraint sc@(HsPClass c tys) 
	= checkTc (all is_tyvar tys) (superClassErr clas sc)

    is_tyvar (HsTyVar _) = True
    is_tyvar other	 = False


tcClassSig :: TcEnv			-- Knot tying only!
	   -> Class	    		-- ...ditto...
	   -> [TyVar]		 	-- The class type variable, used for error check only
	   -> NameEnv (DefMeth Name)	-- Info about default methods
	   -> RenamedClassOpSig
	   -> TcM (Type,		-- Type of the method
		     ClassOpItem)	-- Selector Id, default-method Id, True if explicit default binding

-- This warrants an explanation: we need to separate generic
-- default methods and default methods later on in the compiler
-- so we distinguish them in checkDefaultBinds, and pass this knowledge in the
-- Class.DefMeth data structure. 

tcClassSig rec_env clas clas_tyvars dm_info
	   (ClassOpSig op_name maybe_dm op_ty src_loc)
  = tcAddSrcLoc src_loc $

	-- Check the type signature.  NB that the envt *already has*
	-- bindings for the type variables; see comments in TcTyAndClassDcls.

    -- NB: Renamer checks that the class type variable is mentioned in local_ty,
    -- and that it is not constrained by theta
    tcHsSigType op_ty				`thenTc` \ local_ty ->
    let
	global_ty   = mkSigmaTy clas_tyvars 
			        [mkClassPred clas (mkTyVarTys clas_tyvars)]
			        local_ty

	-- Build the selector id and default method id
	sel_id      = mkDictSelId op_name clas

	dm_info_name = maybe_dm `orElse` lookupNameEnv dm_info op_name `orElse` NoDefMeth

	dm_info_id = case dm_info_name of 
			NoDefMeth       -> NoDefMeth
			GenDefMeth      -> GenDefMeth
			DefMeth dm_name -> DefMeth (tcAddImportedIdInfo rec_env dm_id)
				        where
					   dm_id = mkDefaultMethodId dm_name clas global_ty
    in
	-- Check that for a generic method, the type of 
	-- the method is sufficiently simple
    checkTc (dm_info_name /= GenDefMeth || validGenericMethodType local_ty)
	    (badGenericMethodType op_name op_ty)		`thenTc_`

    returnTc (local_ty, (sel_id, dm_info_id))
\end{code}


%************************************************************************
%*									*
\subsection[ClassDcl-pass2]{Class decls pass 2: default methods}
%*									*
%************************************************************************

@mkImplicitClassBinds@ produces a binding for the selector function for each method
and superclass dictionary.

\begin{code}
mkImplicitClassBinds :: [Class] -> NF_TcM ([Id], TcMonoBinds)
mkImplicitClassBinds classes
  = returnNF_Tc (concat cls_ids_s, andMonoBindList binds_s)
	-- The selector binds are already in the selector Id's unfoldings
	-- We don't return the data constructor etc from the class,
	-- because that's done via the class's TyCon
  where
    (cls_ids_s, binds_s) = unzip (map mk_implicit classes)

    mk_implicit clas = (sel_ids, binds)
		     where
			sel_ids = classSelIds clas
			binds | isLocallyDefined clas = idsToMonoBinds sel_ids
			      | otherwise	      = EmptyMonoBinds
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
tcClassDecls2 :: [RenamedHsDecl] -> NF_TcM (LIE, TcMonoBinds)

tcClassDecls2 decls
  = foldr combine
	  (returnNF_Tc (emptyLIE, EmptyMonoBinds))
	  [tcClassDecl2 cls_decl | TyClD cls_decl <- decls, 
				   isClassDecl cls_decl,
				   isLocallyDefined (tyClDeclName cls_decl)]
  where
    combine tc1 tc2 = tc1 `thenNF_Tc` \ (lie1, binds1) ->
		      tc2 `thenNF_Tc` \ (lie2, binds2) ->
		      returnNF_Tc (lie1 `plusLIE` lie2,
				   binds1 `AndMonoBinds` binds2)
\end{code}

@tcClassDecl2@ generates bindings for polymorphic default methods
(generic default methods have by now turned into instance declarations)

\begin{code}
tcClassDecl2 :: RenamedTyClDecl		-- The class declaration
	     -> NF_TcM (LIE, TcMonoBinds)

tcClassDecl2 (ClassDecl context class_name
			tyvar_names _ sigs default_binds pragmas _ src_loc)
  = 	-- A locally defined class
    recoverNF_Tc (returnNF_Tc (emptyLIE, EmptyMonoBinds)) $ 
    tcAddSrcLoc src_loc		     		          $
    tcLookupClass class_name				  `thenNF_Tc` \ clas ->

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
    mapAndUnzipTc tc_dm op_items	`thenTc` \ (defm_binds, const_lies) ->

    returnTc (plusLIEs const_lies, andMonoBindList defm_binds)
    

tcDefMeth clas tyvars binds_in prags (_, NoDefMeth)  = returnTc (EmptyMonoBinds, emptyLIE)
tcDefMeth clas tyvars binds_in prags (_, GenDefMeth) = returnTc (EmptyMonoBinds, emptyLIE)
	-- Generate code for polymorphic default methods only
	-- (Generic default methods have turned into instance decls by now.)
	-- This is incompatible with Hugs, which expects a polymorphic 
	-- default method for every class op, regardless of whether or not 
	-- the programmer supplied an explicit default decl for the class.  
	-- (If necessary we can fix that, but we don't have a convenient Id to hand.)

tcDefMeth clas tyvars binds_in prags op_item@(_, DefMeth dm_id)
  = tcInstTyVars tyvars			`thenNF_Tc` \ (clas_tyvars, inst_tys, _) ->
    let
        theta = [(mkClassPred clas inst_tys)]
    in
    newDicts origin theta 		`thenNF_Tc` \ (this_dict, [this_dict_id]) ->

    tcExtendTyVarEnvForMeths tyvars clas_tyvars (
        tcMethodBind clas origin clas_tyvars inst_tys theta
	             binds_in prags False op_item
    )					`thenTc` \ (defm_bind, insts_needed, (_, local_dm_id)) ->
    
    tcAddErrCtxt (defltMethCtxt clas) $
    
        -- tcMethodBind has checked that the class_tyvars havn't
        -- been unified with each other or another type, but we must
        -- still zonk them before passing them to tcSimplifyAndCheck
    zonkTcSigTyVars clas_tyvars		`thenNF_Tc` \ clas_tyvars' ->
    
        -- Check the context
    tcSimplifyAndCheck
        (ptext SLIT("class") <+> ppr clas)
        (mkVarSet clas_tyvars')
        this_dict
        insts_needed			`thenTc` \ (const_lie, dict_binds) ->
    
    let
        full_bind = AbsBinds
    		    clas_tyvars'
    		    [this_dict_id]
    		    [(clas_tyvars', dm_id, local_dm_id)]
    		    emptyNameSet	-- No inlines (yet)
    		    (dict_binds `andMonoBinds` defm_bind)
    in
    returnTc (full_bind, const_lie)
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
tcMethodBind 
	:: Class
	-> InstOrigin
	-> [TcTyVar]		-- Instantiated type variables for the
				--  enclosing class/instance decl. 
				--  They'll be signature tyvars, and we
				--  want to check that they don't get bound
	-> [TcType]		-- Instance types
	-> TcThetaType		-- Available theta; this could be used to check
				--  the method signature, but actually that's done by
				--  the caller;  here, it's just used for the error message
	-> RenamedMonoBinds	-- Method binding (pick the right one from in here)
	-> [RenamedSig]		-- Pramgas (just for this one)
	-> Bool			-- True <=> This method is from an instance declaration
	-> ClassOpItem		-- The method selector and default-method Id
	-> TcM (TcMonoBinds, LIE, (LIE, TcId))

tcMethodBind clas origin inst_tyvars inst_tys inst_theta
	     meth_binds prags is_inst_decl (sel_id, dm_info)
  = tcGetSrcLoc 			`thenNF_Tc` \ loc -> 
    newMethod origin sel_id inst_tys	`thenNF_Tc` \ meth@(_, meth_id) ->
    mkTcSig meth_id loc			`thenNF_Tc` \ sig_info -> 
    let
	meth_name  = idName meth_id
	sig_msg    = ptext SLIT("When checking the expected type for class method") <+> ppr sel_id
	meth_prags = find_prags (idName sel_id) meth_name prags
    in
	-- Figure out what method binding to use
	-- If the user suppplied one, use it, else construct a default one
    (case find_bind (idName sel_id) meth_name meth_binds of
	Just user_bind -> returnTc user_bind 
	Nothing	       -> mkDefMethRhs is_inst_decl clas inst_tys sel_id loc dm_info	`thenTc` \ rhs ->
			  returnTc (FunMonoBind meth_name False	-- Not infix decl
				                [mkSimpleMatch [] rhs Nothing loc] loc)
    )								`thenTc` \ meth_bind ->
     -- Check the bindings; first add inst_tyvars to the envt
     -- so that we don't quantify over them in nested places
     -- The *caller* put the class/inst decl tyvars into the envt
     tcExtendGlobalTyVars (mkVarSet inst_tyvars) 
     		    (tcAddErrCtxt (methodCtxt sel_id)		$
     		     tcBindWithSigs NotTopLevel meth_bind 
     		     [sig_info] meth_prags NonRecursive 
     		    ) 						`thenTc` \ (binds, insts, _) -> 

     tcExtendLocalValEnv [(meth_name, meth_id)] 
			 (tcSpecSigs meth_prags)		`thenTc` \ (prag_binds1, prag_lie) ->
     
     -- The prag_lie for a SPECIALISE pragma will mention the function
     -- itself, so we have to simplify them away right now lest they float
     -- outwards!
     bindInstsOfLocalFuns prag_lie [meth_id]	`thenTc` \ (prag_lie', prag_binds2) ->

     -- Now check that the instance type variables
     -- (or, in the case of a class decl, the class tyvars)
     -- have not been unified with anything in the environment
     --	
     -- We do this for each method independently to localise error messages
     -- ...and this is why the call to tcExtendGlobalTyVars must be here
     --    rather than in the caller
     tcAddErrCtxtM (sigCtxt sig_msg inst_tyvars inst_theta (idType meth_id))	$
     checkSigTyVars inst_tyvars emptyVarSet					`thenTc_` 

     returnTc (binds `AndMonoBinds` prag_binds1 `AndMonoBinds` prag_binds2, 
	       insts `plusLIE` prag_lie',
	       meth)

     -- The user didn't supply a method binding, 
     -- so we have to make up a default binding
     -- The RHS of a default method depends on the default-method info
mkDefMethRhs is_inst_decl clas inst_tys sel_id loc (DefMeth dm_id)
  =  -- An polymorphic default method
    returnTc (HsVar (idName dm_id))

mkDefMethRhs is_inst_decl clas inst_tys sel_id loc NoDefMeth
  =  	-- No default method
	-- Warn only if -fwarn-missing-methods
    doptsTc Opt_WarnMissingMethods  `thenNF_Tc` \ warn -> 
    warnTc (is_inst_decl && warn)
   	   (omittedMethodWarn sel_id clas)		`thenNF_Tc_`
    returnTc error_rhs
  where
    error_rhs = HsApp (HsVar (getName nO_METHOD_BINDING_ERROR_ID)) 
	    		  (HsLit (HsString (_PK_ error_msg)))
    error_msg = showSDoc (hcat [ppr loc, text "|", ppr sel_id ])


mkDefMethRhs is_inst_decl clas inst_tys sel_id loc GenDefMeth 
  =  	-- A generic default method
	-- If the method is defined generically, we can only do the job if the
	-- instance declaration is for a single-parameter type class with
	-- a type constructor applied to type arguments in the instance decl
	-- 	(checkTc, so False provokes the error)
     checkTc (not is_inst_decl || simple_inst)
	     (badGenericInstance sel_id clas)			`thenTc_`

     ioToTc (dumpIfSet opt_PprStyle_Debug "Generic RHS" stuff)	`thenNF_Tc_`
     returnTc rhs
  where
    rhs = mkGenericRhs sel_id clas_tyvar tycon

    stuff = vcat [ppr clas <+> ppr inst_tys,
		  nest 4 (ppr sel_id <+> equals <+> ppr rhs)]

	  -- The tycon is only used in the generic case, and in that
	  -- case we require that the instance decl is for a single-parameter
	  -- type class with type variable arguments:
	  --	instance (...) => C (T a b)
    simple_inst   = maybeToBool maybe_tycon
    clas_tyvar    = head (classTyVars clas)
    Just tycon	  = maybe_tycon
    maybe_tycon   = case inst_tys of 
			[ty] -> case splitTyConApp_maybe ty of
				  Just (tycon, arg_tys) | all isTyVarTy arg_tys -> Just tycon
				  other						-> Nothing
			other -> Nothing
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
find_prags sel_name meth_name (InlineSig name phase loc : prags)
   | name == sel_name = InlineSig meth_name phase loc : find_prags sel_name meth_name prags
find_prags sel_name meth_name (NoInlineSig name phase loc : prags)
   | name == sel_name = NoInlineSig meth_name phase loc : find_prags sel_name meth_name prags
find_prags sel_name meth_name (prag:prags) = find_prags sel_name meth_name prags
\end{code}


Contexts and errors
~~~~~~~~~~~~~~~~~~~
\begin{code}
classArityErr class_name
  = ptext SLIT("Too many parameters for class") <+> quotes (ppr class_name)

superClassErr clas sc
  = ptext SLIT("Illegal superclass constraint") <+> quotes (ppr sc)
    <+> ptext SLIT("in declaration for class") <+> quotes (ppr clas)

defltMethCtxt clas
  = ptext SLIT("When checking the default methods for class") <+> quotes (ppr clas)

methodCtxt sel_id
  = ptext SLIT("In the definition for method") <+> quotes (ppr sel_id)

badMethodErr clas op
  = hsep [ptext SLIT("Class"), quotes (ppr clas), 
	  ptext SLIT("does not have a method"), quotes (ppr op)]

omittedMethodWarn sel_id clas
  = sep [ptext SLIT("No explicit method nor default method for") <+> quotes (ppr sel_id), 
	 ptext SLIT("in an instance declaration for") <+> quotes (ppr clas)]

badGenericMethodType op op_ty
  = hang (ptext SLIT("Generic method type is too complex"))
       4 (vcat [ppr op <+> dcolon <+> ppr op_ty,
		ptext SLIT("You can only use type variables, arrows, and tuples")])

badGenericInstance sel_id clas
  = sep [ptext SLIT("Can't derive generic code for") <+> quotes (ppr sel_id),
	 ptext SLIT("because the instance declaration is not for a simple type (T a b c)"),
	 ptext SLIT("(where T is a derivable type constructor)"),
	 ptext SLIT("in an instance declaration for") <+> quotes (ppr clas)]

mixedGenericErr op
  = ptext SLIT("Can't mix generic and non-generic equations for class method") <+> quotes (ppr op)

genericMultiParamErr clas
  = ptext SLIT("The multi-parameter class") <+> quotes (ppr clas) <+> 
    ptext SLIT("cannot have generic methods")
\end{code}
