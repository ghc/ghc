%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcInstDecls]{Typechecking instance declarations}

\begin{code}
module TcInstDcls (
	tcInstDecls1,
	tcInstDecls2
    ) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), InstDecl(..),
			  HsBinds(..), MonoBinds(..), GRHSsAndBinds(..), GRHS(..),
			  HsExpr(..), InPat(..), HsLit(..), Sig(..),
			  unguardedRHS,
			  collectMonoBinders, andMonoBinds
			)
import HsBinds		( sigsForMe )
import RnHsSyn		( RenamedHsBinds, RenamedMonoBinds,
			  RenamedInstDecl, RenamedHsExpr,
			  RenamedSig, RenamedHsDecl
			)
import TcHsSyn		( TcMonoBinds, TcIdOcc(..), TcIdBndr, 
			  maybeBoxedPrimType, tcIdType
			)

import TcBinds		( tcPragmaSigs )
import TcClassDcl	( tcMethodBind, badMethodErr )
import TcMonad
import RnMonad		( RnNameSupply )
import Inst		( Inst, InstOrigin(..),
			  newDicts, LIE, emptyLIE, plusLIE, plusLIEs )
import TcDeriv		( tcDeriving )
import TcEnv		( GlobalValueEnv, tcExtendGlobalValEnv, tcAddImportedIdInfo )
import TcInstUtil	( InstInfo(..), mkInstanceRelatedIds, classDataCon )
import TcKind		( TcKind, unifyKind )
import TcMonoType	( tcHsType )
import TcSimplify	( tcSimplifyAndCheck )
import TcType		( TcType, TcTyVar, TcTyVarSet, 
			  zonkSigTyVar, tcInstSigTyVars, tcInstType, tcInstTheta
			)

import Bag		( emptyBag, unitBag, unionBags, unionManyBags,
			  foldBag, bagToList, Bag
			)
import CmdLineOpts	( opt_GlasgowExts )
import Class		( classBigSig, Class )
import Id		( isNullaryDataCon, dataConArgTys, replaceIdInfo, idName, Id )
import Maybes 		( maybeToBool, seqMaybe, catMaybes )
import Name		( nameOccName, mkLocalName,
			  isLocallyDefined, Module,
			  NamedThing(..)
			)
import PrelVals		( eRROR_ID )
import PprType		( pprParendType,  pprConstraint )
import SrcLoc		( SrcLoc, noSrcLoc )
import TyCon		( isSynTyCon, isDataTyCon, tyConDerivings )
import Type		( Type, ThetaType, isUnpointedType,
			  splitSigmaTy, isTyVarTy, mkSigmaTy,
			  splitTyConApp_maybe, splitDictTy_maybe,
			  splitAlgTyConApp_maybe, splitRhoTy,
			  tyVarsOfTypes
			)
import TyVar		( zipTyVarEnv, mkTyVarSet, tyVarSetToList, TyVar )
import TysPrim		( byteArrayPrimTyCon, mutableByteArrayPrimTyCon )
import TysWiredIn	( stringTy )
import Unique		( Unique, cCallableClassKey, cReturnableClassKey, Uniquable(..) )
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

\begin{code}
tcInstDecls1 :: GlobalValueEnv		-- Contains IdInfo for dfun ids
	     -> [RenamedHsDecl]
	     -> Module			-- module name for deriving
	     -> RnNameSupply			-- for renaming derivings
	     -> TcM s (Bag InstInfo,
		       RenamedHsBinds,
		       SDoc)

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


tcInstDecl1 :: GlobalValueEnv -> Module -> RenamedInstDecl -> NF_TcM s (Bag InstInfo)

tcInstDecl1 unf_env mod_name (InstDecl poly_ty binds uprags (Just dfun_name) src_loc)
  = 	-- Prime error recovery, set source location
    recoverNF_Tc (returnNF_Tc emptyBag)	$
    tcAddSrcLoc src_loc			$

	-- Type-check all the stuff before the "where"
    tcHsType poly_ty			`thenTc` \ poly_ty' ->
    let
	(tyvars, theta, dict_ty) = splitSigmaTy poly_ty'
	(clas, inst_tys)         = case splitDictTy_maybe dict_ty of
				     Nothing   -> pprPanic "tcInstDecl1" (ppr poly_ty)
				     Just pair -> pair
    in

	-- Check for respectable instance type
    scrutiniseInstanceType clas inst_tys	`thenTc_`

	-- Make the dfun id and constant-method ids
    let
	(dfun_id, dfun_theta) = mkInstanceRelatedIds dfun_name
				         clas tyvars inst_tys theta
	-- Add info from interface file
	final_dfun_id = tcAddImportedIdInfo unf_env dfun_id
    in
    returnTc (unitBag (InstInfo clas tyvars inst_tys theta	
				dfun_theta final_dfun_id
			     	binds src_loc uprags))
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

tcInstDecl2 (InstInfo clas inst_tyvars inst_tys
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
    let 
	origin = InstanceDeclOrigin
        (class_tyvars,
	 sc_theta, sc_sel_ids,
	 op_sel_ids, defm_ids) = classBigSig clas
    in
      
	-- Instantiate the instance decl with tc-style type variables
    tcInstSigTyVars inst_tyvars		`thenNF_Tc` \ (inst_tyvars', _, tenv) ->
    mapNF_Tc (tcInstType tenv) inst_tys	`thenNF_Tc` \ inst_tys' ->
    tcInstTheta tenv dfun_theta		`thenNF_Tc` \ dfun_theta' ->
    tcInstTheta tenv inst_decl_theta	`thenNF_Tc` \ inst_decl_theta' ->

         -- Instantiate the super-class context with inst_tys
    tcInstTheta (zipTyVarEnv class_tyvars inst_tys') sc_theta		`thenNF_Tc` \ sc_theta' ->

	 -- Create dictionary Ids from the specified instance contexts.
    newDicts origin sc_theta'		`thenNF_Tc` \ (sc_dicts,        sc_dict_ids) ->
    newDicts origin dfun_theta'		`thenNF_Tc` \ (dfun_arg_dicts,  dfun_arg_dicts_ids)  ->
    newDicts origin inst_decl_theta'	`thenNF_Tc` \ (inst_decl_dicts, _) ->
    newDicts origin [(clas,inst_tys')]	`thenNF_Tc` \ (this_dict,       [this_dict_id]) ->

	 -- Check that all the method bindings come from this class
    let
	check_from_this_class (bndr, loc)
	  | nameOccName bndr `elem` sel_names = returnNF_Tc ()
	  | otherwise			      = tcAddSrcLoc loc $
						addErrTc (badMethodErr bndr clas)
	sel_names = map getOccName op_sel_ids
	bndrs = bagToList (collectMonoBinders monobinds)
    in
    mapNF_Tc check_from_this_class bndrs		`thenNF_Tc_`

    tcExtendGlobalValEnv (catMaybes defm_ids) (

		-- Default-method Ids may be mentioned in synthesised RHSs 
	mapAndUnzip3Tc (tcMethodBind clas origin inst_tys' inst_tyvars' monobinds uprags True) 
		       (op_sel_ids `zip` defm_ids)
    )		 	`thenTc` \ (method_binds_s, insts_needed_s, meth_lies_w_ids) ->

	-- Deal with SPECIALISE instance pragmas
    let
	dfun_prags = [Sig (idName dfun_id) ty loc | SpecInstSig ty loc <- uprags]
    in
    tcExtendGlobalValEnv [dfun_id] (
	tcPragmaSigs dfun_prags
    )					`thenTc` \ (prag_info_fn, prag_binds, prag_lie) ->

	-- Check the overloading constraints of the methods and superclasses
    mapNF_Tc zonkSigTyVar inst_tyvars' 	`thenNF_Tc` \ zonked_inst_tyvars ->

    let
        inst_tyvars_set = mkTyVarSet zonked_inst_tyvars

	(meth_lies, meth_ids) = unzip meth_lies_w_ids

		 -- These insts are in scope; quite a few, eh?
	avail_insts = this_dict			`plusLIE` 
		      dfun_arg_dicts		`plusLIE`
		      sc_dicts			`plusLIE`
		      unionManyBags meth_lies

        methods_lie = plusLIEs insts_needed_s
    in

	-- Ditto method bindings
    tcAddErrCtxt methodCtxt (
      tcSimplifyAndCheck
		 (ptext SLIT("instance declaration context"))
		 inst_tyvars_set			-- Local tyvars
		 avail_insts
		 methods_lie
    )						 `thenTc` \ (const_lie1, lie_binds1) ->
    
	-- Check that we *could* construct the superclass dictionaries,
	-- even though we are *actually* going to pass the superclass dicts in;
	-- the check ensures that the caller will never have 
	--a problem building them.
    tcAddErrCtxt superClassCtxt (
      tcSimplifyAndCheck
		 (ptext SLIT("instance declaration context"))
		 inst_tyvars_set		-- Local tyvars
		 inst_decl_dicts		-- The instance dictionaries available
		 sc_dicts			-- The superclass dicationaries reqd
    )					`thenTc_`
    						-- Ignore the result; we're only doing
						-- this to make sure it can be done.

	-- Now do the simplification again, this time to get the
	-- bindings; this time we use an enhanced "avails"
	-- Ignore errors because they come from the *previous* tcSimplify
    discardErrsTc (
	tcSimplifyAndCheck
		 (ptext SLIT("instance declaration context"))
		 inst_tyvars_set
		 dfun_arg_dicts		-- NB! Don't include this_dict here, else the sc_dicts
					-- get bound by just selecting from this_dict!!
		 sc_dicts
    )						 `thenTc` \ (const_lie2, lie_binds2) ->
	

	-- Create the result bindings
    let
        dict_constr   = classDataCon clas
	scs_and_meths = sc_dict_ids ++ meth_ids

	dict_rhs
	  | null scs_and_meths
	  = 	-- Blatant special case for CCallable, CReturnable
		-- If the dictionary is empty then we should never
		-- select anything from it, so we make its RHS just
		-- emit an error message.  This in turn means that we don't
		-- mention the constructor, which doesn't exist for CCallable, CReturnable
		-- Hardly beautiful, but only three extra lines.
	    HsApp (TyApp (HsVar (RealId eRROR_ID)) [tcIdType this_dict_id])
		  (HsLitOut (HsString msg) stringTy)

	  | otherwise	-- The common case
	  = HsCon dict_constr inst_tys' (map HsVar (sc_dict_ids ++ meth_ids))
		-- We don't produce a binding for the dict_constr; instead we
		-- just generate the saturated constructor directly
	  where
	    msg = _PK_ ("Compiler error: bad dictionary " ++ showSDoc (ppr clas))

	dict_bind    = VarMonoBind this_dict_id dict_rhs
	method_binds = andMonoBinds method_binds_s

	final_dfun_id = replaceIdInfo dfun_id (prag_info_fn (idName dfun_id))
				-- Pretty truesome
	main_bind
	  = AbsBinds
		 zonked_inst_tyvars
		 dfun_arg_dicts_ids
		 [(inst_tyvars', RealId final_dfun_id, this_dict_id)] 
		 (lie_binds1	`AndMonoBinds` 
		  lie_binds2	`AndMonoBinds`
		  method_binds	`AndMonoBinds`
		  dict_bind)
    in
    returnTc (const_lie1 `plusLIE` const_lie2 `plusLIE` prag_lie,
	      main_bind `AndMonoBinds` prag_binds)
\end{code}


%************************************************************************
%*									*
\subsection{Checking for a decent instance type}
%*									*
%************************************************************************

@scrutiniseInstanceType@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere). In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.

\begin{code}
scrutiniseInstanceType clas inst_taus
  |	-- CCALL CHECK (a).... urgh!
	-- To verify that a user declaration of a CCallable/CReturnable 
	-- instance is OK, we must be able to see the constructor(s)
	-- of the instance type (see next guard.)
	--  
        -- We flag this separately to give a more precise error msg.
        --
     (uniqueOf clas == cCallableClassKey || uniqueOf clas == cReturnableClassKey)
  && is_alg_tycon_app && not constructors_visible
  = failWithTc (invisibleDataConPrimCCallErr clas first_inst_tau)

  |	-- CCALL CHECK (b) 
	-- A user declaration of a CCallable/CReturnable instance
	-- must be for a "boxed primitive" type.
    (uniqueOf clas == cCallableClassKey   && not (ccallable_type   first_inst_tau)) ||
    (uniqueOf clas == cReturnableClassKey && not (creturnable_type first_inst_tau))
  = failWithTc (nonBoxedPrimCCallErr clas first_inst_tau)

  	-- DERIVING CHECK
	-- It is obviously illegal to have an explicit instance
	-- for something that we are also planning to `derive'
  | maybeToBool alg_tycon_app_maybe && clas `elem` (tyConDerivings alg_tycon)
  = failWithTc (derivingWhenInstanceExistsErr clas first_inst_tau)
	   -- Kind check will have ensured inst_taus is of length 1

	-- WITH HASKELL 1.4, MUST HAVE C (T a b c)
  |  not opt_GlasgowExts
  && not (length inst_taus == 1 &&
	  maybeToBool maybe_tycon_app &&	-- Yes, there's a type constuctor
          not (isSynTyCon tycon) &&		-- ...but not a synonym
          all isTyVarTy arg_tys && 		-- Applied to type variables
	  length (tyVarSetToList (tyVarsOfTypes arg_tys)) == length arg_tys
		 -- This last condition checks that all the type variables are distinct
     )
  = failWithTc (instTypeErr clas inst_taus
			(text "the instance type must be of form (T a b c)" $$
			 text "where T is not a synonym, and a,b,c are distinct type variables")
    )

  | otherwise
  = returnTc ()

  where
    (first_inst_tau : _)       = inst_taus

	-- Stuff for algebraic or -> type
    maybe_tycon_app	  = splitTyConApp_maybe first_inst_tau
    Just (tycon, arg_tys) = maybe_tycon_app

	-- Stuff for an *algebraic* data type
    alg_tycon_app_maybe	           = splitAlgTyConApp_maybe first_inst_tau
					-- The "Alg" part looks through synonyms
    is_alg_tycon_app		   = maybeToBool alg_tycon_app_maybe
    Just (alg_tycon, _, data_cons) = alg_tycon_app_maybe

    constructors_visible = not (null data_cons)
 

-- These conditions come directly from what the DsCCall is capable of.
-- Totally grotesque.  Green card should solve this.

ccallable_type   ty = isUnpointedType ty ||				-- Allow CCallable Int# etc
                      maybeToBool (maybeBoxedPrimType ty) ||	-- Ditto Int etc
		      ty == stringTy ||
		      byte_arr_thing
  where
    byte_arr_thing = case splitAlgTyConApp_maybe ty of
			Just (tycon, ty_args, [data_con]) | isDataTyCon tycon -> 
		     		length data_con_arg_tys == 2 &&
				maybeToBool maybe_arg2_tycon &&
				(arg2_tycon == byteArrayPrimTyCon ||
				 arg2_tycon == mutableByteArrayPrimTyCon)
			     where
				data_con_arg_tys = dataConArgTys data_con ty_args
				(data_con_arg_ty1 : data_con_arg_ty2 : _) = data_con_arg_tys
				maybe_arg2_tycon = splitTyConApp_maybe data_con_arg_ty2
				Just (arg2_tycon,_) = maybe_arg2_tycon

			other -> False

creturnable_type ty = maybeToBool (maybeBoxedPrimType ty) ||
			-- Or, a data type with a single nullary constructor
		      case (splitAlgTyConApp_maybe ty) of
			Just (tycon, tys_applied, [data_con])
				-> isNullaryDataCon data_con
			other -> False
\end{code}

\begin{code}

instTypeErr clas tys msg
  = sep [ptext SLIT("Illegal instance declaration for") <+> quotes (pprConstraint clas tys),
	 nest 4 (parens msg)
    ]

derivingWhenInstanceExistsErr clas tycon
  = hang (hsep [ptext SLIT("Deriving class"), 
		       quotes (ppr clas), 
		       ptext SLIT("type"), quotes (ppr tycon)])
         4 (ptext SLIT("when an explicit instance exists"))

nonBoxedPrimCCallErr clas inst_ty
  = hang (ptext SLIT("Unacceptable instance type for ccall-ish class"))
	 4 (hsep [ ptext SLIT("class"), ppr clas, ptext SLIT("type"),
    		        ppr inst_ty])

{-
  Declaring CCallable & CReturnable instances in a module different
  from where the type was defined. Caused by importing data type
  abstractly (either programmatically or by the renamer being over-eager
  in its pruning.)
-}
invisibleDataConPrimCCallErr clas inst_ty
  = hang (hsep [ptext SLIT("Constructors for"), quotes (ppr inst_ty),
		ptext SLIT("not visible when checking"),
                quotes (ppr clas), ptext SLIT("instance")])
        4 (hsep [text "(Try either importing", ppr inst_ty, 
	         text "non-abstractly or compile using -fno-prune-tydecls ..)"])

methodCtxt     = ptext SLIT("When checking the methods of an instance declaration")
superClassCtxt = ptext SLIT("When checking the superclasses of an instance declaration")
\end{code}
