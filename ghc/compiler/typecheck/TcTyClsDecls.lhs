%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcTyClsDecls]{Typecheck type and class declarations}

\begin{code}
module TcTyClsDecls (
	tcTyAndClassDecls
    ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlags, DynFlag(..), dopt )
import HsSyn		( TyClDecl(..),  
			  ConDecl(..),   Sig(..), HsPred(..), 
			  tyClDeclName, hsTyVarNames, tyClDeclTyVars,
			  isIfaceSigDecl, isClassDecl, isSynDecl, isClassOpSig
			)
import RnHsSyn		( RenamedTyClDecl, tyClDeclFVs )
import BasicTypes	( RecFlag(..), NewOrData(..) )
import HscTypes		( implicitTyThingIds )
import Module		( Module )

import TcMonad
import TcEnv		( TcEnv, RecTcEnv, TcTyThing(..), TyThing(..), TyThingDetails(..),
			  tcExtendKindEnv, tcLookup, tcExtendGlobalEnv,
			  isLocalThing )
import TcTyDecls	( tcTyDecl, kcConDetails, checkValidTyCon )
import TcClassDcl	( tcClassDecl1, checkValidClass )
import TcInstDcls	( tcAddDeclCtxt )
import TcMonoType	( kcHsTyVars, kcHsType, kcHsLiftedSigType, kcHsContext, mkTyClTyVars )
import TcMType		( newKindVar, zonkKindEnv )
import TcUnify		( unifyKind )
import TcType		( Type, Kind, TcKind, mkArrowKind, liftedTypeKind, zipFunTys )
import Type		( splitTyConApp_maybe )
import Variance         ( calcTyConArgVrcs )
import Class		( Class, mkClass, classTyCon )
import TyCon		( TyCon, ArgVrcs, AlgTyConFlavour(..), DataConDetails(..), visibleDataCons,
			  tyConKind, tyConTyVars, tyConDataCons, isNewTyCon,
			  mkSynTyCon, mkAlgTyCon, mkClassTyCon, mkForeignTyCon, 
			)
import TysWiredIn	( unitTy )
import Subst		( substTyWith )
import DataCon		( dataConOrigArgTys )
import Var		( varName )
import FiniteMap
import Digraph		( stronglyConnComp, SCC(..) )
import Name		( Name, getSrcLoc, isTyVarName )
import NameEnv
import NameSet
import Outputable
import Maybes		( mapMaybe )
import ErrUtils		( Message )
import HsDecls          ( getClassDeclSysNames )
import Generics         ( mkTyConGenInfo )
\end{code}


%************************************************************************
%*									*
\subsection{Type checking for type and class declarations}
%*									*
%************************************************************************

The main function
~~~~~~~~~~~~~~~~~
\begin{code}
tcTyAndClassDecls :: RecTcEnv		-- Knot tying stuff
		  -> Module 		-- Current module
		  -> [RenamedTyClDecl]
		  -> TcM [TyThing]	-- Returns newly defined things:
					-- types, classes and implicit Ids

tcTyAndClassDecls unf_env this_mod decls
  = sortByDependency decls 		`thenTc` \ groups ->
    tcGroups unf_env this_mod groups

tcGroups unf_env this_mod []
  = tcGetEnv	`thenNF_Tc` \ env ->
    returnTc []

tcGroups unf_env this_mod (group:groups)
  = tcGroup unf_env this_mod group	`thenTc` \ (env, new_things1) ->
    tcSetEnv env			$
    tcGroups unf_env this_mod groups	`thenTc` \ new_things2 ->
    returnTc (new_things1 ++ new_things2)
\end{code}

Dealing with a group
~~~~~~~~~~~~~~~~~~~~
Consider a mutually-recursive group, binding 
a type constructor T and a class C.

Step 1: 	getInitialKind
	Construct a KindEnv by binding T and C to a kind variable 

Step 2: 	kcTyClDecl
	In that environment, do a kind check

Step 3: Zonk the kinds

Step 4: 	buildTyConOrClass
	Construct an environment binding T to a TyCon and C to a Class.
	a) Their kinds comes from zonking the relevant kind variable
	b) Their arity (for synonyms) comes direct from the decl
	c) The funcional dependencies come from the decl
	d) The rest comes a knot-tied binding of T and C, returned from Step 4
	e) The variances of the tycons in the group is calculated from 
		the knot-tied stuff

Step 5: 	tcTyClDecl1
	In this environment, walk over the decls, constructing the TyCons and Classes.
	This uses in a strict way items (a)-(c) above, which is why they must
	be constructed in Step 4. Feed the results back to Step 4.
	For this step, pass the is-recursive flag as the wimp-out flag
	to tcTyClDecl1.
	

Step 6:		Extend environment
	We extend the type environment with bindings not only for the TyCons and Classes,
	but also for their "implicit Ids" like data constructors and class selectors

Step 7:		checkValidTyCl
	For a recursive group only, check all the decls again, just
	to check all the side conditions on validity.  We could not
	do this before because we were in a mutually recursive knot.


The knot-tying parameters: @rec_details_list@ is an alist mapping @Name@s to
@TyThing@s.  @rec_vrcs@ is a finite map from @Name@s to @ArgVrcs@s.

\begin{code}
tcGroup :: RecTcEnv -> Module -> SCC RenamedTyClDecl 
	-> TcM (TcEnv, 		-- Input env extended by types and classes only
		[TyThing])	-- Things defined by this group
					
tcGroup unf_env this_mod scc
  = getDOptsTc							`thenNF_Tc` \ dflags ->
	-- Step 1
    mapNF_Tc getInitialKind decls 				`thenNF_Tc` \ initial_kinds ->

	-- Step 2
    tcExtendKindEnv initial_kinds (mapTc kcTyClDecl decls)	`thenTc_`

	-- Step 3
    zonkKindEnv initial_kinds			`thenNF_Tc` \ final_kinds ->

	-- Tie the knot
    traceTc (text "starting" <+> ppr final_kinds)		`thenTc_`
    fixTc ( \ ~(rec_details_list, _, _) ->
		-- Step 4 
 	let
	    kind_env    = mkNameEnv final_kinds
	    rec_details = mkNameEnv rec_details_list

	    tyclss, all_tyclss :: [TyThing]
	    tyclss = map (buildTyConOrClass dflags is_rec kind_env 
				            rec_vrcs rec_details) decls

		-- Add the tycons that come from the classes
		-- We want them in the environment because 
		-- they are mentioned in interface files
	    all_tyclss  = [ATyCon (classTyCon clas) | AClass clas <- tyclss]
			  ++ tyclss

		-- Calculate variances, and (yes!) feed back into buildTyConOrClass.
            rec_vrcs    = calcTyConArgVrcs [tc | ATyCon tc <- all_tyclss]
	in
		-- Step 5
		-- Extend the environment with the final 
		-- TyCons/Classes and check the decls
	tcExtendGlobalEnv all_tyclss			$
	mapTc (tcTyClDecl1 unf_env) decls		`thenTc` \ tycls_details ->

		-- Return results
	tcGetEnv					`thenNF_Tc` \ env ->
	returnTc (tycls_details, env, all_tyclss)
    )						`thenTc` \ (_, env, all_tyclss) ->

	-- Step 7: Check validity
    traceTc (text "ready for validity check")	`thenTc_`
    tcSetEnv env (
	mapTc_ (checkValidTyCl this_mod) decls
    )						`thenTc_`
    traceTc (text "done")			`thenTc_`
   
    let
	implicit_things = [AnId id | id <- implicitTyThingIds all_tyclss]
	new_things      = all_tyclss ++ implicit_things
    in
    returnTc (env, new_things)

  where
    is_rec = case scc of
		AcyclicSCC _ -> NonRecursive
		CyclicSCC _  -> Recursive

    decls = case scc of
		AcyclicSCC decl -> [decl]
		CyclicSCC decls -> decls

tcTyClDecl1 unf_env decl
  | isClassDecl decl = tcAddDeclCtxt decl (tcClassDecl1 decl)
  | otherwise	     = tcAddDeclCtxt decl (tcTyDecl     unf_env decl)

-- We do the validity check over declarations, rather than TyThings
-- only so that we can add a nice context with tcAddDeclCtxt
checkValidTyCl this_mod decl
  = tcLookup (tcdName decl)	`thenNF_Tc` \ (AGlobal thing) ->
    if not (isLocalThing this_mod thing) then
	-- Don't bother to check validity for non-local things
	returnTc ()
    else
    tcAddDeclCtxt decl $
    case thing of
	ATyCon tc -> checkValidTyCon tc
	AClass cl -> checkValidClass cl
\end{code}


%************************************************************************
%*									*
\subsection{Step 1: Initial environment}
%*									*
%************************************************************************

\begin{code}
getInitialKind :: RenamedTyClDecl -> NF_TcM (Name, TcKind)
getInitialKind decl
 = kcHsTyVars (tyClDeclTyVars decl)	`thenNF_Tc` \ arg_kinds ->
   newKindVar				`thenNF_Tc` \ result_kind  ->
   returnNF_Tc (tcdName decl, mk_kind arg_kinds result_kind)

mk_kind tvs_w_kinds res_kind = foldr (mkArrowKind . snd) res_kind tvs_w_kinds
\end{code}


%************************************************************************
%*									*
\subsection{Step 2: Kind checking}
%*									*
%************************************************************************

We need to kind check all types in the mutually recursive group
before we know the kind of the type variables.  For example:

class C a where
   op :: D b => a -> b -> b

class D c where
   bop :: (Monad c) => ...

Here, the kind of the locally-polymorphic type variable "b"
depends on *all the uses of class D*.  For example, the use of
Monad c in bop's type signature means that D must have kind Type->Type.

\begin{code}
kcTyClDecl :: RenamedTyClDecl -> TcM ()

kcTyClDecl decl@(TySynonym {tcdSynRhs = rhs})
  = kcTyClDeclBody decl		$ \ result_kind ->
    kcHsType rhs		`thenTc` \ rhs_kind ->
    unifyKind result_kind rhs_kind

kcTyClDecl (ForeignType {}) = returnTc ()

kcTyClDecl decl@(TyData {tcdND = new_or_data, tcdCtxt = context, tcdCons = con_decls})
  = kcTyClDeclBody decl			$ \ result_kind ->
    kcHsContext context			`thenTc_` 
    mapTc_ kc_con_decl (visibleDataCons con_decls)
  where
    kc_con_decl (ConDecl _ _ ex_tvs ex_ctxt details loc)
      = kcHsTyVars ex_tvs		`thenNF_Tc` \ kind_env ->
	tcExtendKindEnv kind_env	$
	kcConDetails new_or_data ex_ctxt details

kcTyClDecl decl@(ClassDecl {tcdCtxt = context,  tcdSigs = class_sigs})
  = kcTyClDeclBody decl		$ \ result_kind ->
    kcHsContext context		`thenTc_`
    mapTc_ kc_sig (filter isClassOpSig class_sigs)
  where
    kc_sig (ClassOpSig _ _ op_ty loc) = kcHsLiftedSigType op_ty

kcTyClDeclBody :: RenamedTyClDecl -> (Kind -> TcM a) -> TcM a
-- Extend the env with bindings for the tyvars, taken from
-- the kind of the tycon/class.  Give it to the thing inside, and 
-- check the result kind matches
kcTyClDeclBody decl thing_inside
  = tcAddDeclCtxt decl		$
    tcLookup (tcdName decl)	`thenNF_Tc` \ thing ->
    let
	kind = case thing of
		  AGlobal (ATyCon tc) -> tyConKind tc
		  AGlobal (AClass cl) -> tyConKind (classTyCon cl)
		  AThing kind	      -> kind
		-- For some odd reason, a class doesn't include its kind

	(tyvars_w_kinds, result_kind) = zipFunTys (hsTyVarNames (tyClDeclTyVars decl)) kind
    in
    tcExtendKindEnv tyvars_w_kinds (thing_inside result_kind)
\end{code}



%************************************************************************
%*									*
\subsection{Step 4: Building the tycon/class}
%*									*
%************************************************************************

\begin{code}
buildTyConOrClass 
	:: DynFlags
	-> RecFlag -> NameEnv Kind
	-> FiniteMap TyCon ArgVrcs -> NameEnv TyThingDetails
	-> RenamedTyClDecl -> TyThing

buildTyConOrClass dflags is_rec kenv rec_vrcs rec_details
	          (TySynonym {tcdName = tycon_name, tcdTyVars = tyvar_names})
  = ATyCon tycon
  where
	tycon = mkSynTyCon tycon_name tycon_kind arity tyvars rhs_ty argvrcs
	tycon_kind	    = lookupNameEnv_NF kenv tycon_name
	arity		    = length tyvar_names
	tyvars		    = mkTyClTyVars tycon_kind tyvar_names
	SynTyDetails rhs_ty = lookupNameEnv_NF rec_details tycon_name
        argvrcs		    = lookupWithDefaultFM rec_vrcs bogusVrcs tycon

buildTyConOrClass dflags is_rec kenv rec_vrcs  rec_details
	          (TyData {tcdND = data_or_new, tcdName = tycon_name, 
			   tcdTyVars = tyvar_names, tcdSysNames = sys_names})
  = ATyCon tycon
  where
	tycon = mkAlgTyCon tycon_name tycon_kind tyvars ctxt argvrcs
			   data_cons sel_ids
			   flavour is_rec gen_info

	gen_info | not (dopt Opt_Generics dflags) = Nothing
		 | otherwise = mkTyConGenInfo tycon sys_names

	DataTyDetails ctxt data_cons sel_ids = lookupNameEnv_NF rec_details tycon_name

	tycon_kind = lookupNameEnv_NF kenv tycon_name
	tyvars	   = mkTyClTyVars tycon_kind tyvar_names
        argvrcs	   = lookupWithDefaultFM rec_vrcs bogusVrcs tycon

	-- Watch out!  mkTyConApp asks whether the tycon is a NewType,
	-- so flavour has to be able to answer this question without consulting rec_details
	flavour = case data_or_new of
		    NewType  -> NewTyCon (mkNewTyConRep tycon)
		    DataType | all_nullary data_cons -> EnumTyCon
	 		     | otherwise	     -> DataTyCon

 	all_nullary (DataCons cons) = all (null . dataConOrigArgTys) cons
	all_nullary other	    = False	-- Safe choice for unknown data types
			-- NB (null . dataConOrigArgTys).  It used to say isNullaryDataCon
			-- but that looks at the *representation* arity, and that in turn
			-- depends on deciding whether to unpack the args, and that 
			-- depends on whether it's a data type or a newtype --- so
			-- in the recursive case we can get a loop.  This version is simple!

buildTyConOrClass dflags is_rec kenv rec_vrcs  rec_details
                  (ForeignType {tcdName = tycon_name, tcdExtName = tycon_ext_name})
  = ATyCon (mkForeignTyCon tycon_name tycon_ext_name liftedTypeKind 0 [])

buildTyConOrClass dflags is_rec kenv rec_vrcs  rec_details
                  (ClassDecl {tcdName = class_name, tcdTyVars = tyvar_names,
			      tcdFDs = fundeps, tcdSysNames = name_list} )
  = AClass clas
  where
        (tycon_name, _, _, _) = getClassDeclSysNames name_list
 	clas = mkClass class_name tyvars fds
		       sc_theta sc_sel_ids op_items
		       tycon

	tycon = mkClassTyCon tycon_name class_kind tyvars
                             argvrcs dict_con
			     clas 		-- Yes!  It's a dictionary 
			     flavour
			     is_rec
		-- A class can be recursive, and in the case of newtypes 
		-- this matters.  For example
		-- 	class C a where { op :: C b => a -> b -> Int }
		-- Because C has only one operation, it is represented by
		-- a newtype, and it should be a *recursive* newtype.
		-- [If we don't make it a recursive newtype, we'll expand the
		-- newtype like a synonym, but that will lead toan inifinite type

	ClassDetails sc_theta sc_sel_ids op_items dict_con = lookupNameEnv_NF rec_details class_name

	class_kind = lookupNameEnv_NF kenv class_name
	tyvars	   = mkTyClTyVars class_kind tyvar_names
        argvrcs	   = lookupWithDefaultFM rec_vrcs bogusVrcs tycon

 	flavour = case dataConOrigArgTys dict_con of
			-- The tyvars in the datacon are the same as in the class
		    [rep_ty] -> NewTyCon rep_ty
		    other    -> DataTyCon 

	-- We can find the functional dependencies right away, 
	-- and it is vital to do so. Why?  Because in the next pass
	-- we check for ambiguity in all the type signatures, and we
	-- need the functional dependcies to be done by then
	fds	   = [(map lookup xs, map lookup ys) | (xs,ys) <- fundeps]
	tyvar_env  = mkNameEnv [(varName tv, tv) | tv <- tyvars]
	lookup     = lookupNameEnv_NF tyvar_env

bogusVrcs = panic "Bogus tycon arg variances"
\end{code}

\begin{code}
mkNewTyConRep :: TyCon		-- The original type constructor
	      -> Type		-- Chosen representation type
				-- (guaranteed not to be another newtype)

-- Find the representation type for this newtype TyCon
-- 
-- The non-recursive newtypes are easy, because they look transparent
-- to splitTyConApp_maybe, but recursive ones really are represented as
-- TyConApps (see TypeRep).
-- 
-- The trick is to to deal correctly with recursive newtypes
-- such as	newtype T = MkT T

mkNewTyConRep tc
  = go [] tc
  where
	-- Invariant: tc is a NewTyCon
	-- 	      tcs have been seen before
    go tcs tc 
	| tc `elem` tcs = unitTy
	| otherwise
	= let
	      rep_ty = head (dataConOrigArgTys (head (tyConDataCons tc)))
	  in
	  case splitTyConApp_maybe rep_ty of
			Nothing -> rep_ty 
			Just (tc', tys) | not (isNewTyCon tc') -> rep_ty
				        | otherwise	       -> go1 (tc:tcs) tc' tys

    go1 tcs tc tys = substTyWith (tyConTyVars tc) tys (go tcs tc)
\end{code}

%************************************************************************
%*									*
\subsection{Dependency analysis}
%*									*
%************************************************************************

Dependency analysis
~~~~~~~~~~~~~~~~~~~
\begin{code}
sortByDependency :: [RenamedTyClDecl] -> TcM [SCC RenamedTyClDecl]
sortByDependency decls
  = let		-- CHECK FOR CLASS CYCLES
	cls_sccs   = stronglyConnComp (mapMaybe mkClassEdges tycl_decls)
	cls_cycles = [ decls | CyclicSCC decls <- cls_sccs]
    in
    checkTc (null cls_cycles) (classCycleErr cls_cycles)	`thenTc_`

    let		-- CHECK FOR SYNONYM CYCLES
	syn_sccs   = stronglyConnComp (filter is_syn_decl edges)
	syn_cycles = [ decls | CyclicSCC decls <- syn_sccs]

    in
    checkTc (null syn_cycles) (typeCycleErr syn_cycles)		`thenTc_`

    	-- DO THE MAIN DEPENDENCY ANALYSIS
    let
	decl_sccs  = stronglyConnComp edges
    in
    returnTc decl_sccs
  where
    tycl_decls = filter (not . isIfaceSigDecl) decls
    edges      = map mkEdges tycl_decls
    
    is_syn_decl (d, _, _) = isSynDecl d
\end{code}

Edges in Type/Class decls
~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
tyClDeclFTVs :: RenamedTyClDecl -> [Name]
	-- Find the free non-tyvar vars
tyClDeclFTVs d = foldNameSet add [] (tyClDeclFVs d)
	       where
		 add n fvs | isTyVarName n = fvs
			   | otherwise	   = n : fvs

----------------------------------------------------
-- mk_cls_edges looks only at the context of class decls
-- Its used when we are figuring out if there's a cycle in the
-- superclass hierarchy

mkClassEdges :: RenamedTyClDecl -> Maybe (RenamedTyClDecl, Name, [Name])

mkClassEdges decl@(ClassDecl {tcdCtxt = ctxt, tcdName = name}) = Just (decl, name, [c | HsClassP c _ <- ctxt])
mkClassEdges other_decl				   	       = Nothing

mkEdges :: RenamedTyClDecl -> (RenamedTyClDecl, Name, [Name])
mkEdges decl = (decl, tyClDeclName decl, tyClDeclFTVs decl)
\end{code}


%************************************************************************
%*									*
\subsection{Error management
%*									*
%************************************************************************

\begin{code}
typeCycleErr, classCycleErr :: [[RenamedTyClDecl]] -> Message

typeCycleErr syn_cycles
  = vcat (map (pp_cycle "Cycle in type declarations:") syn_cycles)

classCycleErr cls_cycles
  = vcat (map (pp_cycle "Cycle in class declarations:") cls_cycles)

pp_cycle str decls
  = hang (text str)
	 4 (vcat (map pp_decl decls))
  where
    pp_decl decl
      = hsep [quotes (ppr name), ptext SLIT("at"), ppr (getSrcLoc name)]
     where
        name = tyClDeclName decl

\end{code}
