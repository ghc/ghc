%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcTyClsDecls]{Typecheck type and class declarations}

\begin{code}
module TcTyClsDecls (
	tcTyAndClassDecls
    ) where

#include "HsVersions.h"

import HsSyn		( TyClDecl(..),  
			  ConDecl(..),   Sig(..), HsPred(..), 
			  tyClDeclName, hsTyVarNames, tyClDeclTyVars,
			  isTypeOrClassDecl, isClassDecl, isSynDecl, isClassOpSig
			)
import RnHsSyn		( RenamedTyClDecl, tyClDeclFVs )
import RnEnv		( lookupSysName )
import BasicTypes	( RecFlag(..), NewOrData(..) )
import HscTypes		( implicitTyThings )

import TcRnMonad
import TcEnv		( TcTyThing(..), TyThing(..), TyThingDetails(..),
			  tcExtendKindEnv, tcLookup, tcLookupGlobal, tcExtendGlobalEnv,
			  isLocalThing )
import TcTyDecls	( tcTyDecl, kcConDetails )
import TcClassDcl	( tcClassDecl1 )
import TcInstDcls	( tcAddDeclCtxt )
import TcMonoType	( kcHsTyVars, kcHsType, kcHsLiftedSigType, kcHsContext, mkTyClTyVars )
import TcMType		( newKindVar, zonkKindEnv, checkValidTyCon, checkValidClass )
import TcUnify		( unifyKind )
import TcType		( Type, Kind, TcKind, mkArrowKind, liftedTypeKind, zipFunTys )
import Type		( splitTyConApp_maybe )
import Variance         ( calcTyConArgVrcs )
import Class		( Class, mkClass, classTyCon )
import TyCon		( TyCon, ArgVrcs, AlgTyConFlavour(..), DataConDetails(..), visibleDataCons,
			  tyConKind, tyConTyVars, tyConDataCons, isNewTyCon,
			  mkSynTyCon, mkAlgTyCon, mkClassTyCon, mkForeignTyCon
			)
import TysWiredIn	( unitTy )
import Subst		( substTyWith )
import DataCon		( dataConOrigArgTys )
import Var		( varName )
import OccName		( mkClassTyConOcc )
import FiniteMap
import Digraph		( stronglyConnComp, SCC(..) )
import Name		( Name )
import NameEnv
import NameSet
import Outputable
import Maybes		( mapMaybe, orElse, catMaybes )
\end{code}


%************************************************************************
%*									*
\subsection{Type checking for type and class declarations}
%*									*
%************************************************************************

The main function
~~~~~~~~~~~~~~~~~
\begin{code}
tcTyAndClassDecls :: [RenamedTyClDecl]
		  -> TcM TcGblEnv	-- Returns extended environment

tcTyAndClassDecls decls
  = do { edge_map <- mkEdgeMap tc_decls ;
	 let { edges = mkEdges edge_map tc_decls } ;
	 tcGroups edge_map (stronglyConnComp edges) }
  where
    tc_decls = filter isTypeOrClassDecl decls

tcGroups edge_map [] = getGblEnv

tcGroups edge_map (group:groups)
  = tcGroup edge_map group	`thenM` \ env ->
    setGblEnv env		$
    tcGroups edge_map groups
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
tcGroup :: EdgeMap -> SCC RenamedTyClDecl 
	-> TcM TcGblEnv 	-- Input env extended by types and classes 
				-- and their implicit Ids,DataCons
					
tcGroup edge_map scc
  = 	-- Step 1
    mappM getInitialKind decls 		`thenM` \ initial_kinds ->

	-- Step 2
    tcExtendKindEnv initial_kinds (mappM kcTyClDecl decls)	`thenM_`

	-- Step 3
    zonkKindEnv initial_kinds		`thenM` \ final_kinds ->

	-- Check for loops; if any are found, bale out now
	-- because the compiler itself will loop otherwise!
    checkNoErrs (checkLoops edge_map scc)	`thenM` \ is_rec_tycon ->

	-- Tie the knot
    traceTc (text "starting" <+> ppr final_kinds)		`thenM_`
    fixM ( \ ~(rec_details_list, _, _) ->
		-- Step 4 
 	let
	    kind_env    = mkNameEnv final_kinds
	    rec_details = mkNameEnv rec_details_list

		-- Calculate variances, and feed into buildTyConOrClass
            rec_vrcs = calcTyConArgVrcs [tc | ATyCon tc <- tyclss]

	    build_one = buildTyConOrClass is_rec_tycon kind_env
					  rec_vrcs rec_details
	    tyclss = map build_one decls

	in
		-- Step 5
		-- Extend the environment with the final 
		-- TyCons/Classes and check the decls
	tcExtendGlobalEnv tyclss	$
	mappM tcTyClDecl1 decls		`thenM` \ tycls_details ->

		-- Return results
	getGblEnv				`thenM` \ env ->
	returnM (tycls_details, env, tyclss)
    )						`thenM` \ (_, env, tyclss) ->

	-- Step 7: Check validity
    setGblEnv env 				$

    traceTc (text "ready for validity check")	`thenM_`
    getModule					`thenM` \ mod ->
    mappM_ (checkValidTyCl mod) decls		`thenM_`
    traceTc (text "done")			`thenM_`
   
    let		-- Add the tycons that come from the classes
		-- We want them in the environment because 
		-- they are mentioned in interface files
	implicit_things = implicitTyThings tyclss
    in
    traceTc ((text "Adding" <+> ppr tyclss) $$ (text "and" <+> ppr implicit_things)) 	`thenM_`
    tcExtendGlobalEnv implicit_things getGblEnv

  where
    decls = case scc of
		AcyclicSCC decl -> [decl]
		CyclicSCC decls -> decls

tcTyClDecl1 decl
  | isClassDecl decl = tcAddDeclCtxt decl (tcClassDecl1 decl)
  | otherwise	     = tcAddDeclCtxt decl (tcTyDecl     decl)

-- We do the validity check over declarations, rather than TyThings
-- only so that we can add a nice context with tcAddDeclCtxt
checkValidTyCl this_mod decl
  = tcLookupGlobal (tcdName decl)	`thenM` \ thing ->
    if not (isLocalThing this_mod thing) then
	-- Don't bother to check validity for non-local things
	returnM ()
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
getInitialKind :: RenamedTyClDecl -> TcM (Name, TcKind)
getInitialKind decl
 = kcHsTyVars (tyClDeclTyVars decl)	`thenM` \ arg_kinds ->
   newKindVar				`thenM` \ result_kind  ->
   returnM (tcdName decl, mk_kind arg_kinds result_kind)

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
    kcHsType rhs		`thenM` \ rhs_kind ->
    unifyKind result_kind rhs_kind

kcTyClDecl (ForeignType {}) = returnM ()

kcTyClDecl decl@(TyData {tcdND = new_or_data, tcdCtxt = context, tcdCons = con_decls})
  = kcTyClDeclBody decl			$ \ result_kind ->
    kcHsContext context			`thenM_` 
    mappM_ kc_con_decl (visibleDataCons con_decls)
  where
    kc_con_decl (ConDecl _ ex_tvs ex_ctxt details loc)
      = kcHsTyVars ex_tvs		`thenM` \ kind_env ->
	tcExtendKindEnv kind_env	$
	kcConDetails new_or_data ex_ctxt details

kcTyClDecl decl@(ClassDecl {tcdCtxt = context,  tcdSigs = class_sigs})
  = kcTyClDeclBody decl		$ \ result_kind ->
    kcHsContext context		`thenM_`
    mappM_ kc_sig (filter isClassOpSig class_sigs)
  where
    kc_sig (ClassOpSig _ _ op_ty loc) = kcHsLiftedSigType op_ty

kcTyClDeclBody :: RenamedTyClDecl -> (Kind -> TcM a) -> TcM a
-- Extend the env with bindings for the tyvars, taken from
-- the kind of the tycon/class.  Give it to the thing inside, and 
-- check the result kind matches
kcTyClDeclBody decl thing_inside
  = tcAddDeclCtxt decl		$
    tcLookup (tcdName decl)	`thenM` \ thing ->
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
	:: (Name -> AlgTyConFlavour -> RecFlag)	-- Whether it's recursive
	-> NameEnv Kind
	-> FiniteMap TyCon ArgVrcs -> NameEnv TyThingDetails
	-> RenamedTyClDecl -> TyThing

buildTyConOrClass rec_tycon kenv rec_vrcs rec_details
    (TySynonym {tcdName = tycon_name, tcdTyVars = tyvar_names})
  = ATyCon tycon
  where
	tycon = mkSynTyCon tycon_name tycon_kind arity tyvars rhs_ty argvrcs
	tycon_kind	    = lookupNameEnv_NF kenv tycon_name
	arity		    = length tyvar_names
	tyvars		    = mkTyClTyVars tycon_kind tyvar_names
	SynTyDetails rhs_ty = lookupNameEnv_NF rec_details tycon_name
        argvrcs		    = lookupWithDefaultFM rec_vrcs bogusVrcs tycon

buildTyConOrClass rec_tycon kenv rec_vrcs rec_details
    (TyData {tcdND = data_or_new, tcdName = tycon_name, 
	     tcdTyVars = tyvar_names})
  = ATyCon tycon
  where
	tycon = mkAlgTyCon tycon_name tycon_kind tyvars ctxt argvrcs
			   data_cons sel_ids flavour 
			   (rec_tycon tycon_name flavour) gen_info

	DataTyDetails ctxt data_cons sel_ids gen_info = lookupNameEnv_NF rec_details tycon_name

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

buildTyConOrClass rec_tycon kenv rec_vrcs rec_details
  (ForeignType {tcdName = tycon_name, tcdExtName = tycon_ext_name})
  = ATyCon (mkForeignTyCon tycon_name tycon_ext_name liftedTypeKind 0 [])

buildTyConOrClass rec_tycon kenv rec_vrcs rec_details
  (ClassDecl {tcdName = class_name, tcdTyVars = tyvar_names, tcdFDs = fundeps} )
  = AClass clas
  where
 	clas = mkClass class_name tyvars fds
		       sc_theta sc_sel_ids op_items
		       tycon

	tycon = mkClassTyCon tycon_name class_kind tyvars
                             argvrcs dict_con
			     clas 		-- Yes!  It's a dictionary 
			     flavour
			     (rec_tycon class_name flavour)
		-- A class can be recursive, and in the case of newtypes 
		-- this matters.  For example
		-- 	class C a where { op :: C b => a -> b -> Int }
		-- Because C has only one operation, it is represented by
		-- a newtype, and it should be a *recursive* newtype.
		-- [If we don't make it a recursive newtype, we'll expand the
		-- newtype like a synonym, but that will lead toan inifinite type

	ClassDetails sc_theta sc_sel_ids op_items dict_con tycon_name 
		= lookupNameEnv_NF rec_details class_name

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
-- Remember that the representation type is the ultimate representation
-- type, looking through other newtypes.
-- 
-- The non-recursive newtypes are easy, because they look transparent
-- to splitTyConApp_maybe, but recursive ones really are represented as
-- TyConApps (see TypeRep).
-- 
-- The trick is to to deal correctly with recursive newtypes
-- such as	newtype T = MkT T

-- a newtype with no data constructors -- appears in External Core programs
mkNewTyConRep tc | (null (tyConDataCons tc)) = unitTy
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
checkLoops :: EdgeMap -> SCC RenamedTyClDecl
	   -> TcM (Name -> AlgTyConFlavour -> RecFlag)
-- Check for illegal loops in a single strongly-connected component
--	a) type synonyms
--	b) superclass hierarchy
--
-- Also return a function that says which tycons are recursive.
-- Remember: 
--	a newtype is recursive if it is part of a recursive
--	group consisting only of newtype and synonyms

checkLoops edge_map (AcyclicSCC _)
  = returnM (\ _ _ -> NonRecursive)

checkLoops edge_map (CyclicSCC decls)
  = let		-- CHECK FOR CLASS CYCLES
	cls_edges  = mapMaybe mkClassEdges decls
	cls_cycles = findCycles cls_edges
    in
    mapM_ (cycleErr "class") cls_cycles		`thenM_`

    let		-- CHECK FOR SYNONYM CYCLES
	syn_edges  = mkEdges edge_map (filter isSynDecl decls)
	syn_cycles = findCycles syn_edges
    in
    mapM_ (cycleErr "type synonym") syn_cycles	`thenM_`

    let 	-- CHECK FOR NEWTYPE CYCLES
	newtype_edges  = mkEdges edge_map (filter is_nt_cycle_decl decls)
	newtype_cycles = findCycles newtype_edges
	rec_newtypes   = mkNameSet [tcdName d | ds <- newtype_cycles, d <- ds]

	rec_tycon name (NewTyCon _)
	  | name `elemNameSet` rec_newtypes = Recursive
	  | otherwise			    = NonRecursive
	rec_tycon name other_flavour = Recursive
    in
    returnM rec_tycon

----------------------------------------------------
-- A class with one op and no superclasses, or vice versa,
--		is treated just like a newtype.
-- It's a bit unclean that this test is repeated in buildTyConOrClass
is_nt_cycle_decl (TySynonym {})				     = True
is_nt_cycle_decl (TyData {tcdND = NewType}) 		     = True
is_nt_cycle_decl (ClassDecl {tcdCtxt = ctxt, tcdSigs = sigs}) = length ctxt + length sigs == 1
is_nt_cycle_decl other					     = False

----------------------------------------------------
findCycles edges = [ ds | CyclicSCC ds <- stronglyConnComp edges]

----------------------------------------------------
--		Building edges for SCC analysis
--
-- When building the edges, we treat the 'main name' of the declaration as the
-- key for the node, but when dealing with External Core we may come across 
-- references to one of the implicit names for the declaration.  For example:
--	class Eq a where ....			
--	data :TSig a = :TSig (:TEq a) ....
-- The first decl is sucked in from an interface file; the second
-- is in an External Core file, generated from a class decl for Sig.  
-- We have to recognise that the reference to :TEq represents a 
-- dependency on the class Eq declaration, else the SCC stuff won't work right.
-- 
-- This complication can only happen when consuming an External Core file
-- 
-- Solution: keep an "EdgeMap" (bad name) that maps :TEq -> Eq.
-- Don't worry about data constructors, because we're only building
-- SCCs for type and class declarations here.  So the tiresome mapping
-- is need only to map   [class tycon -> class]

type EdgeMap = NameEnv Name

mkEdgeMap :: [RenamedTyClDecl] -> TcM EdgeMap
mkEdgeMap decls = do { mb_pairs <- mapM mk_mb_pair decls ;
		       return (mkNameEnv (catMaybes mb_pairs)) }
		where
		  mk_mb_pair (ClassDecl { tcdName = cls_name })
			= do { tc_name <- lookupSysName cls_name mkClassTyConOcc ;
			       return (Just (tc_name, cls_name)) }
		  mk_mb_pair other = return Nothing

mkEdges :: EdgeMap -> [RenamedTyClDecl] -> [(RenamedTyClDecl, Name, [Name])]
-- We use the EdgeMap to map any implicit names to 
-- the 'main name' for the declaration
mkEdges edge_map decls 
  = [ (decl, tyClDeclName decl, get_refs decl) | decl <- decls ]
  where
    get_refs decl = [ lookupNameEnv edge_map n `orElse` n 
		    | n <- nameSetToList (tyClDeclFVs decl) ]

----------------------------------------------------
-- mk_cls_edges looks only at the context of class decls
-- Its used when we are figuring out if there's a cycle in the
-- superclass hierarchy

mkClassEdges :: RenamedTyClDecl -> Maybe (RenamedTyClDecl, Name, [Name])
mkClassEdges decl@(ClassDecl {tcdCtxt = ctxt, tcdName = name}) = Just (decl, name, [c | HsClassP c _ <- ctxt])
mkClassEdges other_decl				   	       = Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Error management
%*									*
%************************************************************************

\begin{code}
cycleErr :: String -> [RenamedTyClDecl] -> TcM ()

cycleErr kind_of_decl decls
  = addErrAt loc (ppr_cycle kind_of_decl decls)
  where
    loc = tcdLoc (head decls)

ppr_cycle kind_of_decl decls
  = hang (ptext SLIT("Cycle in") <+> text kind_of_decl <+> ptext SLIT("declarations:"))
	 4 (vcat (map pp_decl decls))
  where
    pp_decl decl = hsep [quotes (ppr (tcdName decl)), 
			 ptext SLIT("at"), ppr (tcdLoc decl)]
\end{code}
