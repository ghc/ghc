%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
section
\%[RnIfaces]{Cacheing and Renaming of Interfaces}

\begin{code}
module RnIfaces
     (	slurpImpDecls, importSupportingDecls,
	RecompileRequired, outOfDate, upToDate, checkVersions
       )
where

#include "HsVersions.h"

import CmdLineOpts	( DynFlag(..), opt_NoPruneDecls )
import HscTypes
import HsSyn		( HsDecl(..), Sig(..), TyClDecl(..), ConDecl(..), HsConDetails(..),
			  InstDecl(..), HsType(..), hsTyVarNames, getBangType
			)
import RdrHsSyn		( RdrNameTyClDecl, RdrNameInstDecl, RdrNameRuleDecl )
import RnHsSyn		( RenamedHsDecl, RenamedTyClDecl,
			  extractHsTyNames, extractHsCtxtTyNames, 
			  tyClDeclFVs, ruleDeclFVs, impDeclFVs
			)
import RnHiFiles	( loadInterface, loadHomeInterface, loadOrphanModules )
import RnNames		( mkModDeps )
import RnSource		( rnTyClDecl, rnInstDecl, rnIfaceRuleDecl )
import TcEnv		( getInGlobalScope, tcLookupGlobal_maybe )
import TcRnMonad
import Id		( idType, idName, globalIdDetails )
import IdInfo		( GlobalIdDetails(..) )
import TcType		( tyClsNamesOfType, classNamesOfTheta )
import FieldLabel	( fieldLabelTyCon )
import DataCon		( dataConTyCon, dataConWrapId )
import TyCon		( visibleDataCons, isSynTyCon, getSynTyConDefn, tyConClass_maybe, tyConName )
import Class		( className, classSCTheta )
import Name		( Name {-instance NamedThing-}, isWiredInName, nameIsLocalOrFrom, 
			  nameModule, NamedThing(..) )
import NameEnv 		( delFromNameEnv, lookupNameEnv )
import NameSet
import Module		( Module, isHomeModule )
import PrelNames	( hasKey, fractionalClassKey, numClassKey, 
			  integerTyConName, doubleTyConName )
import Outputable
import Bag
import Maybe( fromJust )
\end{code}


%*********************************************************
%*						 	 *
\subsection{Slurping declarations}
%*							 *
%*********************************************************

\begin{code}
-------------------------------------------------------
slurpImpDecls :: FreeVars -> TcRn m [RenamedHsDecl]
slurpImpDecls source_fvs
  = traceRn (text "slurpImp" <+> fsep (map ppr (nameSetToList source_fvs))) `thenM_`

	-- Slurp in things which might be 'gates' for instance
	-- declarations, plus the instance declarations themselves
    slurpSourceRefs source_fvs				`thenM` \ (gate_decls, bndrs) ->

	-- Then get everything else
    let
	needed = foldr (plusFV . impDeclFVs) emptyFVs gate_decls
    in	
    import_supporting_decls (gate_decls, bndrs) needed


-------------------------------------------------------
slurpSourceRefs :: FreeVars			-- Variables referenced in source
		-> TcRn m ([RenamedHsDecl],	-- Needed declarations
			 NameSet)		-- Names bound by those declarations
-- Slurp imported declarations needed directly by the source code;
-- and some of the ones they need.  The goal is to find all the 'gates'
-- for instance declarations.

slurpSourceRefs source_fvs
  = go_outer [] emptyFVs 		-- Accumulating decls
  	     (nameSetToList source_fvs)	-- Things whose defn hasn't been loaded yet
  where
	-- The outer loop repeatedly slurps the decls for the current gates
	-- and the instance decls 

	-- The outer loop is needed because consider
	--	instance Foo a => Baz (Maybe a) where ...
	-- It may be that Baz and Maybe are used in the source module,
	-- but not Foo; so we need to chase Foo too.
	--
	-- We also need to follow superclass refs.  In particular, 'chasing Foo' must
	-- include actually getting in Foo's class decl
	--	class Wib a => Foo a where ..
	-- so that its superclasses are discovered.  The point is that Wib is a gate too.
	-- We do this for tycons too, so that we look through type synonyms.

    go_outer decls bndrs [] = returnM (decls, bndrs)

    go_outer decls bndrs refs		-- 'refs' are not necessarily slurped yet
	= traceRn (text "go_outer" <+> ppr refs)	`thenM_`
	  foldlM go_inner (decls, bndrs, emptyFVs) refs	`thenM` \ (decls1, bndrs1, gates1) ->
	  getImportedInstDecls gates1			`thenM` \ (inst_decls, new_gates) ->
	  rnIfaceDecls rnInstDecl inst_decls		`thenM` \ inst_decls' ->
	  go_outer (map InstD inst_decls' ++ decls1) 
		   bndrs1
		   (nameSetToList (new_gates `plusFV` plusFVs (map getInstDeclGates inst_decls')))
		-- NB: we go round again to fetch the decls for any gates of any decls
		--     we have loaded.  For example, if we mention
		--		print :: Show a => a -> String
		--     then we must load the decl for Show before stopping, to ensure
		--     that instances from its home module are available

    go_inner (decls, bndrs, gates) wanted_name
	= importDecl bndrs wanted_name 		`thenM` \ import_result ->
	  case import_result of
	    AlreadySlurped -> returnM (decls, bndrs, gates)

	    InTypeEnv ty_thing 
		-> returnM (decls, 
			    bndrs `addOneFV` wanted_name,	-- Avoid repeated calls to getWiredInGates
			    gates `plusFV` getWiredInGates ty_thing)

	    HereItIs decl new_bndrs 
		-> rnIfaceDecl rnTyClDecl decl		`thenM` \ new_decl ->
		   returnM (TyClD new_decl : decls, 
			    bndrs `plusFV` new_bndrs,
			    gates `plusFV` getGates source_fvs new_decl)
\end{code}

\begin{code}
-------------------------------------------------------
-- import_supporting_decls keeps going until the free-var set is empty
importSupportingDecls needed
 = import_supporting_decls ([], emptyNameSet) needed

import_supporting_decls 
	:: ([RenamedHsDecl], NameSet) 	-- Some imported decls, with their binders
	-> FreeVars			-- Remaining un-slurped names
	-> TcRn m [RenamedHsDecl]
import_supporting_decls decls needed
  = slurpIfaceDecls decls needed	`thenM` \ (decls1, bndrs1) ->
    getImportedRules bndrs1		`thenM` \ rule_decls ->
    case rule_decls of
	[]    -> returnM decls1	-- No new rules, so we are done
	other -> rnIfaceDecls rnIfaceRuleDecl rule_decls	`thenM` \ rule_decls' ->
		 let
		    rule_fvs = plusFVs (map ruleDeclFVs rule_decls')
		    decls2   = decls1 ++ map RuleD rule_decls'
		 in
		 traceRn (text "closeRules" <+> ppr rule_decls' $$ 
			  fsep (map ppr (nameSetToList rule_fvs)))	`thenM_`
		 import_supporting_decls (decls2, bndrs1) rule_fvs


-------------------------------------------------------
-- Augment decls with any decls needed by needed,
-- and so on transitively
slurpIfaceDecls :: ([RenamedHsDecl], NameSet) 	-- Already slurped
		-> FreeVars 			-- Still needed
		-> TcRn m ([RenamedHsDecl], NameSet)
slurpIfaceDecls (decls, bndrs) needed
  = slurp decls bndrs (nameSetToList needed) 
  where
    slurp decls bndrs [] = returnM (decls, bndrs)
    slurp decls bndrs (n:ns) 
      = importDecl bndrs n 		`thenM` \ import_result ->
	case import_result of
	  HereItIs decl new_bndrs  	-- Found a declaration... rename it
	    -> 	rnIfaceDecl rnTyClDecl decl	`thenM` \ new_decl ->
	       	slurp (TyClD new_decl : decls) 
		      (bndrs `plusFV` new_bndrs)
		      (nameSetToList (tyClDeclFVs new_decl) ++ ns)
  
	  
	  other -> 	-- No declaration... (wired in thing, or deferred, 
			-- 		      or already slurped)
		slurp decls (bndrs `addOneFV` n) ns

-------------------------------------------------------
rnIfaceDecls rn decls	   = mappM (rnIfaceDecl rn) decls
rnIfaceDecl rn (mod, decl) = initRn (InterfaceMode mod) (rn decl)	
\end{code}


\begin{code}
	-- Tiresomely, we must get the "main" name for the 
	-- thing, because that's what VSlurp contains, and what
	-- is recorded in the usage information
get_main_name (AClass cl)   = className cl
get_main_name (ADataCon dc) = tyConName (dataConTyCon dc)
get_main_name (ATyCon tc)
  | Just clas <- tyConClass_maybe tc = get_main_name (AClass clas)
  | otherwise			     = tyConName tc
get_main_name (AnId id)
  = case globalIdDetails id of
	DataConWorkId dc -> get_main_name (ATyCon (dataConTyCon dc))
	DataConWrapId dc -> get_main_name (ATyCon (dataConTyCon dc))
	RecordSelId lbl  -> get_main_name (ATyCon (fieldLabelTyCon lbl))
	GenericOpId tc   -> get_main_name (ATyCon tc)
	ClassOpId cl     -> className cl
	other	         -> idName id


recordUsage :: Name -> TcRn m ()
-- Record that the Name has been used, for 
-- later generation of usage info in the interface file
recordUsage name = updUsages (upd_usg name)

upd_usg name usages
  | isHomeModule mod = addOneToNameSet usages name
  | otherwise        = usages
  where
    mod = nameModule name
\end{code}


%*********************************************************
%*							*
\subsection{Getting in a declaration}
%*							*
%*********************************************************

\begin{code}
importDecl :: NameSet -> Name -> TcRn m ImportDeclResult

data ImportDeclResult
  = AlreadySlurped
  | InTypeEnv TyThing
  | HereItIs (Module, RdrNameTyClDecl) NameSet	
	-- The NameSet is the bunch of names bound by this decl

importDecl already_slurped name
  = 	-- STEP 0: Check if it's from this module
	-- Doing this catches a common case quickly
    getModule				`thenM` \ this_mod ->
    if nameIsLocalOrFrom this_mod name then
	-- Variables defined on the GHCi command line (e.g. let x = 3)
	-- are Internal names (which don't have a Module)
	returnM AlreadySlurped
    else

   	-- STEP 1: Check if we've slurped it in while compiling this module
    if name `elemNameSet` already_slurped then	
	returnM AlreadySlurped	
    else

	-- STEP 2: Check if it's already in the type environment
    tcLookupGlobal_maybe name		`thenM` \ maybe_thing ->
    case maybe_thing of {

      Just ty_thing 
	| isWiredInName name 
	->  -- When we find a wired-in name we must load its home
	    -- module so that we find any instance decls lurking therein
	    loadHomeInterface wi_doc name	`thenM_`
	    returnM (InTypeEnv ty_thing)

	| otherwise
	->  -- We have slurp something that's already in the type environment, 
	    -- that was not slurped in an earlier compilation.
	    -- Must still record it in the Usages info, because that's used to
	    -- generate usage information

	    traceRn (text "not wired in" <+> ppr name)	`thenM_`
	    recordUsage (get_main_name ty_thing)	`thenM_`
	    returnM (InTypeEnv ty_thing) ;

	Nothing -> 

	-- STEP 4: OK, we have to slurp it in from an interface file
	--	   First load the interface file
    traceRn nd_doc			`thenM_`
    loadHomeInterface nd_doc name	`thenM_`

	-- STEP 4: Get the declaration out
    getEps				`thenM` \ eps ->
    let
	(decls_map, n_slurped) = eps_decls eps
    in
    case lookupNameEnv decls_map name of
      Just (avail,_,decl) -> setEps eps'			`thenM_` 
			     recordUsage (availName avail)	`thenM_`
			     returnM (HereItIs decl (mkFVs avail_names))
	where
	   avail_names   = availNames avail
	   new_decls_map = foldl delFromNameEnv decls_map avail_names
	   eps' 	 = eps { eps_decls = (new_decls_map, n_slurped+1) }

      Nothing -> addErr (getDeclErr name)	`thenM_` 
		 returnM AlreadySlurped
    }
  where
    wi_doc = ptext SLIT("need home module for wired in thing") <+> ppr name
    nd_doc = ptext SLIT("need decl for") <+> ppr name

\end{code}


%*********************************************************
%*						 	 *
\subsection{Extracting the `gates'}
%*							 *
%*********************************************************

The gating story
~~~~~~~~~~~~~~~~~
We want to avoid sucking in too many instance declarations.
An instance decl is only useful if the types and classes mentioned in
its 'head' are all available in the program being compiled.  E.g.

	instance (..) => C (T1 a) (T2 b) where ...

is only useful if C, T1 and T2 are all "available".  So we keep
instance decls that have been parsed from .hi files, but not yet
slurped in, in a pool called the 'gated instance pool'.
Each has its set of 'gates': {C, T1, T2} in the above example.

More precisely, the gates of a module are the types and classes 
that are mentioned in:

	a) the source code	[Note: in fact these don't seem
				to be treated as gates, perhaps
				because no imported instance decl
				can mention them; mutter mutter
				recursive modules.]
	b) the type of an Id that's mentioned in the source code
	   [includes constructors and selectors]
	c) the RHS of a type synonym that is a gate
	d) the superclasses of a class that is a gate
	e) the context of an instance decl that is slurped in

We slurp in an instance decl from the gated instance pool iff
	
	all its gates are either in the gates of the module,
	or the gates of a previously-loaded module

The latter constraint is because there might have been an instance
decl slurped in during an earlier compilation, like this:

	instance Foo a => Baz (Maybe a) where ...

In the module being compiled we might need (Baz (Maybe T)), where T is
defined in this module, and hence we need the instance for (Foo T).
So @Foo@ becomes a gate.  But there's no way to 'see' that.  More
generally, types might be involved as well:

	instance Foo2 S a => Baz2 a where ...

Now we must treat S as a gate too, as well as Foo2.  So the solution
we adopt is:

	we simply treat the gates of all previously-loaded 
	modules as gates of this one

So the gates are remembered across invocations of the renamer in the
PersistentRenamerState.  This gloss mainly affects ghc --make and ghc
--interactive.

(We used to use the persistent type environment for this purpose,
but it has too much.  For a start, it contains all tuple types, 
because they are in the wired-in type env!)


Consructors and class operations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we import a declaration like

	data T = T1 Wibble | T2 Wobble

we don't want to treat @Wibble@ and @Wobble@ as gates {\em unless}
@T1@, @T2@ respectively are mentioned by the user program. If only
@T@ is mentioned we want only @T@ to be a gate; that way we don't suck
in useless instance decls for (say) @Eq Wibble@, when they can't
possibly be useful.

And that's just what (b) says: we only treat T1's type as a gate if
T1 is mentioned.  getGates, which deals with decls we are slurping in,
has to be a bit careful, because a mention of T1 will slurp in T's whole
declaration.

-----------------------------
@getGates@ takes a newly imported (and renamed) decl, and the free
vars of the source program, and extracts from the decl the gate names.

\begin{code}
getGates :: FreeVars		-- Things mentioned in the source program
				-- Used for the cunning "constructors and 
				-- class ops" story described 10 lines above.
	 -> RenamedTyClDecl
	 -> FreeVars

getGates source_fvs decl 
  = get_gates (\n -> n `elemNameSet` source_fvs) decl

get_gates is_used (ForeignType {tcdName = tycon}) = unitNameSet tycon
get_gates is_used (IfaceSig    {tcdType = ty})    = extractHsTyNames ty

get_gates is_used (ClassDecl { tcdCtxt = ctxt, tcdName = cls, tcdTyVars = tvs, tcdSigs = sigs})
  = (super_cls_and_sigs `addOneToNameSet` cls) `unionNameSets` 
    implicitClassGates cls
  where
    super_cls_and_sigs = delListFromNameSet (foldr (plusFV . get) (extractHsCtxtTyNames ctxt) sigs)
		        		    (hsTyVarNames tvs)
    get (ClassOpSig n _ ty _) 
	| is_used n = extractHsTyNames ty
	| otherwise = emptyFVs

get_gates is_used (TySynonym {tcdTyVars = tvs, tcdSynRhs = ty})
  = delListFromNameSet (extractHsTyNames ty) (hsTyVarNames tvs)
	-- A type synonym type constructor isn't a "gate" for instance decls

get_gates is_used (TyData {tcdCtxt = ctxt, tcdName = tycon, tcdTyVars = tvs, tcdCons = cons})
  = delListFromNameSet (foldr (plusFV . get) (extractHsCtxtTyNames ctxt) 
					     (visibleDataCons cons))
		       (hsTyVarNames tvs)
    `addOneToNameSet` tycon
  where
    get (ConDecl n tvs ctxt details _)
	| is_used n
		-- If the constructor is method, get fvs from all its fields
	= delListFromNameSet (get_details details `plusFV` 
		  	      extractHsCtxtTyNames ctxt)
			     (hsTyVarNames tvs)
    get (ConDecl n tvs ctxt (RecCon fields) _)
		-- Even if the constructor isn't mentioned, the fields
		-- might be, as selectors.  They can't mention existentially
		-- bound tyvars (typechecker checks for that) so no need for 
		-- the deleteListFromNameSet part
	= foldr (plusFV . get_field) emptyFVs fields
	
    get other_con = emptyFVs

    get_details (PrefixCon tys)  = plusFVs (map get_bang tys)
    get_details (InfixCon t1 t2) = get_bang t1 `plusFV` get_bang t2
    get_details (RecCon fields)  = plusFVs [get_bang t | (_, t) <- fields]

    get_field (f,t) | is_used f = get_bang t
		    | otherwise = emptyFVs

    get_bang bty = extractHsTyNames (getBangType bty)

implicitClassGates :: Name -> FreeVars
implicitClassGates cls
	-- If we load class Num, add Integer to the free gates
	-- This takes account of the fact that Integer might be needed for
	-- defaulting, but we don't want to load Integer (and all its baggage)
	-- if there's no numeric stuff needed.
	-- Similarly for class Fractional and Double
	--
	-- NB: adding T to the gates will force T to be loaded
	--
	-- NB: If we load (say) Floating, we'll end up loading Fractional too,
	--     since Fractional is a superclass of Floating
  | cls `hasKey` numClassKey	    = unitFV integerTyConName
  | cls `hasKey` fractionalClassKey = unitFV doubleTyConName
  | otherwise			    = emptyFVs
\end{code}

@getWiredInGates@ is just like @getGates@, but it sees a previously-loaded
thing rather than a declaration.

\begin{code}
getWiredInGates :: TyThing -> FreeVars
-- The TyThing is one that we already have in our type environment, either
--	a) because the TyCon or Id is wired in, or
--	b) from a previous compile
--
-- Either way, we might have instance decls in the (persistent) collection
-- of parsed-but-not-slurped instance decls that should be slurped in.
-- This might be the first module that mentions both the type and the class
-- for that instance decl, even though both the type and the class were
-- mentioned in other modules, and hence are in the type environment

getWiredInGates (AClass cl)
  = unitFV (getName cl) `plusFV` mkFVs super_classes
  where
    super_classes = classNamesOfTheta (classSCTheta cl)

getWiredInGates (AnId the_id) = tyClsNamesOfType (idType the_id)
getWiredInGates (ADataCon dc) = tyClsNamesOfType (idType (dataConWrapId dc))
	-- Should include classes in the 'stupid context' of the data con?
getWiredInGates (ATyCon tc)
  | isSynTyCon tc = tyClsNamesOfType ty
  | otherwise	  = unitFV (getName tc)
  where
    (_,ty)  = getSynTyConDefn tc

getInstDeclGates (InstDecl inst_ty _ _ _ _) = extractHsTyNames inst_ty
\end{code}

\begin{code}
getImportedInstDecls :: NameSet -> TcRn m ([(Module,RdrNameInstDecl)], NameSet)
	-- Returns the gates that are new since last time
getImportedInstDecls gates
  =    	-- First, load any orphan-instance modules that aren't aready loaded
	-- Orphan-instance modules are recorded in the module dependecnies
    getImports			`thenM` \ imports ->
    getEps 			`thenM` \ eps ->
    let
	old_gates = eps_inst_gates eps
	new_gates = gates `minusNameSet` old_gates
	all_gates = new_gates `unionNameSets` old_gates
	orphan_mods = imp_orphs imports
    in
    loadOrphanModules orphan_mods			`thenM_` 

	-- Now we're ready to grab the instance declarations
	-- Find the un-gated ones and return them, 
	-- removing them from the bag kept in EPS
	-- Don't foget to get the EPS a second time... 
	--	loadOrphanModules may have side-effected it!
    getEps 					`thenM` \ eps ->
    let
	available n        = n `elemNameSet` all_gates 
	(decls, new_insts) = selectGated available (eps_insts eps)
    in
    setEps (eps { eps_insts = new_insts,
		  eps_inst_gates = all_gates })		`thenM_`

    traceRn (sep [text "getImportedInstDecls:", 
		  nest 4 (fsep (map ppr (nameSetToList gates))),
		  nest 4 (fsep (map ppr (nameSetToList all_gates))),
		  nest 4 (fsep (map ppr (nameSetToList new_gates))),
		  text "Slurped" <+> int (length decls) <+> text "instance declarations",
		  nest 4 (vcat (map ppr_brief_inst_decl decls))])	`thenM_`
    returnM (decls, new_gates)

ppr_brief_inst_decl (mod, InstDecl inst_ty _ _ _ _)
  = case inst_ty of
	HsForAllTy _ _ tau -> ppr tau
	other		   -> ppr inst_ty

getImportedRules :: NameSet 	-- Slurped already
		 -> TcRn m [(Module,RdrNameRuleDecl)]
getImportedRules slurped
  = doptM Opt_IgnoreInterfacePragmas 	`thenM` \ ignore_prags ->
    if ignore_prags then returnM [] else -- ...
    getEps		`thenM` \ eps ->
    getInGlobalScope	`thenM` \ in_type_env ->
    let		-- Slurp rules for anything that is slurped, 
		-- either now, or previously
	available n        = n `elemNameSet` slurped || in_type_env n
	(decls, new_rules) = selectGated available (eps_rules eps)
    in
    if null decls then
	returnM []
    else
    setEps (eps { eps_rules = new_rules })		     `thenM_`
    traceRn (sep [text "getImportedRules:", 
		  text "Slurped" <+> int (length decls) <+> text "rules"])   `thenM_`
    returnM decls

selectGated :: (Name->Bool) -> GatedDecls d
	    -> ([(Module,d)], GatedDecls d)
selectGated available (decl_bag, n_slurped)
	-- Select only those decls whose gates are *all* available
#ifdef DEBUG
  | opt_NoPruneDecls	-- Just to try the effect of not gating at all
  = let
	decls = foldrBag (\ (_,d) ds -> d:ds) [] decl_bag	-- Grab them all
    in
    (decls, (emptyBag, n_slurped + length decls))

  | otherwise
#endif
  = case foldrBag select ([], emptyBag) decl_bag of
	(decls, new_bag) -> (decls, (new_bag, n_slurped + length decls))
  where
    select (gate_fn, decl) (yes, no)
	| gate_fn available  = (decl:yes, no)
	| otherwise	     = (yes,      (gate_fn,decl) `consBag` no)
\end{code}


%********************************************************
%*							*
\subsection{Checking usage information}
%*							*
%********************************************************

@recompileRequired@ is called from the HscMain.   It checks whether
a recompilation is required.  It needs access to the persistent state,
finder, etc, because it may have to load lots of interface files to
check their versions.

\begin{code}
type RecompileRequired = Bool
upToDate  = False	-- Recompile not required
outOfDate = True	-- Recompile required

checkVersions :: Bool		-- True <=> source unchanged
	      -> ModIface 	-- Old interface
	      -> TcRn m RecompileRequired
checkVersions source_unchanged iface
  | not source_unchanged
  = returnM outOfDate
  | otherwise
  = traceHiDiffs (text "Considering whether compilation is required for" <+> 
		  ppr (mi_module iface) <> colon)	`thenM_`

	-- Source code unchanged and no errors yet... carry on 
	-- First put the dependent-module info in the envt, just temporarily,
	-- so that when we look for interfaces we look for the right one (.hi or .hi-boot)
	-- It's just temporary because either the usage check will succeed 
	-- (in which case we are done with this module) or it'll fail (in which
	-- case we'll compile the module from scratch anyhow).
    updGblEnv (\ gbl -> gbl { tcg_imports = mod_deps }) (
	checkList [checkModUsage u | u <- mi_usages iface]
    )

  where
	-- This is a bit of a hack really
    mod_deps = emptyImportAvails { imp_dep_mods = mkModDeps (dep_mods (mi_deps iface)) }

checkList :: [TcRn m RecompileRequired] -> TcRn m RecompileRequired
checkList []		 = returnM upToDate
checkList (check:checks) = check	`thenM` \ recompile ->
			   if recompile then 
				returnM outOfDate
			   else
				checkList checks
\end{code}
	
\begin{code}
checkModUsage :: Usage Name -> TcRn m RecompileRequired
-- Given the usage information extracted from the old
-- M.hi file for the module being compiled, figure out
-- whether M needs to be recompiled.

checkModUsage (Usage { usg_name = mod_name, usg_mod = old_mod_vers,
		       usg_rules = old_rule_vers,
		       usg_exports = maybe_old_export_vers, 
		       usg_entities = old_decl_vers })
  = 	-- Load the imported interface is possible
    let
    	doc_str = sep [ptext SLIT("need version info for"), ppr mod_name]
    in
    traceHiDiffs (text "Checking usages for module" <+> ppr mod_name) `thenM_`

    tryM (loadInterface doc_str mod_name ImportBySystem)	`thenM` \ mb_iface ->

    case mb_iface of {
	Left exn ->  (out_of_date (sep [ptext SLIT("Can't find version number for module"), 
				       ppr mod_name]));
		-- Couldn't find or parse a module mentioned in the
		-- old interface file.  Don't complain -- it might just be that
		-- the current module doesn't need that import and it's been deleted

	Right iface -> 
    let
	new_vers      	= mi_version iface
	new_mod_vers    = vers_module  new_vers
	new_decl_vers 	= vers_decls   new_vers
	new_export_vers = vers_exports new_vers
	new_rule_vers   = vers_rules   new_vers
    in
	-- CHECK MODULE
    checkModuleVersion old_mod_vers new_mod_vers	`thenM` \ recompile ->
    if not recompile then
	returnM upToDate
    else
				 
	-- CHECK EXPORT LIST
    if checkExportList maybe_old_export_vers new_export_vers then
	out_of_date_vers (ptext SLIT("  Export list changed"))
		         (fromJust maybe_old_export_vers) 
		         new_export_vers
    else

	-- CHECK RULES
    if old_rule_vers /= new_rule_vers then
	out_of_date_vers (ptext SLIT("  Rules changed")) 
			 old_rule_vers new_rule_vers
    else

	-- CHECK ITEMS ONE BY ONE
    checkList [checkEntityUsage new_decl_vers u | u <- old_decl_vers]	`thenM` \ recompile ->
    if recompile then
	returnM outOfDate	-- This one failed, so just bail out now
    else
	up_to_date (ptext SLIT("  Great!  The bits I use are up to date"))

    }

------------------------
checkModuleVersion old_mod_vers new_mod_vers
  | new_mod_vers == old_mod_vers
  = up_to_date (ptext SLIT("Module version unchanged"))

  | otherwise
  = out_of_date_vers (ptext SLIT("  Module version has changed"))
		     old_mod_vers new_mod_vers

------------------------
checkExportList Nothing  new_vers = upToDate
checkExportList (Just v) new_vers = v /= new_vers

------------------------
checkEntityUsage new_vers (name,old_vers)
  = case lookupNameEnv new_vers name of

	Nothing       -> 	-- We used it before, but it ain't there now
			  out_of_date (sep [ptext SLIT("No longer exported:"), ppr name])

	Just new_vers 	-- It's there, but is it up to date?
	  | new_vers == old_vers -> traceHiDiffs (text "  Up to date" <+> ppr name <+> parens (ppr new_vers)) `thenM_`
			  	    returnM upToDate
	  | otherwise	 	 -> out_of_date_vers (ptext SLIT("  Out of date:") <+> ppr name)
						     old_vers new_vers

up_to_date  msg = traceHiDiffs msg `thenM_` returnM upToDate
out_of_date msg = traceHiDiffs msg `thenM_` returnM outOfDate
out_of_date_vers msg old_vers new_vers 
  = out_of_date (hsep [msg, ppr old_vers, ptext SLIT("->"), ppr new_vers])
\end{code}


%*********************************************************
%*						 	 *
\subsection{Errors}
%*							 *
%*********************************************************

\begin{code}
getDeclErr name
  = vcat [ptext SLIT("Failed to find interface decl for") <+> quotes (ppr name),
	  ptext SLIT("from module") <+> quotes (ppr (nameModule name))
	 ]
\end{code}
