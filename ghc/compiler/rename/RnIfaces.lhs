%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnIfaces]{Cacheing and Renaming of Interfaces}

\begin{code}
module RnIfaces
     (
	getInterfaceExports,
	recordLocalSlurps, 
	mkImportInfo, 

	slurpImpDecls, closeDecls,

	RecompileRequired, outOfDate, upToDate, recompileRequired
       )
where

#include "HsVersions.h"

import CmdLineOpts	( opt_IgnoreIfacePragmas, opt_NoPruneDecls )
import HscTypes
import HsSyn		( HsDecl(..), Sig(..), TyClDecl(..), ConDecl(..), ConDetails(..),
			  InstDecl(..), HsType(..), hsTyVarNames, getBangType
			)
import HsImpExp		( ImportDecl(..) )
import RdrHsSyn		( RdrNameTyClDecl, RdrNameInstDecl, RdrNameRuleDecl )
import RnHsSyn		( RenamedHsDecl, RenamedTyClDecl,
			  extractHsTyNames, extractHsCtxtTyNames, 
			  tyClDeclFVs, ruleDeclFVs, instDeclFVs
			)
import RnHiFiles	( tryLoadInterface, loadHomeInterface, loadInterface, 
			  loadOrphanModules
			)
import RnSource		( rnTyClDecl, rnInstDecl, rnIfaceRuleDecl )
import RnEnv
import RnMonad
import Id		( idType )
import Type		( namesOfType )
import TyCon		( isSynTyCon, getSynTyConDefn )
import Name		( Name {-instance NamedThing-}, nameOccName,
			  nameModule, isLocalName, nameUnique,
			  NamedThing(..)
			 )
import Name 		( elemNameEnv, delFromNameEnv )
import Module		( Module, ModuleEnv, 
			  moduleName, isModuleInThisPackage,
			  ModuleName, WhereFrom(..),
			  emptyModuleEnv, 
			  extendModuleEnv_C, foldModuleEnv, lookupModuleEnv,
			  elemModuleSet, extendModuleSet
			)
import NameSet
import PrelInfo		( wiredInThingEnv, fractionalClassKeys )
import TysWiredIn	( doubleTyCon )
import Maybes		( orElse )
import FiniteMap
import Outputable
import Bag
import Util		( sortLt )
\end{code}


%*********************************************************
%*							*
\subsection{Getting what a module exports}
%*							*
%*********************************************************

@getInterfaceExports@ is called only for directly-imported modules.

\begin{code}
getInterfaceExports :: ModuleName -> WhereFrom -> RnMG (Module, [(ModuleName,Avails)])
getInterfaceExports mod_name from
  = loadInterface doc_str mod_name from	`thenRn` \ iface ->
    returnRn (mi_module iface, mi_exports iface)
  where
      doc_str = sep [ppr mod_name, ptext SLIT("is directly imported")]
\end{code}


%*********************************************************
%*							*
\subsection{Keeping track of what we've slurped, and version numbers}
%*							*
%*********************************************************

mkImportInof figures out what the ``usage information'' for this
moudule is; that is, what it must record in its interface file as the
things it uses.  

We produce a line for every module B below the module, A, currently being
compiled:
	import B <n> ;
to record the fact that A does import B indireclty.  This is used to decide
to look to look for B.hi rather than B.hi-boot when compiling a module that
imports A.  This line says that A imports B, but uses nothing in it.
So we'll get an early bale-out when compiling A if B's version changes.

\begin{code}
mkImportInfo :: ModuleName			-- Name of this module
	     -> [ImportDecl n]			-- The import decls
	     -> RnMG [ImportVersion Name]

mkImportInfo this_mod imports
  = getIfacesRn					`thenRn` \ ifaces ->
    getHomeIfaceTableRn				`thenRn` \ hit -> 
    let
	(imp_pkg_mods, imp_home_names) = iVSlurp ifaces
	pit	 		       = iPIT 	 ifaces

	import_all_mods :: [ModuleName]
		-- Modules where we imported all the names
		-- (apart from hiding some, perhaps)
	import_all_mods = [ m | ImportDecl m _ _ _ imp_list _ <- imports,
			        import_all imp_list ]
		 	where
			  import_all (Just (False, _)) = False	-- Imports are specified explicitly
			  import_all other	       = True	-- Everything is imported

	-- mv_map groups together all the things imported and used
	-- from a particular module in this package
	-- We use a finite map because we want the domain
	mv_map :: ModuleEnv [Name]
	mv_map  = foldNameSet add_mv emptyModuleEnv imp_home_names
        add_mv name mv_map = extendModuleEnv_C add_item mv_map mod [name]
			   where
			     mod = nameModule name
			     add_item names _ = name:names

	-- In our usage list we record
	--	a) Specifically: Detailed version info for imports from modules in this package
	--			 Gotten from iVSlurp plus import_all_mods
	--
	--	b) Everything:   Just the module version for imports from modules in other packages
	--			 Gotten from iVSlurp plus import_all_mods
	--
	--	c) NothingAtAll: The name only of modules, Baz, in this package that are 'below' us, 
	--			 but which we didn't need at all (this is needed only to decide whether
	--			 to open Baz.hi or Baz.hi-boot higher up the tree).
	--			 This happens when a module, Foo, that we explicitly imported has 
	--			 'import Baz' in its interface file, recording that Baz is below
	--			 Foo in the module dependency hierarchy.  We want to propagate this info.
	--			 These modules are in a combination of HIT/PIT and iImpModInfo
	--
	--	d) NothingAtAll: The name only of all orphan modules we know of (this is needed
	--	   		 so that anyone who imports us can find the orphan modules)
	--			 These modules are in a combination of HIT/PIT and iImpModInfo

	import_info0 = foldModuleEnv mk_imp_info  []	       pit
	import_info1 = foldModuleEnv mk_imp_info  import_info0 hit
	import_info  = [ (mod_name, orphans, is_boot, NothingAtAll) 
		       | (mod_name, (orphans, is_boot)) <- fmToList (iImpModInfo ifaces) ] ++ 
		       import_info1
	
	mk_imp_info :: ModIface -> [ImportVersion Name] -> [ImportVersion Name]
	mk_imp_info iface so_far

	  | Just ns <- lookupModuleEnv mv_map mod 	-- Case (a)
	  = go_for_it (Specifically mod_vers maybe_export_vers 
				    (mk_import_items ns) rules_vers)

	  | mod `elemModuleSet` imp_pkg_mods		-- Case (b)
	  = go_for_it (Everything mod_vers)

	  | import_all_mod				-- Case (a) and (b); the import-all part
	  = if is_home_pkg_mod then
		go_for_it (Specifically mod_vers (Just export_vers) [] rules_vers)
	    else
		go_for_it (Everything mod_vers)
		
	  | is_home_pkg_mod || has_orphans		-- Case (c) or (d)
	  = go_for_it NothingAtAll

	  | otherwise = so_far
	  where
	    go_for_it exports = (mod_name, has_orphans, mi_boot iface, exports) : so_far

	    mod		    = mi_module iface
	    mod_name	    = moduleName mod
	    is_home_pkg_mod = isModuleInThisPackage mod
	    version_info    = mi_version iface
	    version_env     = vers_decls   version_info
	    mod_vers	    = vers_module  version_info
	    rules_vers	    = vers_rules   version_info
	    export_vers	    = vers_exports version_info
	    import_all_mod  = mod_name `elem` import_all_mods
	    has_orphans	    = mi_orphan iface
	    
		-- The sort is to put them into canonical order
	    mk_import_items ns = [(n,v) | n <- sortLt lt_occ ns, 
	    		                  let v = lookupNameEnv version_env n `orElse` 
	    			                  pprPanic "mk_whats_imported" (ppr n)
		                 ]
			 where
			   lt_occ n1 n2 = nameOccName n1 < nameOccName n2

	    maybe_export_vers | import_all_mod = Just (vers_exports version_info)
			      | otherwise      = Nothing
    in
    returnRn import_info
\end{code}

%*********************************************************
%*						 	 *
\subsection{Slurping declarations}
%*							 *
%*********************************************************

\begin{code}
-------------------------------------------------------
slurpImpDecls source_fvs
  = traceRn (text "slurpImp" <+> fsep (map ppr (nameSetToList source_fvs))) `thenRn_`

	-- The current slurped-set records all local things
    getSlurped					`thenRn` \ source_binders ->
    slurpSourceRefs source_binders source_fvs	`thenRn` \ (decls, needed) ->

	-- Then get everything else
    closeDecls decls needed


-------------------------------------------------------
slurpSourceRefs :: NameSet			-- Variables defined in source
		-> FreeVars			-- Variables referenced in source
		-> RnMG ([RenamedHsDecl],
			 FreeVars)		-- Un-satisfied needs
-- The declaration (and hence home module) of each gate has
-- already been loaded

slurpSourceRefs source_binders source_fvs
  = go_outer [] 			-- Accumulating decls
	     emptyFVs 			-- Unsatisfied needs
	     emptyFVs			-- Accumulating gates
  	     (nameSetToList source_fvs)	-- Things whose defn hasn't been loaded yet
  where
	-- The outer loop repeatedly slurps the decls for the current gates
	-- and the instance decls 

	-- The outer loop is needed because consider

    go_outer decls fvs all_gates []	
	= returnRn (decls, fvs)

    go_outer decls fvs all_gates refs	-- refs are not necessarily slurped yet
	= traceRn (text "go_outer" <+> ppr refs)		`thenRn_`
	  getImportedInstDecls all_gates			`thenRn` \ inst_decls ->
	  foldlRn go_inner (decls, fvs, emptyFVs) refs		`thenRn` \ (decls1, fvs1, gates1) ->
	  rnIfaceInstDecls decls1 fvs1 gates1 inst_decls	`thenRn` \ (decls2, fvs2, gates2) ->
	  go_outer decls2 fvs2 (all_gates `plusFV` gates2)
			       (nameSetToList (gates2 `minusNameSet` all_gates))
		-- Knock out the all_gates because even if we don't slurp any new
		-- decls we can get some apparently-new gates from wired-in names

    go_inner (decls, fvs, gates) wanted_name
	= importDecl wanted_name 		`thenRn` \ import_result ->
	  case import_result of
	    AlreadySlurped     -> returnRn (decls, fvs, gates)
	    InTypeEnv ty_thing -> returnRn (decls, fvs, gates `plusFV` getWiredInGates ty_thing)
			
	    HereItIs decl -> rnIfaceTyClDecl decl		`thenRn` \ (new_decl, fvs1) ->
			     returnRn (TyClD new_decl : decls, 
				       fvs1 `plusFV` fvs,
			   	       gates `plusFV` getGates source_fvs new_decl)
\end{code}


\begin{code}
-------------------------------------------------------
-- closeDecls keeps going until the free-var set is empty
closeDecls decls needed
  | not (isEmptyFVs needed)
  = slurpDecls decls needed	`thenRn` \ (decls1, needed1) ->
    closeDecls decls1 needed1

  | otherwise
  = getImportedRules 			`thenRn` \ rule_decls ->
    case rule_decls of
	[]    -> returnRn decls	-- No new rules, so we are done
	other -> rnIfaceDecls rnIfaceRuleDecl rule_decls	`thenRn` \ rule_decls' ->
		 let
			rule_fvs = plusFVs (map ruleDeclFVs rule_decls')
		 in
		 traceRn (text "closeRules" <+> ppr rule_decls' $$ fsep (map ppr (nameSetToList rule_fvs)))	`thenRn_`
		 closeDecls (map RuleD rule_decls' ++ decls) rule_fvs

		 

-------------------------------------------------------
-- Augment decls with any decls needed by needed.
-- Return also free vars of the new decls (only)
slurpDecls decls needed
  = go decls emptyFVs (nameSetToList needed) 
  where
    go decls fvs []         = returnRn (decls, fvs)
    go decls fvs (ref:refs) = slurpDecl decls fvs ref	`thenRn` \ (decls1, fvs1) ->
			      go decls1 fvs1 refs

-------------------------------------------------------
slurpDecl decls fvs wanted_name
  = importDecl wanted_name 		`thenRn` \ import_result ->
    case import_result of
	-- Found a declaration... rename it
	HereItIs decl -> rnIfaceTyClDecl decl		`thenRn` \ (new_decl, fvs1) ->
			 returnRn (TyClD new_decl:decls, fvs1 `plusFV` fvs)

	-- No declaration... (wired in thing, or deferred, or already slurped)
	other -> returnRn (decls, fvs)


-------------------------------------------------------
rnIfaceDecls rn decls	   = mapRn (rnIfaceDecl rn) decls
rnIfaceDecl rn (mod, decl) = initIfaceRnMS mod (rn decl)	

rnIfaceInstDecls decls fvs gates inst_decls
  = rnIfaceDecls rnInstDecl inst_decls	`thenRn` \ inst_decls' ->
    returnRn (map InstD inst_decls' ++ decls,
	      fvs `plusFV` plusFVs (map instDeclFVs inst_decls'),
	      gates `plusFV` plusFVs (map getInstDeclGates inst_decls'))

rnIfaceTyClDecl (mod, decl) = initIfaceRnMS mod (rnTyClDecl decl)	`thenRn` \ decl' ->
			      returnRn (decl', tyClDeclFVs decl')
\end{code}


\begin{code}
getSlurped
  = getIfacesRn 	`thenRn` \ ifaces ->
    returnRn (iSlurp ifaces)

recordSlurp ifaces@(Ifaces { iDecls = (decls_map, n_slurped),
			     iSlurp = slurped_names, 
			     iVSlurp = (imp_mods, imp_names) })
	    avail
  = ASSERT2( not (isLocalName (availName avail)), ppr avail )
    ifaces { iDecls = (decls_map', n_slurped+1),
	     iSlurp  = new_slurped_names, 
	     iVSlurp = new_vslurp }
  where
    decls_map' = foldl delFromNameEnv decls_map (availNames avail)
    main_name  = availName avail
    mod	       = nameModule main_name
    new_slurped_names = addAvailToNameSet slurped_names avail
    new_vslurp | isModuleInThisPackage mod = (imp_mods, addOneToNameSet imp_names main_name)
    	       | otherwise		   = (extendModuleSet imp_mods mod, imp_names)

recordLocalSlurps local_avails
  = getIfacesRn 	`thenRn` \ ifaces ->
    let
	new_slurped_names = foldl addAvailToNameSet (iSlurp ifaces) local_avails
    in
    setIfacesRn (ifaces { iSlurp  = new_slurped_names })
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

	a) the source code
	b) the type of an Id that's mentioned in the source code
	   [includes constructors and selectors]
	c) the RHS of a type synonym that is a gate
	d) the superclasses of a class that is a gate
	e) the context of an instance decl that is slurped in

We slurp in an instance decl from the gated instance pool iff
	
	all its gates are either in the gates of the module, 
	or are a previously-loaded class.  

The latter constraint is because there might have been an instance
decl slurped in during an earlier compilation, like this:

	instance Foo a => Baz (Maybe a) where ...

In the module being compiled we might need (Baz (Maybe T)), where T
is defined in this module, and hence we need (Foo T).  So @Foo@ becomes
a gate.  But there's no way to 'see' that, so we simply treat all 
previously-loaded classes as gates.

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
	 -> RenamedTyClDecl
	 -> FreeVars

getGates source_fvs decl 
  = get_gates (\n -> n `elemNameSet` source_fvs) decl

get_gates is_used (IfaceSig _ ty _ _)
  = extractHsTyNames ty

get_gates is_used (ClassDecl ctxt cls tvs _ sigs _ _ _ )
  = (delListFromNameSet (foldr (plusFV . get) (extractHsCtxtTyNames ctxt) sigs)
		        (hsTyVarNames tvs)
     `addOneToNameSet` cls)
    `plusFV` maybe_double
  where
    get (ClassOpSig n _ ty _) 
	| is_used n = extractHsTyNames ty
	| otherwise = emptyFVs

	-- If we load any numeric class that doesn't have
	-- Int as an instance, add Double to the gates. 
	-- This takes account of the fact that Double might be needed for
	-- defaulting, but we don't want to load Double (and all its baggage)
	-- if the more exotic classes aren't used at all.
    maybe_double | nameUnique cls `elem` fractionalClassKeys 
		 = unitFV (getName doubleTyCon)
		 | otherwise
		 = emptyFVs

get_gates is_used (TySynonym tycon tvs ty _)
  = delListFromNameSet (extractHsTyNames ty) (hsTyVarNames tvs)
	-- A type synonym type constructor isn't a "gate" for instance decls

get_gates is_used (TyData _ ctxt tycon tvs cons _ _ _ _ _)
  = delListFromNameSet (foldr (plusFV . get) (extractHsCtxtTyNames ctxt) cons)
		       (hsTyVarNames tvs)
    `addOneToNameSet` tycon
  where
    get (ConDecl n _ tvs ctxt details _)
	| is_used n
		-- If the constructor is method, get fvs from all its fields
	= delListFromNameSet (get_details details `plusFV` 
		  	      extractHsCtxtTyNames ctxt)
			     (hsTyVarNames tvs)
    get (ConDecl n _ tvs ctxt (RecCon fields) _)
		-- Even if the constructor isn't mentioned, the fields
		-- might be, as selectors.  They can't mention existentially
		-- bound tyvars (typechecker checks for that) so no need for 
		-- the deleteListFromNameSet part
	= foldr (plusFV . get_field) emptyFVs fields
	
    get other_con = emptyFVs

    get_details (VanillaCon tys) = plusFVs (map get_bang tys)
    get_details (InfixCon t1 t2) = get_bang t1 `plusFV` get_bang t2
    get_details (RecCon fields)  = plusFVs [get_bang t | (_, t) <- fields]

    get_field (fs,t) | any is_used fs = get_bang t
		     | otherwise      = emptyFVs

    get_bang bty = extractHsTyNames (getBangType bty)
\end{code}

@getWiredInGates@ is just like @getGates@, but it sees a previously-loaded
thing rather than a declaration.

\begin{code}
getWiredInGates :: TyThing -> FreeVars
-- The TyThing is one that we already have in our type environment, either
--	a) because the TyCon or Id is wired in, or
--	b) from a previous compile
-- Either way, we might have instance decls in the (persistent) collection
-- of parsed-but-not-slurped instance decls that should be slurped in.
-- This might be the first module that mentions both the type and the class
-- for that instance decl, even though both the type and the class were
-- mentioned in other modules, and hence are in the type environment

getWiredInGates (AnId the_id) = namesOfType (idType the_id)
getWiredInGates (AClass cl)   = emptyFVs	-- The superclasses must also be previously
						-- loaded, and hence are automatically gates
getWiredInGates (ATyCon tc)
  | isSynTyCon tc = delListFromNameSet (namesOfType ty) (map getName tyvars)
  | otherwise	  = unitFV (getName tc)
  where
    (tyvars,ty)  = getSynTyConDefn tc

getInstDeclGates (InstDecl inst_ty _ _ _ _) = extractHsTyNames inst_ty
\end{code}

\begin{code}
getImportedInstDecls :: NameSet -> RnMG [(Module,RdrNameInstDecl)]
getImportedInstDecls gates
  =    	-- First, load any orphan-instance modules that aren't aready loaded
	-- Orphan-instance modules are recorded in the module dependecnies
    getIfacesRn 					`thenRn` \ ifaces ->
    let
	orphan_mods =
	  [mod | (mod, (True, _)) <- fmToList (iImpModInfo ifaces)]
    in
    loadOrphanModules orphan_mods			`thenRn_` 

	-- Now we're ready to grab the instance declarations
	-- Find the un-gated ones and return them, 
	-- removing them from the bag kept in Ifaces
    getIfacesRn 					`thenRn` \ ifaces ->
    getTypeEnvRn					`thenRn` \ lookup ->
    let
	(decls, new_insts) = selectGated gates lookup (iInsts ifaces)
    in
    setIfacesRn (ifaces { iInsts = new_insts })		`thenRn_`

    traceRn (sep [text "getImportedInstDecls:", 
		  nest 4 (fsep (map ppr gate_list)),
		  text "Slurped" <+> int (length decls) <+> text "instance declarations",
		  nest 4 (vcat (map ppr_brief_inst_decl decls))])	`thenRn_`
    returnRn decls
  where
    gate_list      = nameSetToList gates

ppr_brief_inst_decl (mod, InstDecl inst_ty _ _ _ _)
  = case inst_ty of
	HsForAllTy _ _ tau -> ppr tau
	other		   -> ppr inst_ty

getImportedRules :: RnMG [(Module,RdrNameRuleDecl)]
getImportedRules 
  | opt_IgnoreIfacePragmas = returnRn []
  | otherwise
  = getIfacesRn 	`thenRn` \ ifaces ->
    getTypeEnvRn	`thenRn` \ lookup ->
    let
	gates		   = iSlurp ifaces	-- Anything at all that's been slurped
	rules		   = iRules ifaces
	(decls, new_rules) = selectGated gates lookup rules
    in
    if null decls then
	returnRn []
    else
    setIfacesRn (ifaces { iRules = new_rules })		     `thenRn_`
    traceRn (sep [text "getImportedRules:", 
		  text "Slurped" <+> int (length decls) <+> text "rules"])   `thenRn_`
    returnRn decls

selectGated gates lookup (decl_bag, n_slurped)
	-- Select only those decls whose gates are *all* in 'gates'
	-- or are a class in 'lookup'
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
    available n = n `elemNameSet` gates 
		|| case lookup n of { Just (AClass c) -> True; other -> False }

    select (reqd, decl) (yes, no)
	| all available reqd = (decl:yes, no)
	| otherwise	     = (yes,      (reqd,decl) `consBag` no)
\end{code}


%*********************************************************
%*							*
\subsection{Getting in a declaration}
%*							*
%*********************************************************

\begin{code}
importDecl :: Name -> RnMG ImportDeclResult

data ImportDeclResult
  = AlreadySlurped
  | InTypeEnv TyThing
  | HereItIs (Module, RdrNameTyClDecl)

importDecl name
  = 	-- STEP 1: Check if it was loaded before beginning this module
    if isLocalName name then
	traceRn (text "Already (local)" <+> ppr name) `thenRn_`
	returnRn AlreadySlurped
    else

	-- STEP 2: Check if we've slurped it in while compiling this module
    getIfacesRn				`thenRn` \ ifaces ->
    if name `elemNameSet` iSlurp ifaces then	
	returnRn AlreadySlurped	
    else

	-- STEP 3: Check if it's already in the type environment
    getTypeEnvRn 			`thenRn` \ lookup ->
    case lookup name of {
	Just ty_thing | name `elemNameEnv` wiredInThingEnv
		      -> 	-- When we find a wired-in name we must load its home
				-- module so that we find any instance decls lurking therein
			 loadHomeInterface wi_doc name	`thenRn_`
			 returnRn (InTypeEnv ty_thing)

		      | otherwise
		      -> returnRn (InTypeEnv ty_thing) ;

	Nothing -> 

	-- STEP 4: OK, we have to slurp it in from an interface file
	--	   First load the interface file
    traceRn nd_doc			`thenRn_`
    loadHomeInterface nd_doc name	`thenRn_`
    getIfacesRn				`thenRn` \ ifaces ->

	-- STEP 5: Get the declaration out
    let
	(decls_map, _) = iDecls ifaces
    in
    case lookupNameEnv decls_map name of
      Just (avail,_,decl)
	-> setIfacesRn (recordSlurp ifaces avail)	`thenRn_`
	   returnRn (HereItIs decl)

      Nothing 
	-> addErrRn (getDeclErr name)	`thenRn_` 
	   returnRn AlreadySlurped
    }
  where
    wi_doc = ptext SLIT("need home module for wired in thing") <+> ppr name
    nd_doc = ptext SLIT("need decl for") <+> ppr name

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

recompileRequired :: FilePath		-- Only needed for debug msgs
		  -> Bool 		-- Source unchanged
		  -> ModIface 		-- Old interface
		  -> RnMG RecompileRequired
recompileRequired iface_path source_unchanged iface
  = traceHiDiffsRn (text "Considering whether compilation is required for" <+> text iface_path <> colon)	`thenRn_`

	-- CHECK WHETHER THE SOURCE HAS CHANGED
    if not source_unchanged then
	traceHiDiffsRn (nest 4 (text "Source file changed or recompilation check turned off"))	`thenRn_` 
	returnRn outOfDate
    else

	-- Source code unchanged and no errors yet... carry on 
    checkList [checkModUsage u | u <- mi_usages iface]

checkList :: [RnMG RecompileRequired] -> RnMG RecompileRequired
checkList []		 = returnRn upToDate
checkList (check:checks) = check	`thenRn` \ recompile ->
			   if recompile then 
				returnRn outOfDate
			   else
				checkList checks
\end{code}
	
\begin{code}
checkModUsage :: ImportVersion Name -> RnMG RecompileRequired
-- Given the usage information extracted from the old
-- M.hi file for the module being compiled, figure out
-- whether M needs to be recompiled.

checkModUsage (mod_name, _, _, NothingAtAll)
	-- If CurrentModule.hi contains 
	--	import Foo :: ;
	-- then that simply records that Foo lies below CurrentModule in the
	-- hierarchy, but CurrentModule doesn't depend in any way on Foo.
	-- In this case we don't even want to open Foo's interface.
  = up_to_date (ptext SLIT("Nothing used from:") <+> ppr mod_name)

checkModUsage (mod_name, _, _, whats_imported)
  = tryLoadInterface doc_str mod_name ImportBySystem	`thenRn` \ (iface, maybe_err) ->
    case maybe_err of {
	Just err -> out_of_date (sep [ptext SLIT("Can't find version number for module"), 
				      ppr mod_name]) ;
		-- Couldn't find or parse a module mentioned in the
		-- old interface file.  Don't complain -- it might just be that
		-- the current module doesn't need that import and it's been deleted

	Nothing -> 
    let
	new_vers      = mi_version iface
	new_decl_vers = vers_decls new_vers
    in
    case whats_imported of {	-- NothingAtAll dealt with earlier

      Everything old_mod_vers -> checkModuleVersion old_mod_vers new_vers	`thenRn` \ recompile ->
				 if recompile then
					out_of_date (ptext SLIT("...and I needed the whole module"))
				 else
					returnRn upToDate ;

      Specifically old_mod_vers maybe_old_export_vers old_decl_vers old_rule_vers ->

	-- CHECK MODULE
    checkModuleVersion old_mod_vers new_vers	`thenRn` \ recompile ->
    if not recompile then
	returnRn upToDate
    else
				 
	-- CHECK EXPORT LIST
    if checkExportList maybe_old_export_vers new_vers then
	out_of_date (ptext SLIT("Export list changed"))
    else

	-- CHECK RULES
    if old_rule_vers /= vers_rules new_vers then
	out_of_date (ptext SLIT("Rules changed"))
    else

	-- CHECK ITEMS ONE BY ONE
    checkList [checkEntityUsage new_decl_vers u | u <- old_decl_vers]	`thenRn` \ recompile ->
    if recompile then
	returnRn outOfDate	-- This one failed, so just bail out now
    else
	up_to_date (ptext SLIT("...but the bits I use haven't."))

    }}
  where
    doc_str = sep [ptext SLIT("need version info for"), ppr mod_name]

------------------------
checkModuleVersion old_mod_vers new_vers
  | vers_module new_vers == old_mod_vers
  = up_to_date (ptext SLIT("Module version unchanged"))

  | otherwise
  = out_of_date (ptext SLIT("Module version has changed"))

------------------------
checkExportList Nothing  new_vers = upToDate
checkExportList (Just v) new_vers = v /= vers_exports new_vers

------------------------
checkEntityUsage new_vers (name,old_vers)
  = case lookupNameEnv new_vers name of

	Nothing       -> 	-- We used it before, but it ain't there now
			  out_of_date (sep [ptext SLIT("No longer exported:"), ppr name])

	Just new_vers 	-- It's there, but is it up to date?
	  | new_vers == old_vers -> returnRn upToDate
	  | otherwise	 	 -> out_of_date (sep [ptext SLIT("Out of date:"), ppr name])

up_to_date  msg = traceHiDiffsRn msg `thenRn_` returnRn upToDate
out_of_date msg = traceHiDiffsRn msg `thenRn_` returnRn outOfDate
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
