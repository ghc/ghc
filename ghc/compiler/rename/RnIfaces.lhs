%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnIfaces]{Cacheing and Renaming of Interfaces}

\begin{code}
module RnIfaces (
	getInterfaceExports, 
	getImportedInstDecls, getImportedRules,
	lookupFixity, loadHomeInterface,
	importDecl, recordSlurp,
	getImportVersions, getSlurped,

	checkUpToDate,

	getDeclBinders, getDeclSysBinders,
	removeContext	 	-- removeContext probably belongs somewhere else
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_NoPruneDecls, opt_IgnoreIfacePragmas )
import HsSyn		( HsDecl(..), TyClDecl(..), InstDecl(..), IfaceSig(..), 
			  HsType(..), ConDecl(..), IE(..), ConDetails(..), Sig(..),
			  FixitySig(..), RuleDecl(..),
			  isClassOpSig
			)
import BasicTypes	( Version, NewOrData(..), defaultFixity )
import RdrHsSyn		( RdrNameHsDecl, RdrNameInstDecl, RdrNameTyClDecl, RdrNameRuleDecl,
			  extractHsTyRdrNames
			)
import RnEnv		( mkImportedGlobalName, newImportedBinder, mkImportedGlobalFromRdrName,
			  lookupOccRn,
			  pprAvail,
			  availName, availNames, addAvailToNameSet,
			  FreeVars, emptyFVs
			)
import RnMonad
import RnHsSyn          ( RenamedHsDecl )
import ParseIface	( parseIface, IfaceStuff(..) )

import FiniteMap	( FiniteMap, sizeFM, emptyFM, delFromFM,
			  lookupFM, addToFM, addToFM_C, addListToFM, 
			  fmToList, elemFM, foldFM
			)
import Name		( Name {-instance NamedThing-},
			  nameModule, isLocallyDefined,
			  isWiredInName, nameUnique, NamedThing(..)
			 )
import Module		( Module, moduleString, pprModule,
			  mkVanillaModule, pprModuleName,
			  moduleUserString, moduleName, isLibModule,
			  ModuleName, WhereFrom(..),
			)
import RdrName		( RdrName, rdrNameOcc )
import NameSet
import Var		( Id )
import SrcLoc		( mkSrcLoc, SrcLoc )
import PrelMods		( pREL_GHC )
import PrelInfo		( cCallishTyKeys, thinAirModules )
import Bag
import Maybes		( MaybeErr(..), maybeToBool, orElse )
import ListSetOps	( unionLists )
import Outputable
import Unique		( Unique )
import StringBuffer     ( StringBuffer, hGetStringBuffer )
import FastString	( mkFastString )
import Lex
import Outputable

import IO	( isDoesNotExistError )
import List	( nub )
\end{code}


%*********************************************************
%*							*
\subsection{Loading a new interface file}
%*							*
%*********************************************************

\begin{code}
loadHomeInterface :: SDoc -> Name -> RnM d (Module, Ifaces)
loadHomeInterface doc_str name
  = loadInterface doc_str (moduleName (nameModule name)) ImportBySystem

loadInterface :: SDoc -> ModuleName -> WhereFrom -> RnM d (Module, Ifaces)
loadInterface doc_str mod_name from
 = getIfacesRn 			`thenRn` \ ifaces ->
   let
	mod_map  = iImpModInfo ifaces
	mod_info = lookupFM mod_map mod_name
	in_map   = maybeToBool mod_info
   in

	-- Issue a warning for a redundant {- SOURCE -} import
	-- It's redundant if the moduld is in the iImpModInfo at all,
	-- because we arrange to read all the ordinary imports before 
	-- any of the {- SOURCE -} imports
   warnCheckRn	(not (in_map && case from of {ImportByUserSource -> True; other -> False}))
		(warnRedundantSourceImport mod_name)	`thenRn_`

	-- CHECK WHETHER WE HAVE IT ALREADY
   case mod_info of {
	Just (_, _, Just (load_mod, _, _))
		-> 	-- We're read it already so don't re-read it
		    returnRn (load_mod, ifaces) ;

	mod_map_result ->

	-- READ THE MODULE IN
   findAndReadIface doc_str mod_name from in_map
   `thenRn` \ (hi_boot_read, read_result) ->
   case read_result of {
	Nothing -> 	-- Not found, so add an empty export env to the Ifaces map
			-- so that we don't look again
	   let
		mod         = mkVanillaModule mod_name
		new_mod_map = addToFM mod_map mod_name (0, False, Just (mod, False, []))
		new_ifaces  = ifaces { iImpModInfo = new_mod_map }
	   in
	   setIfacesRn new_ifaces		`thenRn_`
	   failWithRn (mod, new_ifaces) (noIfaceErr mod hi_boot_read) ;

	-- Found and parsed!
	Just (mod, iface) ->

	-- LOAD IT INTO Ifaces

	-- NB: *first* we do loadDecl, so that the provenance of all the locally-defined
	---    names is done correctly (notably, whether this is an .hi file or .hi-boot file).
	--     If we do loadExport first the wrong info gets into the cache (unless we
	-- 	explicitly tag each export which seems a bit of a bore)

    getModuleRn 		`thenRn` \ this_mod_nm ->
    let
	rd_decls = pi_decls iface
    in
    foldlRn (loadDecl mod)	     (iDecls ifaces) rd_decls 		`thenRn` \ new_decls ->
    foldlRn (loadInstDecl mod)	     (iInsts ifaces) (pi_insts iface)	`thenRn` \ new_insts ->
    foldlRn (loadRule mod)	     (iRules ifaces) (pi_rules iface)	`thenRn` \ new_rules -> 
    foldlRn (loadFixDecl mod_name)   (iFixes ifaces) rd_decls  		`thenRn` \ new_fixities ->
    mapRn   (loadExport this_mod_nm) (pi_exports iface)			`thenRn` \ avails_s ->
    let
	-- For an explicit user import, add to mod_map info about
	-- the things the imported module depends on, extracted
	-- from its usage info.
	mod_map1 = case from of
			ImportByUser -> addModDeps mod mod_map (pi_usages iface)
			other        -> mod_map

	-- Now add info about this module
	mod_map2    = addToFM mod_map1 mod_name mod_details
	mod_details = (pi_mod iface, pi_orphan iface, Just (mod, hi_boot_read, concat avails_s))

	new_ifaces = ifaces { iImpModInfo = mod_map2,
			      iDecls      = new_decls,
			      iFixes      = new_fixities,
			      iRules	  = new_rules,
			      iInsts      = new_insts }
    in
    setIfacesRn new_ifaces		`thenRn_`
    returnRn (mod, new_ifaces)
    }}

addModDeps :: Module -> ImportedModuleInfo
	   -> [ImportVersion a] -> ImportedModuleInfo
addModDeps mod mod_deps new_deps
  = foldr add mod_deps new_deps
  where
    is_lib = isLibModule mod	-- Don't record dependencies when importing a library module
    add (imp_mod, version, has_orphans, _) deps
	| is_lib && not has_orphans = deps
	| otherwise  =  addToFM_C combine deps imp_mod (version, has_orphans, Nothing)
	-- Record dependencies for modules that are
	--	either are dependent via a non-library module
	--	or contain orphan rules or instance decls

	-- Don't ditch a module that's already loaded!!
    combine old@(_, _, Just _)  new = old
    combine old@(_, _, Nothing) new = new

loadExport :: ModuleName -> ExportItem -> RnM d [AvailInfo]
loadExport this_mod (mod, entities)
  | mod == this_mod = returnRn []
	-- If the module exports anything defined in this module, just ignore it.
	-- Reason: otherwise it looks as if there are two local definition sites
	-- for the thing, and an error gets reported.  Easiest thing is just to
	-- filter them out up front. This situation only arises if a module
	-- imports itself, or another module that imported it.  (Necessarily,
	-- this invoves a loop.)  Consequence: if you say
	--	module A where
	--	   import B( AType )
	--	   type AType = ...
	--
	--	module B( AType ) where
	--	   import {-# SOURCE #-} A( AType )
	--
	-- then you'll get a 'B does not export AType' message.  A bit bogus
	-- but it's a bogus thing to do!

  | otherwise
  = mapRn (load_entity mod) entities
  where
    new_name mod occ = mkImportedGlobalName mod occ

    load_entity mod (Avail occ)
      =	new_name mod occ	`thenRn` \ name ->
	returnRn (Avail name)
    load_entity mod (AvailTC occ occs)
      =	new_name mod occ	      `thenRn` \ name ->
        mapRn (new_name mod) occs     `thenRn` \ names ->
        returnRn (AvailTC name names)


loadFixDecl :: ModuleName -> FixityEnv
	    -> (Version, RdrNameHsDecl)
	    -> RnM d FixityEnv
loadFixDecl mod_name fixity_env (version, FixD sig@(FixitySig rdr_name fixity loc))
  = 	-- Ignore the version; when the fixity changes the version of
	-- its 'host' entity changes, so we don't need a separate version
	-- number for fixities
    mkImportedGlobalName mod_name (rdrNameOcc rdr_name) 	`thenRn` \ name ->
    let
	new_fixity_env = addToNameEnv fixity_env name (FixitySig name fixity loc)
    in
    returnRn new_fixity_env

	-- Ignore the other sorts of decl
loadFixDecl mod_name fixity_env other_decl = returnRn fixity_env

loadDecl :: Module 
	 -> DeclsMap
	 -> (Version, RdrNameHsDecl)
	 -> RnM d DeclsMap

loadDecl mod decls_map (version, decl)
  = getDeclBinders new_name decl	`thenRn` \ maybe_avail ->
    case maybe_avail of {
	Nothing -> returnRn decls_map;	-- No bindings
	Just avail ->

    getDeclSysBinders new_name decl	`thenRn` \ sys_bndrs ->
    let
	main_name     = availName avail
	new_decls_map = foldl add_decl decls_map
				       [ (name, (version, avail, name==main_name, (mod, decl'))) 
				       | name <- sys_bndrs ++ availNames avail]
	add_decl decls_map (name, stuff)
	  = WARN( name `elemNameEnv` decls_map, ppr name )
	    addToNameEnv decls_map name stuff
    in
    returnRn new_decls_map
    }
  where
	-- newImportedBinder puts into the cache the binder with the
	-- module information set correctly.  When the decl is later renamed,
	-- the binding site will thereby get the correct module.
    new_name rdr_name loc = newImportedBinder mod rdr_name

    {-
      If a signature decl is being loaded, and optIgnoreIfacePragmas is on,
      we toss away unfolding information.

      Also, if the signature is loaded from a module we're importing from source,
      we do the same. This is to avoid situations when compiling a pair of mutually
      recursive modules, peering at unfolding info in the interface file of the other, 
      e.g., you compile A, it looks at B's interface file and may as a result change
      its interface file. Hence, B is recompiled, maybe changing its interface file,
      which will the unfolding info used in A to become invalid. Simple way out is to
      just ignore unfolding info.

      [Jan 99: I junked the second test above.  If we're importing from an hi-boot
       file there isn't going to *be* any pragma info.  Maybe the above comment
       dates from a time where we picked up a .hi file first if it existed?]
    -}
    decl' = case decl of
	       SigD (IfaceSig name tp ls loc) | opt_IgnoreIfacePragmas
			 ->  SigD (IfaceSig name tp [] loc)
	       other	 -> decl

loadInstDecl :: Module
	     -> Bag GatedDecl
	     -> RdrNameInstDecl
	     -> RnM d (Bag GatedDecl)
loadInstDecl mod insts decl@(InstDecl inst_ty binds uprags dfun_name src_loc)
  = 
	-- Find out what type constructors and classes are "gates" for the
	-- instance declaration.  If all these "gates" are slurped in then
	-- we should slurp the instance decl too.
	-- 
	-- We *don't* want to count names in the context part as gates, though.
	-- For example:
	--		instance Foo a => Baz (T a) where ...
	--
	-- Here the gates are Baz and T, but *not* Foo.
    let 
	munged_inst_ty = removeContext inst_ty
	free_names     = extractHsTyRdrNames munged_inst_ty
    in
    setModuleRn (moduleName mod) $
    mapRn mkImportedGlobalFromRdrName free_names	`thenRn` \ gate_names ->
    returnRn ((mkNameSet gate_names, (mod, InstD decl)) `consBag` insts)


-- In interface files, the instance decls now look like
--	forall a. Foo a -> Baz (T a)
-- so we have to strip off function argument types as well
-- as the bit before the '=>' (which is always empty in interface files)
removeContext (HsForAllTy tvs cxt ty) = HsForAllTy tvs [] (removeFuns ty)
removeContext ty		      = removeFuns ty

removeFuns (MonoFunTy _ ty) = removeFuns ty
removeFuns ty		    = ty


loadRule :: Module -> Bag GatedDecl 
	 -> RdrNameRuleDecl -> RnM d (Bag GatedDecl)
-- "Gate" the rule simply by whether the rule variable is
-- needed.  We can refine this later.
loadRule mod rules decl@(IfaceRuleDecl var body src_loc)
  = setModuleRn (moduleName mod) $
    mkImportedGlobalFromRdrName var		`thenRn` \ var_name ->
    returnRn ((unitNameSet var_name, (mod, RuleD decl)) `consBag` rules)
\end{code}


%********************************************************
%*							*
\subsection{Loading usage information}
%*							*
%********************************************************

\begin{code}
checkUpToDate :: ModuleName -> RnMG Bool		-- True <=> no need to recompile
checkUpToDate mod_name
  = getIfacesRn					`thenRn` \ ifaces ->
    findAndReadIface doc_str mod_name 
		     ImportByUser
		     (error "checkUpToDate")	`thenRn` \ (_, read_result) ->

	-- CHECK WHETHER WE HAVE IT ALREADY
    case read_result of
	Nothing -> 	-- Old interface file not found, so we'd better bail out
		    traceRn (sep [ptext SLIT("Didnt find old iface"), 
				  pprModuleName mod_name])	`thenRn_`
		    returnRn False

	Just (_, iface)
		-> 	-- Found it, so now check it
		    checkModUsage (pi_usages iface)
  where
	-- Only look in current directory, with suffix .hi
    doc_str = sep [ptext SLIT("need usage info from"), pprModuleName mod_name]

checkModUsage [] = returnRn True		-- Yes!  Everything is up to date!

checkModUsage ((mod_name, old_mod_vers, _, whats_imported) : rest)
  = loadInterface doc_str mod_name ImportBySystem	`thenRn` \ (mod, ifaces) ->
    let
	maybe_mod_vers = case lookupFM (iImpModInfo ifaces) mod_name of
			   Just (version, _, Just (_, _, _)) -> Just version
			   other			     -> Nothing
    in
    case maybe_mod_vers of {
	Nothing -> 	-- If we can't find a version number for the old module then
			-- bail out saying things aren't up to date
		traceRn (sep [ptext SLIT("Can't find version number for module"), 
			      pprModuleName mod_name])
		`thenRn_` returnRn False ;

	Just new_mod_vers ->

	-- If the module version hasn't changed, just move on
    if new_mod_vers == old_mod_vers then
	traceRn (sep [ptext SLIT("Module version unchanged:"), pprModuleName mod_name])
	`thenRn_` checkModUsage rest
    else
    traceRn (sep [ptext SLIT("Module version has changed:"), pprModuleName mod_name])
    `thenRn_`
	-- Module version changed, so check entities inside

	-- If the usage info wants to say "I imported everything from this module"
	--     it does so by making whats_imported equal to Everything
	-- In that case, we must recompile
    case whats_imported of {
      Everything -> traceRn (ptext SLIT("...and I needed the whole module"))	`thenRn_`
		    returnRn False;		   -- Bale out

      Specifically old_local_vers ->

	-- Non-empty usage list, so check item by item
    checkEntityUsage mod_name (iDecls ifaces) old_local_vers	`thenRn` \ up_to_date ->
    if up_to_date then
	traceRn (ptext SLIT("...but the bits I use haven't."))	`thenRn_`
	checkModUsage rest	-- This one's ok, so check the rest
    else
	returnRn False		-- This one failed, so just bail out now
    }}
  where
    doc_str = sep [ptext SLIT("need version info for"), pprModuleName mod_name]


checkEntityUsage mod decls [] 
  = returnRn True	-- Yes!  All up to date!

checkEntityUsage mod decls ((occ_name,old_vers) : rest)
  = mkImportedGlobalName mod occ_name 	`thenRn` \ name ->
    case lookupNameEnv decls name of

	Nothing       -> 	-- We used it before, but it ain't there now
			  putDocRn (sep [ptext SLIT("No longer exported:"), ppr name])
			  `thenRn_` returnRn False

	Just (new_vers,_,_,_) 	-- It's there, but is it up to date?
		| new_vers == old_vers
			-- Up to date, so check the rest
		-> checkEntityUsage mod decls rest

		| otherwise
			-- Out of date, so bale out
		-> putDocRn (sep [ptext SLIT("Out of date:"), ppr name])  `thenRn_`
		   returnRn False
\end{code}


%*********************************************************
%*							*
\subsection{Getting in a declaration}
%*							*
%*********************************************************

\begin{code}
importDecl :: Name -> RnMG (Maybe (Module, RdrNameHsDecl))
	-- Returns Nothing for 
	--	(a) wired in name
	--	(b) local decl
	--	(c) already slurped

importDecl name
  | isWiredInName name
  = returnRn Nothing
  | otherwise
  = getSlurped 				`thenRn` \ already_slurped ->
    if name `elemNameSet` already_slurped then
	returnRn Nothing	-- Already dealt with
    else
	getModuleRn 		`thenRn` \ this_mod ->
	let
	  mod = moduleName (nameModule name)
	in
	if mod == this_mod then		-- Don't bring in decls from
					-- the renamed module's own interface file
		  addWarnRn (importDeclWarn mod name) `thenRn_`
		  returnRn Nothing
	else
	getNonWiredInDecl name
\end{code}

\begin{code}
getNonWiredInDecl :: Name -> RnMG (Maybe (Module, RdrNameHsDecl))
getNonWiredInDecl needed_name 
  = traceRn doc_str				`thenRn_`
    loadHomeInterface doc_str needed_name	`thenRn` \ (_, ifaces) ->
    case lookupNameEnv (iDecls ifaces) needed_name of

      Just (version,avail,_,decl)
	-> recordSlurp (Just version) avail	`thenRn_`
	   returnRn (Just decl)

      Nothing 	 	-- Can happen legitimately for "Optional" occurrences
	-> addErrRn (getDeclErr needed_name)	`thenRn_` 
	   returnRn Nothing
  where
     doc_str = ptext SLIT("need decl for") <+> ppr needed_name
\end{code}

@getWiredInDecl@ maps a wired-in @Name@ to what it makes available.
It behaves exactly as if the wired in decl were actually in an interface file.
Specifically,
\begin{itemize}
\item	if the wired-in name is a data type constructor or a data constructor, 
	it brings in the type constructor and all the data constructors; and
	marks as ``occurrences'' any free vars of the data con.

\item 	similarly for synonum type constructor

\item 	if the wired-in name is another wired-in Id, it marks as ``occurrences''
	the free vars of the Id's type.

\item	it loads the interface file for the wired-in thing for the
	sole purpose of making sure that its instance declarations are available
\end{itemize}
All this is necessary so that we know all types that are ``in play'', so
that we know just what instances to bring into scope.
	


    
%*********************************************************
%*							*
\subsection{Getting what a module exports}
%*							*
%*********************************************************

@getInterfaceExports@ is called only for directly-imported modules.

\begin{code}
getInterfaceExports :: ModuleName -> WhereFrom -> RnMG (Module, Avails)
getInterfaceExports mod_name from
  = loadInterface doc_str mod_name from	`thenRn` \ (mod, ifaces) ->
    case lookupFM (iImpModInfo ifaces) mod_name of
	Nothing -> -- Not there; it must be that the interface file wasn't found;
		   -- the error will have been reported already.
		   -- (Actually loadInterface should put the empty export env in there
		   --  anyway, but this does no harm.)
		   returnRn (mod, [])

	Just (_, _, Just (mod, _, avails)) -> returnRn (mod, avails)
  where
    doc_str = sep [pprModuleName mod_name, ptext SLIT("is directly imported")]
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations are handled specially}
%*							*
%*********************************************************

\begin{code}
getImportedInstDecls :: NameSet -> RnMG [(Module,RdrNameHsDecl)]
getImportedInstDecls gates
  = 	-- First load any orphan-instance modules that aren't aready loaded
	-- Orphan-instance modules are recorded in the module dependecnies
    getIfacesRn 						`thenRn` \ ifaces ->
    let
	orphan_mods =
	  [mod | (mod, (_, True, Nothing)) <- fmToList (iImpModInfo ifaces)]
    in
    traceRn (text "Loading orphan modules" <+> fsep (map pprModuleName orphan_mods))
    `thenRn_` mapRn_ load_it orphan_mods 	`thenRn_`

	-- Now we're ready to grab the instance declarations
	-- Find the un-gated ones and return them, 
	-- removing them from the bag kept in Ifaces
    getIfacesRn 						`thenRn` \ ifaces ->
    let
	(decls, new_insts) = selectGated gates (iInsts ifaces)
    in
    setIfacesRn (ifaces { iInsts = new_insts })		`thenRn_`

    traceRn (sep [text "getImportedInstDecls:", 
		  nest 4 (fsep (map ppr (nameSetToList gates))),
		  text "Slurped" <+> int (length decls)
			         <+> text "instance declarations"]) `thenRn_`
    returnRn decls
  where
    load_it mod = loadInterface (doc_str mod) mod ImportBySystem
    doc_str mod = sep [pprModuleName mod, ptext SLIT("is a orphan-instance module")]

getImportedRules :: RnMG [(Module,RdrNameHsDecl)]
getImportedRules
  = getIfacesRn 	`thenRn` \ ifaces ->
    let
	gates		   = iSlurp ifaces	-- Anything at all that's been slurped
	(decls, new_rules) = selectGated gates (iRules ifaces)
    in
    setIfacesRn (ifaces { iRules = new_rules })		`thenRn_`
    traceRn (sep [text "getImportedRules:", 
		  text "Slurped" <+> int (length decls) <+> text "rules"])	`thenRn_`
    returnRn decls

selectGated gates decl_bag
#ifdef DEBUG
  | opt_NoPruneDecls	-- Just to try the effect of not gating at all
  = (foldrBag (\ (_,d) ds -> d:ds) [] decl_bag, emptyBag)	-- Grab them all

  | otherwise
#endif
  = foldrBag select ([], emptyBag) decl_bag
  where
    select (reqd, decl) (yes, no)
	| isEmptyNameSet (reqd `minusNameSet` gates) = (decl:yes, no)
	| otherwise				     = (yes,      (reqd,decl) `consBag` no)

lookupFixity :: Name -> RnMS Fixity
lookupFixity name
  | isLocallyDefined name
  = getFixityEnv			`thenRn` \ local_fix_env ->
    case lookupNameEnv local_fix_env name of 
	Just (FixitySig _ fix _) -> returnRn fix
	Nothing		  	 -> returnRn defaultFixity

  | otherwise	-- Imported
  = loadHomeInterface doc name		`thenRn` \ (_, ifaces) ->
    case lookupNameEnv (iFixes ifaces) name of
	Just (FixitySig _ fix _) -> returnRn fix 
	Nothing 		 -> returnRn defaultFixity
  where
    doc = ptext SLIT("Checking fixity for") <+> ppr name
\end{code}


%*********************************************************
%*							*
\subsection{Keeping track of what we've slurped, and version numbers}
%*							*
%*********************************************************

getImportVersions figures out
what the ``usage information'' for this moudule is;
that is, what it must record in its interface file as the things it uses.
It records:
\begin{itemize}
\item anything reachable from its body code
\item any module exported with a @module Foo@.
\end{itemize}
%
Why the latter?  Because if @Foo@ changes then this module's export list
will change, so we must recompile this module at least as far as
making a new interface file --- but in practice that means complete
recompilation.

What about this? 
\begin{verbatim}
	module A( f, g ) where	|	module B( f ) where
	  import B( f )		|	  f = h 3
	  g = ...		|	  h = ...
\end{verbatim}
Should we record @B.f@ in @A@'s usages?  In fact we don't.  Certainly, if
anything about @B.f@ changes than anyone who imports @A@ should be recompiled;
they'll get an early exit if they don't use @B.f@.  However, even if @B.f@
doesn't change at all, @B.h@ may do so, and this change may not be reflected
in @f@'s version number.  So there are two things going on when compiling module @A@:
\begin{enumerate}
\item Are @A.o@ and @A.hi@ correct?  Then we can bale out early.
\item Should modules that import @A@ be recompiled?
\end{enumerate}
For (1) it is slightly harmful to record @B.f@ in @A@'s usages,
because a change in @B.f@'s version will provoke full recompilation of @A@,
producing an identical @A.o@,
and @A.hi@ differing only in its usage-version of @B.f@
(which isn't used by any importer).

For (2), because of the tricky @B.h@ question above,
we ensure that @A.hi@ is touched
(even if identical to its previous version)
if A's recompilation was triggered by an imported @.hi@ file date change.
Given that, there's no need to record @B.f@ in @A@'s usages.

On the other hand, if @A@ exports @module B@,
then we {\em do} count @module B@ among @A@'s usages,
because we must recompile @A@ to ensure that @A.hi@ changes appropriately.

\begin{code}
getImportVersions :: ModuleName			-- Name of this module
		  -> Maybe [IE any]		-- Export list for this module
		  -> RnMG (VersionInfo Name)	-- Version info for these names

getImportVersions this_mod exports
  = getIfacesRn					`thenRn` \ ifaces ->
    let
	mod_map   = iImpModInfo ifaces
	imp_names = iVSlurp     ifaces

	-- mv_map groups together all the things imported from a particular module.
	mv_map1, mv_map2 :: FiniteMap ModuleName (WhatsImported Name)

		-- mv_map1 records all the modules that have a "module M"
		-- in this module's export list with an "Everything" 
	mv_map1 = foldr add_mod emptyFM export_mods

		-- mv_map2 adds the version numbers of things exported individually
	mv_map2 = foldr add_mv mv_map1 imp_names

	-- Build the result list by adding info for each module, 
	-- *omitting*	(a) library modules
	--		(b) source-imported modules
	mk_version_info mod_name (version, has_orphans, cts) so_far
	   | omit cts  = so_far	-- Don't record usage info for this module
	   | otherwise = (mod_name, version, has_orphans, whats_imported) : so_far
	   where
	     whats_imported = case lookupFM mv_map2 mod_name of
				Just wi -> wi
				Nothing -> Specifically []

 	omit (Just (mod, boot_import, _)) = isLibModule mod || boot_import
	omit Nothing			  = False
    in
    returnRn (foldFM mk_version_info [] mod_map)
  where
     export_mods = case exports of
			Nothing -> []
			Just es -> [mod | IEModuleContents mod <- es, mod /= this_mod]

     add_mv v@(name, version) mv_map
      = addToFM_C add_item mv_map mod (Specifically [v]) 
	where
	 mod = moduleName (nameModule name)

         add_item Everything        _ = Everything
         add_item (Specifically xs) _ = Specifically (v:xs)

     add_mod mod mv_map = addToFM mv_map mod Everything
\end{code}

\begin{code}
getSlurped
  = getIfacesRn 	`thenRn` \ ifaces ->
    returnRn (iSlurp ifaces)

recordSlurp maybe_version avail
  = getIfacesRn 	`thenRn` \ ifaces@(Ifaces { iSlurp  = slurped_names,
					            iVSlurp = imp_names }) ->
    let
	new_slurped_names = addAvailToNameSet slurped_names avail

	new_imp_names = case maybe_version of
			   Just version	-> (availName avail, version) : imp_names
			   Nothing      -> imp_names
    in
    setIfacesRn (ifaces { iSlurp  = new_slurped_names,
			  iVSlurp = new_imp_names })
\end{code}


%*********************************************************
%*							*
\subsection{Getting binders out of a declaration}
%*							*
%*********************************************************

@getDeclBinders@ returns the names for a @RdrNameHsDecl@.
It's used for both source code (from @availsFromDecl@) and interface files
(from @loadDecl@).

It doesn't deal with source-code specific things: @ValD@, @DefD@.  They
are handled by the sourc-code specific stuff in @RnNames@.

\begin{code}
getDeclBinders :: (RdrName -> SrcLoc -> RnM d Name)	-- New-name function
		-> RdrNameHsDecl
		-> RnM d (Maybe AvailInfo)

getDeclBinders new_name (TyClD (TyData _ _ tycon _ condecls _ _ src_loc))
  = new_name tycon src_loc			`thenRn` \ tycon_name ->
    getConFieldNames new_name condecls		`thenRn` \ sub_names ->
    returnRn (Just (AvailTC tycon_name (tycon_name : nub sub_names)))
	-- The "nub" is because getConFieldNames can legitimately return duplicates,
	-- when a record declaration has the same field in multiple constructors

getDeclBinders new_name (TyClD (TySynonym tycon _ _ src_loc))
  = new_name tycon src_loc		`thenRn` \ tycon_name ->
    returnRn (Just (AvailTC tycon_name [tycon_name]))

getDeclBinders new_name (TyClD (ClassDecl _ cname _ sigs _ _ _ _ _ src_loc))
  = new_name cname src_loc			`thenRn` \ class_name ->

	-- Record the names for the class ops
    let
	-- just want class-op sigs
	op_sigs = filter isClassOpSig sigs
    in
    mapRn (getClassOpNames new_name) op_sigs	`thenRn` \ sub_names ->

    returnRn (Just (AvailTC class_name (class_name : sub_names)))

getDeclBinders new_name (SigD (IfaceSig var ty prags src_loc))
  = new_name var src_loc			`thenRn` \ var_name ->
    returnRn (Just (Avail var_name))

getDeclBinders new_name (FixD _)  = returnRn Nothing
getDeclBinders new_name (ForD _)  = returnRn Nothing
getDeclBinders new_name (DefD _)  = returnRn Nothing
getDeclBinders new_name (InstD _) = returnRn Nothing
getDeclBinders new_name (RuleD _) = returnRn Nothing

----------------
getConFieldNames new_name (ConDecl con _ _ (RecCon fielddecls) src_loc : rest)
  = mapRn (\n -> new_name n src_loc) (con:fields)	`thenRn` \ cfs ->
    getConFieldNames new_name rest			`thenRn` \ ns  -> 
    returnRn (cfs ++ ns)
  where
    fields = concat (map fst fielddecls)

getConFieldNames new_name (ConDecl con _ _ condecl src_loc : rest)
  = new_name con src_loc		`thenRn` \ n ->
    (case condecl of
      NewCon _ (Just f) -> 
        new_name f src_loc `thenRn` \ new_f ->
	returnRn [n,new_f]
      _ -> returnRn [n])		`thenRn` \ nn ->
    getConFieldNames new_name rest	`thenRn` \ ns -> 
    returnRn (nn ++ ns)

getConFieldNames new_name [] = returnRn []

getClassOpNames new_name (ClassOpSig op _ _ src_loc) = new_name op src_loc
\end{code}

@getDeclSysBinders@ gets the implicit binders introduced by a decl.
A the moment that's just the tycon and datacon that come with a class decl.
They aren't returned by @getDeclBinders@ because they aren't in scope;
but they {\em should} be put into the @DeclsMap@ of this module.

Note that this excludes the default-method names of a class decl,
and the dict fun of an instance decl, because both of these have 
bindings of their own elsewhere.

\begin{code}
getDeclSysBinders new_name (TyClD (ClassDecl _ cname _ sigs _ _ tname dname snames src_loc))
  = new_name dname src_loc		    	    	`thenRn` \ datacon_name ->
    new_name tname src_loc		        	`thenRn` \ tycon_name ->
    sequenceRn [new_name n src_loc | n <- snames]	`thenRn` \ scsel_names ->
    returnRn (tycon_name : datacon_name : scsel_names)

getDeclSysBinders new_name other_decl
  = returnRn []
\end{code}

%*********************************************************
%*							*
\subsection{Reading an interface file}
%*							*
%*********************************************************

\begin{code}
findAndReadIface :: SDoc -> ModuleName -> WhereFrom 
		 -> Bool	-- Only relevant for SystemImport
				-- True  <=> Look for a .hi file
				-- False <=> Look for .hi-boot file unless there's
				--	     a library .hi file
		 -> RnM d (Bool, Maybe (Module, ParsedIface))
	-- Bool is True if the interface actually read was a .hi-boot one
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 

findAndReadIface doc_str mod_name from hi_file
  = traceRn trace_msg			`thenRn_`
      -- we keep two maps for interface files,
      -- one for 'normal' ones, the other for .hi-boot files,
      -- hence the need to signal which kind we're interested.

    getHiMaps			`thenRn` \ hi_maps ->
	
    case find_path from hi_maps of
         -- Found the file
       (hi_boot, Just (fpath, mod)) -> traceRn (ptext SLIT("...reading from") <+> text fpath)
				       `thenRn_`
				       readIface mod fpath	`thenRn` \ result ->
				       returnRn (hi_boot, result)
       (hi_boot, Nothing)           -> traceRn (ptext SLIT("...not found"))	`thenRn_`
				       returnRn (hi_boot, Nothing)
  where
    find_path ImportByUser       (hi_map, _)     = (False, lookupFM hi_map mod_name)
    find_path ImportByUserSource (_, hiboot_map) = (True,  lookupFM hiboot_map mod_name)

    find_path ImportBySystem     (hi_map, hiboot_map)
      | hi_file
      =		-- If the module we seek is in our dependent set, 
		-- Look for a .hi file
         (False, lookupFM hi_map mod_name)

      | otherwise
		-- Check if there's a library module of that name
		-- If not, look for an hi-boot file
      = case lookupFM hi_map mod_name of
	   stuff@(Just (_, mod)) | isLibModule mod -> (False, stuff)
	   other		 		   -> (True, lookupFM hiboot_map mod_name)

    trace_msg = sep [hsep [ptext SLIT("Reading"), 
			   ppr from,
			   ptext SLIT("interface for"), 
			   pprModuleName mod_name <> semi],
		     nest 4 (ptext SLIT("reason:") <+> doc_str)]
\end{code}

@readIface@ tries just the one file.

\begin{code}
readIface :: Module -> String -> RnM d (Maybe (Module, ParsedIface))
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 
readIface the_mod file_path
  = ioToRnM (hGetStringBuffer False file_path)       `thenRn` \ read_result ->
    case read_result of
	Right contents	  -> 
             case parseIface contents
			PState{ bol = 0#, atbol = 1#,
				context = [],
				glasgow_exts = 1#,
				loc = mkSrcLoc (mkFastString file_path) 1 } of
	          PFailed err                    -> failWithRn Nothing err 
		  POk _  (PIface mod_nm iface) ->
		    warnCheckRn (mod_nm == moduleName the_mod)
			(hsep [ ptext SLIT("Something is amiss; requested module name")
		        , pprModule the_mod
		        , ptext SLIT("differs from name found in the interface file ")
			, pprModuleName mod_nm
			])
		    `thenRn_` returnRn (Just (the_mod, iface))

        Left err
	  | isDoesNotExistError err -> returnRn Nothing
	  | otherwise               -> failWithRn Nothing (cannaeReadFile file_path err)
\end{code}

%*********************************************************
%*						 	 *
\subsection{Errors}
%*							 *
%*********************************************************

\begin{code}
noIfaceErr filename boot_file
  = hsep [ptext SLIT("Could not find valid"), boot, 
	  ptext SLIT("interface file"), quotes (pprModule filename)]
  where
    boot | boot_file = ptext SLIT("[boot]")
	 | otherwise = empty

cannaeReadFile file err
  = hcat [ptext SLIT("Failed in reading file: "), 
          text file, 
	  ptext SLIT("; error="), 
	  text (show err)]

getDeclErr name
  = ptext SLIT("Failed to find interface decl for") <+> quotes (ppr name)

getDeclWarn name loc
  = sep [ptext SLIT("Failed to find (optional) interface decl for") <+> quotes (ppr name),
	 ptext SLIT("desired at") <+> ppr loc]

importDeclWarn mod name
  = sep [ptext SLIT(
    "Compiler tried to import decl from interface file with same name as module."), 
	 ptext SLIT(
    "(possible cause: module name clashes with interface file already in scope.)")
	] $$
    hsep [ptext SLIT("Interface:"), quotes (pprModuleName mod), 
	  comma, ptext SLIT("name:"), quotes (ppr name)]

warnRedundantSourceImport mod_name
  = ptext SLIT("Unnecessary {- SOURCE -} in the import of module")
          <+> quotes (pprModuleName mod_name)
\end{code}
