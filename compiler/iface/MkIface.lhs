%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module MkIface ( 
	pprModIface, showIface, 	-- Print the iface in Foo.hi

	mkUsageInfo, 	-- Construct the usage info for a module

	mkIface, 	-- Build a ModIface from a ModGuts, 
			-- including computing version information

	writeIfaceFile,	-- Write the interface file

	checkOldIface	-- See if recompilation is required, by
			-- comparing version information
 ) where
\end{code}

	-----------------------------------------------
		MkIface.lhs deals with versioning
	-----------------------------------------------

Here's the version-related info in an interface file

  module Foo 8		-- module-version 
	     3		-- export-list-version
	     2		-- rule-version
    Usages: 	-- Version info for what this compilation of Foo imported
	Baz 3		-- Module version
	    [4]		-- The export-list version if Foo depended on it
	    (g,2)	-- Function and its version
	    (T,1)	-- Type and its version

    <version> f :: Int -> Int {- Unfolding: \x -> Wib.t[2] x -}
		-- The [2] says that f's unfolding 
		-- mentions verison 2 of Wib.t
	
	-----------------------------------------------
			Basic idea
	-----------------------------------------------

Basic idea: 
  * In the mi_usages information in an interface, we record the 
    version number of each free variable of the module

  * In mkIface, we compute the version number of each exported thing A.f
    by comparing its A.f's info with its new info, and bumping its 
    version number if it differs.  If A.f mentions B.g, and B.g's version
    number has changed, then we count A.f as having changed too.

  * In checkOldIface we compare the mi_usages for the module with
    the actual version info for all each thing recorded in mi_usages


Fixities
~~~~~~~~
We count A.f as changing if its fixity changes

Rules
~~~~~
If a rule changes, we want to recompile any module that might be
affected by that rule.  For non-orphan rules, this is relatively easy.
If module M defines f, and a rule for f, just arrange that the version
number for M.f changes if any of the rules for M.f change.  Any module
that does not depend on M.f can't be affected by the rule-change
either.

Orphan rules (ones whose 'head function' is not defined in M) are
harder.  Here's what we do.

  * We have a per-module orphan-rule version number which changes if 
    any orphan rule changes. (It's unaffected by non-orphan rules.)

  * We record usage info for any orphan module 'below' this one,
    giving the orphan-rule version number.  We recompile if this 
    changes. 

The net effect is that if an orphan rule changes, we recompile every
module above it.  That's very conservative, but it's devilishly hard
to know what it might affect, so we just have to be conservative.

Instance decls
~~~~~~~~~~~~~~
In an iface file we have
     module A where
	instance Eq a => Eq [a]  =  dfun29
	dfun29 :: ... 

We have a version number for dfun29, covering its unfolding
etc. Suppose we are compiling a module M that imports A only
indirectly.  If typechecking M uses this instance decl, we record the
dependency on A.dfun29 as if it were a free variable of the module
(via the tcg_inst_usages accumulator).  That means that A will appear
in M's usage list.  If the shape of the instance declaration changes,
then so will dfun29's version, triggering a recompilation.

Adding an instance declaration, or changing an instance decl that is
not currently used, is more tricky.  (This really only makes a
difference when we have overlapping instance decls, because then the
new instance decl might kick in to override the old one.)  We handle
this in a very similar way that we handle rules above.

  * For non-orphan instance decls, identify one locally-defined tycon/class
    mentioned in the decl.  Treat the instance decl as part of the defn of that
    tycon/class, so that if the shape of the instance decl changes, so does the
    tycon/class; that in turn will force recompilation of anything that uses
    that tycon/class.

  * For orphan instance decls, act the same way as for orphan rules.
    Indeed, we use the same global orphan-rule version number.

mkUsageInfo
~~~~~~~~~~~
mkUsageInfo figures out what the ``usage information'' for this
moudule is; that is, what it must record in its interface file as the
things it uses.  

We produce a line for every module B below the module, A, currently being
compiled:
	import B <n> ;
to record the fact that A does import B indirectly.  This is used to decide
to look to look for B.hi rather than B.hi-boot when compiling a module that
imports A.  This line says that A imports B, but uses nothing in it.
So we'll get an early bale-out when compiling A if B's version changes.

The usage information records:

\begin{itemize}
\item	(a) anything reachable from its body code
\item	(b) any module exported with a @module Foo@
\item   (c) anything reachable from an exported item
\end{itemize}

Why (b)?  Because if @Foo@ changes then this module's export list
will change, so we must recompile this module at least as far as
making a new interface file --- but in practice that means complete
recompilation.

Why (c)?  Consider this:
\begin{verbatim}
	module A( f, g ) where	|	module B( f ) where
	  import B( f )		|	  f = h 3
	  g = ...		|	  h = ...
\end{verbatim}

Here, @B.f@ isn't used in A.  Should we nevertheless record @B.f@ in
@A@'s usages?  Our idea is that we aren't going to touch A.hi if it is
*identical* to what it was before.  If anything about @B.f@ changes
than anyone who imports @A@ should be recompiled in case they use
@B.f@ (they'll get an early exit if they don't).  So, if anything
about @B.f@ changes we'd better make sure that something in A.hi
changes, and the convenient way to do that is to record the version
number @B.f@ in A.hi in the usage list.  If B.f changes that'll force a
complete recompiation of A, which is overkill but it's the only way to 
write a new, slightly different, A.hi.

But the example is tricker.  Even if @B.f@ doesn't change at all,
@B.h@ may do so, and this change may not be reflected in @f@'s version
number.  But with -O, a module that imports A must be recompiled if
@B.h@ changes!  So A must record a dependency on @B.h@.  So we treat
the occurrence of @B.f@ in the export list *just as if* it were in the
code of A, and thereby haul in all the stuff reachable from it.

	*** Conclusion: if A mentions B.f in its export list,
	    behave just as if A mentioned B.f in its source code,
	    and slurp in B.f and all its transitive closure ***

[NB: If B was compiled with -O, but A isn't, we should really *still*
haul in all the unfoldings for B, in case the module that imports A *is*
compiled with -O.  I think this is the case.]


\begin{code}
#include "HsVersions.h"

import HsSyn
import Packages		( isHomeModule, PackageIdH(..) )
import IfaceSyn		( IfaceDecl(..), IfaceClassOp(..), IfaceConDecl(..),
			  IfaceRule(..), IfaceInst(..), IfaceExtName(..), 
			  eqIfDecl, eqIfRule, eqIfInst, IfaceEq(..), (&&&), bool, 
			  eqMaybeBy, eqListBy, visibleIfConDecls,
			  tyThingToIfaceDecl, instanceToIfaceInst, coreRuleToIfaceRule )
import LoadIface	( readIface, loadInterface )
import BasicTypes	( Version, initialVersion, bumpVersion )
import TcRnMonad
import HscTypes		( ModIface(..), ModDetails(..), 
			  ModGuts(..), IfaceExport,
			  HscEnv(..), hscEPS, Dependencies(..), FixItem(..), 
			  ModSummary(..), msHiFilePath, 
			  mkIfaceDepCache, mkIfaceFixCache, mkIfaceVerCache,
			  typeEnvElts, 
			  GenAvailInfo(..), availName, 
			  ExternalPackageState(..),
			  Usage(..), IsBootInterface,
			  Deprecs(..), IfaceDeprecs, Deprecations,
			  lookupIfaceByModule
			)


import Packages		( HomeModules )
import DynFlags		( GhcMode(..), DynFlags(..), DynFlag(..), dopt )
import StaticFlags	( opt_HiVersion )
import Name		( Name, nameModule, nameOccName, nameParent,
			  isExternalName, isInternalName, nameParent_maybe, isWiredInName,
			  isImplicitName, NamedThing(..) )
import NameEnv
import NameSet
import OccName		( OccName, OccEnv, mkOccEnv, lookupOccEnv, emptyOccEnv,
			  extendOccEnv_C,
			  OccSet, emptyOccSet, elemOccSet, occSetElts, 
			  extendOccSet, extendOccSetList,
			  isEmptyOccSet, intersectOccSet, intersectsOccSet,
			  occNameFS, isTcOcc )
import Module		( Module, moduleFS,
			  ModLocation(..), mkModuleFS, moduleString,
			  ModuleEnv, emptyModuleEnv, lookupModuleEnv,
			  extendModuleEnv_C
			)
import Outputable
import Util		( createDirectoryHierarchy, directoryOf )
import Util		( sortLe, seqList )
import Binary		( getBinFileWithDict )
import BinIface		( writeBinIface, v_IgnoreHiWay )
import Unique		( Unique, Uniquable(..) )
import ErrUtils		( dumpIfSet_dyn, showPass )
import Digraph		( stronglyConnComp, SCC(..) )
import SrcLoc		( SrcSpan )
import FiniteMap
import FastString

import DATA_IOREF	( writeIORef )
import Monad		( when )
import List		( insert )
import Maybes		( orElse, mapCatMaybes, isNothing, isJust, 
			  expectJust, MaybeErr(..) )
\end{code}



%************************************************************************
%*				 					*
\subsection{Completing an interface}
%*				 					*
%************************************************************************

\begin{code}
mkIface :: HscEnv
	-> Maybe ModIface	-- The old interface, if we have it
	-> ModGuts		-- Usages, deprecations, etc
	-> ModDetails		-- The trimmed, tidied interface
	-> IO (ModIface, 	-- The new one, complete with decls and versions
	       Bool)		-- True <=> there was an old Iface, and the new one
				--	    is identical, so no need to write it

mkIface hsc_env maybe_old_iface 
	(ModGuts{     mg_module  = this_mod,
		      mg_boot    = is_boot,
		      mg_usages  = usages,
		      mg_deps    = deps,
		      mg_home_mods = home_mods,
		      mg_rdr_env = rdr_env,
		      mg_fix_env = fix_env,
		      mg_deprecs = src_deprecs })
	(ModDetails{  md_insts 	 = insts, 
		      md_rules 	 = rules,
		      md_types 	 = type_env,
		      md_exports = exports })
	
-- NB:	notice that mkIface does not look at the bindings
--	only at the TypeEnv.  The previous Tidy phase has
--	put exactly the info into the TypeEnv that we want
--	to expose in the interface

  = do	{ eps <- hscEPS hsc_env
	; let	{ ext_nm_rhs = mkExtNameFn hsc_env home_mods eps this_mod
		; ext_nm_lhs = mkLhsNameFn this_mod

		; decls  = [ tyThingToIfaceDecl ext_nm_rhs thing 
			   | thing <- typeEnvElts type_env, 
			     let name = getName thing,
			     not (isImplicitName name || isWiredInName name) ]
		    	-- Don't put implicit Ids and class tycons in the interface file
			-- Nor wired-in things; the compiler knows about them anyhow

		; fixities    = [(occ,fix) | FixItem occ fix _ <- nameEnvElts fix_env]
		; deprecs     = mkIfaceDeprec src_deprecs
		; iface_rules = map (coreRuleToIfaceRule ext_nm_lhs ext_nm_rhs) rules
		; iface_insts = map (instanceToIfaceInst ext_nm_lhs) insts

	        ; intermediate_iface = ModIface { 
			mi_module   = this_mod,
			mi_package  = HomePackage,
			mi_boot     = is_boot,
			mi_deps     = deps,
			mi_usages   = usages,
			mi_exports  = mkIfaceExports exports,
			mi_insts    = sortLe le_inst iface_insts,
			mi_rules    = sortLe le_rule iface_rules,
			mi_fixities = fixities,
			mi_deprecs  = deprecs,
			mi_globals  = Just rdr_env,

			-- Left out deliberately: filled in by addVersionInfo
			mi_mod_vers  = initialVersion,
 			mi_exp_vers  = initialVersion,
 			mi_rule_vers = initialVersion,
			mi_orphan    = False,	-- Always set by addVersionInfo, but
						-- it's a strict field, so we can't omit it.
			mi_decls     = deliberatelyOmitted "decls",
			mi_ver_fn    = deliberatelyOmitted "ver_fn",

			-- And build the cached values
			mi_dep_fn = mkIfaceDepCache deprecs,
			mi_fix_fn = mkIfaceFixCache fixities }

		-- Add version information
		; (new_iface, no_change_at_all, pp_diffs, pp_orphs) 
			= _scc_ "versioninfo" 
			 addVersionInfo maybe_old_iface intermediate_iface decls
		}

		-- Debug printing
	; when (isJust pp_orphs && dopt Opt_WarnOrphans dflags) 
	       (printDump (expectJust "mkIface" pp_orphs))
	; when (dopt Opt_D_dump_hi_diffs dflags) (printDump pp_diffs)
	; dumpIfSet_dyn dflags Opt_D_dump_hi "FINAL INTERFACE" 
			(pprModIface new_iface)

	; return (new_iface, no_change_at_all) }
  where
     r1 `le_rule` r2 = ifRuleName r1 <= ifRuleName r2
     i1 `le_inst` i2 = ifDFun     i1 <= ifDFun     i2

     dflags = hsc_dflags hsc_env
     deliberatelyOmitted x = panic ("Deliberately omitted: " ++ x)

					      
-----------------------------
writeIfaceFile :: ModLocation -> ModIface -> IO ()
writeIfaceFile location new_iface
    = do createDirectoryHierarchy (directoryOf hi_file_path)
         writeBinIface hi_file_path new_iface
    where hi_file_path = ml_hi_file location


-----------------------------
mkExtNameFn :: HscEnv -> HomeModules -> ExternalPackageState -> Module -> Name -> IfaceExtName
mkExtNameFn hsc_env hmods eps this_mod
  = ext_nm
  where
    hpt = hsc_HPT hsc_env
    pit = eps_PIT eps

    ext_nm name 
      | mod == this_mod = case nameParent_maybe name of
    				Nothing  -> LocalTop occ
    				Just par -> LocalTopSub occ (nameOccName par)
      | isWiredInName name       = ExtPkg  mod occ
      | isHomeModule hmods mod   = HomePkg mod occ vers
      | otherwise	         = ExtPkg  mod occ
      where
    	mod      = nameModule name
    	occ	 = nameOccName name
    	par_occ  = nameOccName (nameParent name)
    		-- The version of the *parent* is the one want
    	vers     = lookupVersion mod par_occ
    	      
    lookupVersion :: Module -> OccName -> Version
	-- Even though we're looking up a home-package thing, in
	-- one-shot mode the imported interfaces may be in the PIT
    lookupVersion mod occ
      = mi_ver_fn iface occ `orElse` 
        pprPanic "lookupVers1" (ppr mod <+> ppr occ)
      where
        iface = lookupIfaceByModule hpt pit mod `orElse` 
	        pprPanic "lookupVers2" (ppr mod <+> ppr occ)


---------------------
-- mkLhsNameFn ignores versioning info altogether
-- It is used for the LHS of instance decls and rules, where we 
-- there's no point in recording version info
mkLhsNameFn :: Module -> Name -> IfaceExtName
mkLhsNameFn this_mod name	
  | isInternalName name = pprTrace "mkLhsNameFn: unexpected internal" (ppr name) $
			  LocalTop occ	-- Should not happen
  | mod == this_mod = LocalTop occ
  | otherwise	    = ExtPkg mod occ
  where
    mod = nameModule name
    occ	= nameOccName name


-----------------------------
-- Compute version numbers for local decls

addVersionInfo :: Maybe ModIface	-- The old interface, read from M.hi
	       -> ModIface		-- The new interface decls (lacking decls)
	       -> [IfaceDecl]		-- The new decls
	       -> (ModIface, 
		   Bool,		-- True <=> no changes at all; no need to write new Iface
		   SDoc,		-- Differences
		   Maybe SDoc)		-- Warnings about orphans

addVersionInfo Nothing new_iface new_decls
-- No old interface, so definitely write a new one!
  = (new_iface { mi_orphan = anyNothing ifInstOrph (mi_insts new_iface)
			  || anyNothing ifRuleOrph (mi_rules new_iface),
		 mi_decls  = [(initialVersion, decl) | decl <- new_decls],
		 mi_ver_fn = \n -> Just initialVersion },
     False, 
     ptext SLIT("No old interface file"),
     pprOrphans orph_insts orph_rules)
  where
    orph_insts = filter (isNothing . ifInstOrph) (mi_insts new_iface)
    orph_rules = filter (isNothing . ifRuleOrph) (mi_rules new_iface)

addVersionInfo (Just old_iface@(ModIface { mi_mod_vers  = old_mod_vers, 
					   mi_exp_vers  = old_exp_vers, 
					   mi_rule_vers = old_rule_vers, 
				       	   mi_decls     = old_decls,
					   mi_ver_fn    = old_decl_vers,
				       	   mi_fix_fn    = old_fixities }))
	       new_iface@(ModIface { mi_fix_fn = new_fixities })
	       new_decls

  | no_change_at_all = (old_iface,   True,  ptext SLIT("Interface file unchanged"), pp_orphs)
  | otherwise	     = (final_iface, False, vcat [ptext SLIT("Interface file has changed"),
						  nest 2 pp_diffs], pp_orphs)
  where
    final_iface = new_iface { mi_mod_vers  = bump_unless no_output_change old_mod_vers,
			      mi_exp_vers  = bump_unless no_export_change old_exp_vers,
			      mi_rule_vers = bump_unless no_rule_change   old_rule_vers,
			      mi_orphan    = not (null new_orph_rules && null new_orph_insts),
			      mi_decls     = decls_w_vers,
			      mi_ver_fn    = mkIfaceVerCache decls_w_vers }

    decls_w_vers = [(add_vers decl, decl) | decl <- new_decls]

    -------------------
    (old_non_orph_insts, old_orph_insts) = mkOrphMap ifInstOrph (mi_insts old_iface)
    (new_non_orph_insts, new_orph_insts) = mkOrphMap ifInstOrph (mi_insts new_iface)
    same_insts occ = eqMaybeBy	(eqListBy eqIfInst) 
				(lookupOccEnv old_non_orph_insts occ)
				(lookupOccEnv new_non_orph_insts occ)
  
    (old_non_orph_rules, old_orph_rules) = mkOrphMap ifRuleOrph (mi_rules old_iface)
    (new_non_orph_rules, new_orph_rules) = mkOrphMap ifRuleOrph (mi_rules new_iface)
    same_rules occ = eqMaybeBy	(eqListBy eqIfRule)
				(lookupOccEnv old_non_orph_rules occ)
				(lookupOccEnv new_non_orph_rules occ)
    -------------------
    -- Computing what changed
    no_output_change = no_decl_change   && no_rule_change && 
    		       no_export_change && no_deprec_change
    no_export_change = mi_exports new_iface == mi_exports old_iface	-- Kept sorted
    no_decl_change   = isEmptyOccSet changed_occs
    no_rule_change   = not (changedWrt changed_occs (eqListBy eqIfRule old_orph_rules new_orph_rules)
	    		 || changedWrt changed_occs (eqListBy eqIfInst old_orph_insts new_orph_insts))
    no_deprec_change = mi_deprecs new_iface == mi_deprecs old_iface

	-- If the usages havn't changed either, we don't need to write the interface file
    no_other_changes = mi_usages new_iface == mi_usages old_iface && 
		       mi_deps new_iface == mi_deps old_iface
    no_change_at_all = no_output_change && no_other_changes
 
    pp_diffs = vcat [pp_change no_export_change "Export list" 
			(ppr old_exp_vers <+> arrow <+> ppr (mi_exp_vers final_iface)),
 		     pp_change no_rule_change "Rules"
			(ppr old_rule_vers <+> arrow <+> ppr (mi_rule_vers final_iface)),
 		     pp_change no_deprec_change "Deprecations" empty,
 		     pp_change no_other_changes  "Usages" empty,
		     pp_decl_diffs]
    pp_change True  what info = empty
    pp_change False what info = text what <+> ptext SLIT("changed") <+> info

    -------------------
    old_decl_env = mkOccEnv [(ifName decl, decl) | (_,decl) <- old_decls]
    same_fixity n = bool (old_fixities n == new_fixities n)

    -------------------
    -- Adding version info
    new_version = bumpVersion old_mod_vers	-- Start from the old module version, not from zero
						-- so that if you remove f, and then add it again,
						-- you don't thereby reduce f's version number
    add_vers decl | occ `elemOccSet` changed_occs = new_version
		  | otherwise = expectJust "add_vers" (old_decl_vers occ)
				-- If it's unchanged, there jolly well 
		  where		-- should be an old version number
		    occ = ifName decl

    -------------------
    changed_occs :: OccSet
    changed_occs = computeChangedOccs eq_info

    eq_info :: [(OccName, IfaceEq)]
    eq_info = map check_eq new_decls
    check_eq new_decl | Just old_decl <- lookupOccEnv old_decl_env occ 
		      = (occ, new_decl `eqIfDecl` old_decl &&&
			      eq_indirects new_decl)
		      | otherwise {- No corresponding old decl -}      
		      = (occ, NotEqual)	
		      where
			occ = ifName new_decl

    eq_indirects :: IfaceDecl -> IfaceEq
		-- When seeing if two decls are the same, remember to
		-- check whether any relevant fixity or rules have changed
    eq_indirects (IfaceId {ifName = occ}) = eq_ind_occ occ
    eq_indirects (IfaceClass {ifName = cls_occ, ifSigs = sigs})
	= same_insts cls_occ &&& 
	  eq_ind_occs [op | IfaceClassOp op _ _ <- sigs] 
    eq_indirects (IfaceData {ifName = tc_occ, ifCons = cons})
	= same_insts tc_occ &&& same_fixity tc_occ &&&	-- The TyCon can have a fixity too
	  eq_ind_occs (map ifConOcc (visibleIfConDecls cons))
    eq_indirects other = Equal	-- Synonyms and foreign declarations

    eq_ind_occ :: OccName -> IfaceEq	-- For class ops and Ids; check fixity and rules
    eq_ind_occ occ = same_fixity occ &&& same_rules occ
    eq_ind_occs = foldr ((&&&) . eq_ind_occ) Equal 
   
    -------------------
    -- Diffs
    pp_decl_diffs :: SDoc	-- Nothing => no changes
    pp_decl_diffs 
	| isEmptyOccSet changed_occs = empty
	| otherwise 
	= vcat [ptext SLIT("Changed occs:") <+> ppr (occSetElts changed_occs),
		ptext SLIT("Version change for these decls:"),
		nest 2 (vcat (map show_change new_decls))]

    eq_env = mkOccEnv eq_info
    show_change new_decl
	| not (occ `elemOccSet` changed_occs) = empty
	| otherwise
	= vcat [ppr occ <+> ppr (old_decl_vers occ) <+> arrow <+> ppr new_version, 
		nest 2 why]
	where
	  occ = ifName new_decl
	  why = case lookupOccEnv eq_env occ of
		    Just (EqBut occs) -> sep [ppr occ <> colon, ptext SLIT("Free vars (only) changed:"),
					      nest 2 (braces (fsep (map ppr (occSetElts 
						(occs `intersectOccSet` changed_occs)))))]
		    Just NotEqual  
			| Just old_decl <- lookupOccEnv old_decl_env occ 
			-> vcat [ptext SLIT("Old:") <+> ppr old_decl,
			 ptext SLIT("New:") <+> ppr new_decl]
			| otherwise 
			-> ppr occ <+> ptext SLIT("only in new interface")
		    other -> pprPanic "MkIface.show_change" (ppr occ)
	
    pp_orphs = pprOrphans new_orph_insts new_orph_rules

pprOrphans insts rules
  | null insts && null rules = Nothing
  | otherwise
  = Just $ vcat [
	if null insts then empty else
	     hang (ptext SLIT("Warning: orphan instances:"))
		2 (vcat (map ppr insts)),
	if null rules then empty else
	     hang (ptext SLIT("Warning: orphan rules:"))
		2 (vcat (map ppr rules))
    ]

computeChangedOccs :: [(OccName, IfaceEq)] -> OccSet
computeChangedOccs eq_info
  = foldl add_changes emptyOccSet (stronglyConnComp edges)
  where
    edges :: [((OccName,IfaceEq), Unique, [Unique])]
    edges = [ (node, getUnique occ, map getUnique occs)
	    | node@(occ, iface_eq) <- eq_info
	    , let occs = case iface_eq of
			   EqBut occ_set -> occSetElts occ_set
			   other -> [] ]

    -- Changes in declarations
    add_changes :: OccSet -> SCC (OccName, IfaceEq) -> OccSet
    add_changes so_far (AcyclicSCC (occ, iface_eq)) 
	| changedWrt so_far iface_eq 				-- This one has changed
	= extendOccSet so_far occ
    add_changes so_far (CyclicSCC pairs)
	| changedWrt so_far (foldr1 (&&&) (map snd pairs)) 	-- One of this group has changed
	= extendOccSetList so_far (map fst pairs)
    add_changes so_far other = so_far

changedWrt :: OccSet -> IfaceEq -> Bool
changedWrt so_far Equal        = False
changedWrt so_far NotEqual     = True
changedWrt so_far (EqBut kids) = so_far `intersectsOccSet` kids

----------------------
-- mkOrphMap partitions instance decls or rules into
-- 	(a) an OccEnv for ones that are not orphans, 
--	    mapping the local OccName to a list of its decls
--	(b) a list of orphan decls
mkOrphMap :: (decl -> Maybe OccName)	-- (Just occ) for a non-orphan decl, keyed by occ
					-- Nothing for an orphan decl
	  -> [decl] 			-- Sorted into canonical order
	  -> (OccEnv [decl],	 	-- Non-orphan decls associated with their key;
					--	each sublist in canonical order
	      [decl])			-- Orphan decls; in canonical order
mkOrphMap get_key decls
  = foldl go (emptyOccEnv, []) decls
  where
    go (non_orphs, orphs) d
	| Just occ <- get_key d
	= (extendOccEnv_C (\ ds _ -> d:ds) non_orphs occ [d], orphs)
	| otherwise = (non_orphs, d:orphs)

anyNothing :: (a -> Maybe b) -> [a] -> Bool
anyNothing p []     = False
anyNothing p (x:xs) = isNothing (p x) || anyNothing p xs

----------------------
mkIfaceDeprec :: Deprecations -> IfaceDeprecs
mkIfaceDeprec NoDeprecs        = NoDeprecs
mkIfaceDeprec (DeprecAll t)    = DeprecAll t
mkIfaceDeprec (DeprecSome env) = DeprecSome (sortLe (<=) (nameEnvElts env))

----------------------
bump_unless :: Bool -> Version -> Version
bump_unless True  v = v	-- True <=> no change
bump_unless False v = bumpVersion v
\end{code}


%*********************************************************
%*							*
\subsection{Keeping track of what we've slurped, and version numbers}
%*							*
%*********************************************************


\begin{code}
mkUsageInfo :: HscEnv 
	    -> HomeModules
	    -> ModuleEnv (Module, Bool, SrcSpan)
	    -> [(Module, IsBootInterface)]
	    -> NameSet -> IO [Usage]
mkUsageInfo hsc_env hmods dir_imp_mods dep_mods used_names
  = do	{ eps <- hscEPS hsc_env
	; let usages = mk_usage_info (eps_PIT eps) hsc_env hmods
				     dir_imp_mods dep_mods used_names
	; usages `seqList`  return usages }
	 -- seq the list of Usages returned: occasionally these
	 -- don't get evaluated for a while and we can end up hanging on to
	 -- the entire collection of Ifaces.

mk_usage_info pit hsc_env hmods dir_imp_mods dep_mods proto_used_names
  = mapCatMaybes mkUsage dep_mods
	-- ToDo: do we need to sort into canonical order?
  where
    hpt = hsc_HPT hsc_env

    used_names = mkNameSet $			-- Eliminate duplicates
		 [ nameParent n			-- Just record usage on the 'main' names
		 | n <- nameSetToList proto_used_names
		 , not (isWiredInName n)	-- Don't record usages for wired-in names
		 , isExternalName n		-- Ignore internal names
		 ]

    -- ent_map groups together all the things imported and used
    -- from a particular module in this package
    ent_map :: ModuleEnv [OccName]
    ent_map  = foldNameSet add_mv emptyModuleEnv used_names
    add_mv name mv_map = extendModuleEnv_C add_item mv_map mod [occ]
    		   where
		     occ = nameOccName name
    		     mod = nameModule name
    		     add_item occs _ = occ:occs
    
    depend_on_exports mod = case lookupModuleEnv dir_imp_mods mod of
    				Just (_,no_imp,_) -> not no_imp
	    			Nothing		  -> True
    
    -- We want to create a Usage for a home module if 
    --	a) we used something from; has something in used_names
    --	b) we imported all of it, even if we used nothing from it
    --		(need to recompile if its export list changes: export_vers)
    --	c) is a home-package orphan module (need to recompile if its
    --	 	instance decls change: rules_vers)
    mkUsage :: (Module, Bool) -> Maybe Usage
    mkUsage (mod_name, _)
      |  isNothing maybe_iface	-- We can't depend on it if we didn't
      || not (isHomeModule hmods mod)	-- even open the interface!
      || (null used_occs
	  && isNothing export_vers
	  && not orphan_mod)
      = Nothing			-- Record no usage info
    
      | otherwise	
      = Just (Usage { usg_name     = mod,
    	  	      usg_mod      = mod_vers,
    		      usg_exports  = export_vers,
    		      usg_entities = ent_vers,
    		      usg_rules    = rules_vers })
      where
	maybe_iface  = lookupIfaceByModule hpt pit mod_name
		-- In one-shot mode, the interfaces for home-package 
		-- modules accumulate in the PIT not HPT.  Sigh.

        Just iface   = maybe_iface
        mod   	     = mi_module    iface
	orphan_mod   = mi_orphan    iface
        version_env  = mi_ver_fn    iface
        mod_vers     = mi_mod_vers  iface
        rules_vers   = mi_rule_vers iface
        export_vers | depend_on_exports mod = Just (mi_exp_vers iface)
    		    | otherwise 	    = Nothing
    
    	-- The sort is to put them into canonical order
        used_occs = lookupModuleEnv ent_map mod `orElse` []
	ent_vers :: [(OccName,Version)]
        ent_vers = [ (occ, version_env occ `orElse` initialVersion) 
		   | occ <- sortLe (<=) used_occs]
\end{code}

\begin{code}
mkIfaceExports :: NameSet -> [(Module, [GenAvailInfo OccName])]
  -- Group by module and sort by occurrence
  -- This keeps the list in canonical order
mkIfaceExports exports 
  = [ (mkModuleFS fs, eltsFM avails)
    | (fs, avails) <- fmToList groupFM
    ]
  where
    groupFM :: FiniteMap FastString (FiniteMap FastString (GenAvailInfo OccName))
	-- Deliberately use the FastString so we
	-- get a canonical ordering
    groupFM = foldl add emptyFM (nameSetToList exports)

    add env name = addToFM_C add_avail env mod_fs 
			     (unitFM avail_fs avail)
      where
	occ    = nameOccName name
	mod_fs = moduleFS (nameModule name)
	avail | Just p <- nameParent_maybe name	= AvailTC (nameOccName p) [occ]
	      | isTcOcc occ 			= AvailTC occ [occ]
	      | otherwise			= Avail occ
	avail_fs = occNameFS (availName avail)	    
	add_avail avail_fm _ = addToFM_C add_item avail_fm avail_fs avail

	add_item (AvailTC p occs) _ = AvailTC p (List.insert occ occs)
	add_item (Avail n)	  _ = pprPanic "MkIface.addAvail" (ppr n <+> ppr name)
\end{code}


%************************************************************************
%*									*
	Load the old interface file for this module (unless
	we have it aleady), and check whether it is up to date
	
%*									*
%************************************************************************

\begin{code}
checkOldIface :: HscEnv
	      -> ModSummary
	      -> Bool 			-- Source unchanged
	      -> Maybe ModIface 	-- Old interface from compilation manager, if any
	      -> IO (RecompileRequired, Maybe ModIface)

checkOldIface hsc_env mod_summary source_unchanged maybe_iface
  = do	{ showPass (hsc_dflags hsc_env) 
	           ("Checking old interface for " ++ moduleString (ms_mod mod_summary)) ;

	; initIfaceCheck hsc_env $
	  check_old_iface mod_summary source_unchanged maybe_iface
     }

check_old_iface mod_summary source_unchanged maybe_iface
 = 	-- CHECK WHETHER THE SOURCE HAS CHANGED
    ifM (not source_unchanged)
	(traceHiDiffs (nest 4 (text "Source file changed or recompilation check turned off")))
					      	`thenM_`

     -- If the source has changed and we're in interactive mode, avoid reading
     -- an interface; just return the one we might have been supplied with.
    getGhcMode					`thenM` \ ghc_mode ->
    if (ghc_mode == Interactive || ghc_mode == JustTypecheck) 
	&& not source_unchanged then
         returnM (outOfDate, maybe_iface)
    else

    case maybe_iface of {
       Just old_iface -> -- Use the one we already have
                         checkVersions source_unchanged old_iface	`thenM` \ recomp ->
			 returnM (recomp, Just old_iface)

    ;  Nothing ->

	-- Try and read the old interface for the current module
	-- from the .hi file left from the last time we compiled it
    let
	iface_path = msHiFilePath mod_summary
    in
    readIface (ms_mod mod_summary) iface_path False	`thenM` \ read_result ->
    case read_result of {
       Failed err ->	-- Old interface file not found, or garbled; give up
		   traceIf (text "FYI: cannot read old interface file:"
			   	 $$ nest 4 err) 	`thenM_`
	           returnM (outOfDate, Nothing)

    ;  Succeeded iface ->	

	-- We have got the old iface; check its versions
    checkVersions source_unchanged iface	`thenM` \ recomp ->
    returnM (recomp, Just iface)
    }}
\end{code}

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
	      -> IfG RecompileRequired
checkVersions source_unchanged iface
  | not source_unchanged
  = returnM outOfDate
  | otherwise
  = do	{ traceHiDiffs (text "Considering whether compilation is required for" <+> 
		        ppr (mi_module iface) <> colon)

	-- Source code unchanged and no errors yet... carry on 

	-- First put the dependent-module info, read from the old interface, into the envt, 
	-- so that when we look for interfaces we look for the right one (.hi or .hi-boot)
	-- 
	-- It's just temporary because either the usage check will succeed 
	-- (in which case we are done with this module) or it'll fail (in which
	-- case we'll compile the module from scratch anyhow).
	--	
	-- We do this regardless of compilation mode
	; updateEps_ $ \eps  -> eps { eps_is_boot = mod_deps }

	; checkList [checkModUsage u | u <- mi_usages iface]
    }
  where
	-- This is a bit of a hack really
    mod_deps :: ModuleEnv (Module, IsBootInterface)
    mod_deps = mkModDeps (dep_mods (mi_deps iface))

checkModUsage :: Usage -> IfG RecompileRequired
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

    loadInterface doc_str mod_name ImportBySystem	`thenM` \ mb_iface ->
	-- Load the interface, but don't complain on failure;
	-- Instead, get an Either back which we can test

    case mb_iface of {
	Failed exn ->  (out_of_date (sep [ptext SLIT("Can't find version number for module"), 
				       ppr mod_name]));
		-- Couldn't find or parse a module mentioned in the
		-- old interface file.  Don't complain -- it might just be that
		-- the current module doesn't need that import and it's been deleted

	Succeeded iface -> 
    let
	new_mod_vers    = mi_mod_vers  iface
	new_decl_vers 	= mi_ver_fn    iface
	new_export_vers = mi_exp_vers  iface
	new_rule_vers   = mi_rule_vers iface
    in
	-- CHECK MODULE
    checkModuleVersion old_mod_vers new_mod_vers	`thenM` \ recompile ->
    if not recompile then
	returnM upToDate
    else
				 
	-- CHECK EXPORT LIST
    if checkExportList maybe_old_export_vers new_export_vers then
	out_of_date_vers (ptext SLIT("  Export list changed"))
		         (expectJust "checkModUsage" maybe_old_export_vers) 
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
  = case new_vers name of

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

----------------------
checkList :: [IfG RecompileRequired] -> IfG RecompileRequired
-- This helper is used in two places
checkList []		 = returnM upToDate
checkList (check:checks) = check	`thenM` \ recompile ->
			   if recompile then 
				returnM outOfDate
			   else
				checkList checks
\end{code}

%************************************************************************
%*				 					*
		Printing interfaces
%*				 					*
%************************************************************************

\begin{code}
showIface :: FilePath -> IO ()
-- Read binary interface, and print it out
showIface filename = do
   -- skip the version check; we don't want to worry about profiled vs.
   -- non-profiled interfaces, for example.
   writeIORef v_IgnoreHiWay True
   iface <- Binary.getBinFileWithDict filename
   printDump (pprModIface iface)
 where
\end{code}


\begin{code}
pprModIface :: ModIface -> SDoc
-- Show a ModIface
pprModIface iface
 = vcat [ ptext SLIT("interface")
		<+> ppr_package (mi_package iface)
		<+> ppr (mi_module iface) <+> pp_boot 
		<+> ppr (mi_mod_vers iface) <+> pp_sub_vers
		<+> (if mi_orphan iface then ptext SLIT("[orphan module]") else empty)
		<+> int opt_HiVersion
		<+> ptext SLIT("where")
	, vcat (map pprExport (mi_exports iface))
	, pprDeps (mi_deps iface)
	, vcat (map pprUsage (mi_usages iface))
	, pprFixities (mi_fixities iface)
	, vcat (map pprIfaceDecl (mi_decls iface))
	, vcat (map ppr (mi_insts iface))
	, vcat (map ppr (mi_rules iface))
	, pprDeprecs (mi_deprecs iface)
	]
  where
    pp_boot | mi_boot iface = ptext SLIT("[boot]")
	    | otherwise     = empty
    ppr_package HomePackage = empty
    ppr_package (ExtPackage id) = doubleQuotes (ppr id)

    exp_vers  = mi_exp_vers iface
    rule_vers = mi_rule_vers iface

    pp_sub_vers | exp_vers == initialVersion && rule_vers == initialVersion = empty
		| otherwise = brackets (ppr exp_vers <+> ppr rule_vers)
\end{code}

When printing export lists, we print like this:
	Avail   f		f
	AvailTC C [C, x, y]	C(x,y)
	AvailTC C [x, y]	C!(x,y)		-- Exporting x, y but not C

\begin{code}
pprExport :: IfaceExport -> SDoc
pprExport (mod, items)
 = hsep [ ptext SLIT("export"), ppr mod, hsep (map pp_avail items) ]
  where
    pp_avail :: GenAvailInfo OccName -> SDoc
    pp_avail (Avail occ)    = ppr occ
    pp_avail (AvailTC _ []) = empty
    pp_avail (AvailTC n (n':ns)) 
	| n==n'     = ppr n <> pp_export ns
 	| otherwise = ppr n <> char '|' <> pp_export (n':ns)
    
    pp_export []    = empty
    pp_export names = braces (hsep (map ppr names))

pprUsage :: Usage -> SDoc
pprUsage usage
  = hsep [ptext SLIT("import"), ppr (usg_name usage), 
	  int (usg_mod usage), 
	  pp_export_version (usg_exports usage),
	  int (usg_rules usage),
	  pp_versions (usg_entities usage) ]
  where
    pp_versions nvs = hsep [ ppr n <+> int v | (n,v) <- nvs ]
    pp_export_version Nothing  = empty
    pp_export_version (Just v) = int v

pprDeps :: Dependencies -> SDoc
pprDeps (Deps { dep_mods = mods, dep_pkgs = pkgs, dep_orphs = orphs})
  = vcat [ptext SLIT("module dependencies:") <+> fsep (map ppr_mod mods),
	  ptext SLIT("package dependencies:") <+> fsep (map ppr pkgs), 
	  ptext SLIT("orphans:") <+> fsep (map ppr orphs)
	]
  where
    ppr_mod (mod_name, boot) = ppr mod_name <+> ppr_boot boot
    ppr_boot True  = text "[boot]"
    ppr_boot False = empty

pprIfaceDecl :: (Version, IfaceDecl) -> SDoc
pprIfaceDecl (ver, decl)
  = ppr_vers ver <+> ppr decl
  where
	-- Print the version for the decl
    ppr_vers v | v == initialVersion = empty
	       | otherwise	     = int v

pprFixities :: [(OccName, Fixity)] -> SDoc
pprFixities []    = empty
pprFixities fixes = ptext SLIT("fixities") <+> pprWithCommas pprFix fixes
		  where
		    pprFix (occ,fix) = ppr fix <+> ppr occ 

pprDeprecs NoDeprecs	    = empty
pprDeprecs (DeprecAll txt)  = ptext SLIT("Deprecate all") <+> doubleQuotes (ftext txt)
pprDeprecs (DeprecSome prs) = ptext SLIT("Deprecate") <+> vcat (map pprDeprec prs)
			    where
			      pprDeprec (name, txt) = ppr name <+> doubleQuotes (ftext txt)
\end{code}
