%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module MkIface ( 
	mkUsageInfo, 	-- Construct the usage info for a module

	mkIface, 	-- Build a ModIface from a ModGuts, 
			-- including computing version information

	writeIfaceFile,	-- Write the interface file

	checkOldIface,	-- See if recompilation is required, by
			-- comparing version information

        tyThingToIfaceDecl -- Converting things to their Iface equivalents
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

import IfaceSyn
import IfaceType
import LoadIface
import Id
import IdInfo
import NewDemand
import CoreSyn
import Class
import TyCon
import DataCon
import Type
import TcType
import InstEnv
import FamInstEnv
import TcRnMonad
import HscTypes

import DynFlags
import Name
import NameEnv
import NameSet
import OccName
import Module
import BinIface
import Unique
import ErrUtils
import Digraph
import SrcLoc
import PackageConfig    hiding ( Version )
import Outputable
import BasicTypes       hiding ( SuccessFlag(..) )
import UniqFM
import Util             hiding ( eqListBy )
import FiniteMap
import FastString
import Maybes

import Control.Monad
import Data.List
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
	(ModGuts{     mg_module    = this_mod,
		      mg_boot      = is_boot,
		      mg_usages    = usages,
		      mg_deps      = deps,
		      mg_rdr_env   = rdr_env,
		      mg_fix_env   = fix_env,
		      mg_deprecs   = src_deprecs })
	(ModDetails{  md_insts 	   = insts, 
		      md_fam_insts = fam_insts,
		      md_rules 	   = rules,
		      md_types 	   = type_env,
		      md_exports   = exports })
	
-- NB:	notice that mkIface does not look at the bindings
--	only at the TypeEnv.  The previous Tidy phase has
--	put exactly the info into the TypeEnv that we want
--	to expose in the interface

  = do	{ eps <- hscEPS hsc_env
	; let	{ entities = typeEnvElts type_env ;
                  decls  = [ tyThingToIfaceDecl entity
			   | entity <- entities,
			     let name = getName entity,
                             not (isImplicitTyThing entity),
	                        -- No implicit Ids and class tycons in the interface file
			     not (isWiredInName name),
	                        -- Nor wired-in things; the compiler knows about them anyhow
			     nameIsLocalOrFrom this_mod name  ]
				-- Sigh: see Note [Root-main Id] in TcRnDriver

		; fixities    = [(occ,fix) | FixItem occ fix _ <- nameEnvElts fix_env]
		; deprecs     = mkIfaceDeprec src_deprecs
		; iface_rules = map coreRuleToIfaceRule rules
		; iface_insts = map instanceToIfaceInst insts
		; iface_fam_insts = map famInstToIfaceFamInst fam_insts

	        ; intermediate_iface = ModIface { 
			mi_module   = this_mod,
			mi_boot     = is_boot,
			mi_deps     = deps,
			mi_usages   = usages,
			mi_exports  = mkIfaceExports exports,
	
			-- Sort these lexicographically, so that
			-- the result is stable across compilations
			mi_insts    = sortLe le_inst iface_insts,
			mi_fam_insts= sortLe le_fam_inst iface_fam_insts,
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
                        mi_finsts    = False,   -- Ditto
			mi_decls     = deliberatelyOmitted "decls",
			mi_ver_fn    = deliberatelyOmitted "ver_fn",

			-- And build the cached values
			mi_dep_fn = mkIfaceDepCache deprecs,
			mi_fix_fn = mkIfaceFixCache fixities }

		-- Add version information
                ; ext_ver_fn = mkParentVerFun hsc_env eps
		; (new_iface, no_change_at_all, pp_diffs, pp_orphs) 
			= _scc_ "versioninfo" 
			 addVersionInfo ext_ver_fn maybe_old_iface
                                         intermediate_iface decls
		}

		-- Debug printing
	; when (isJust pp_orphs && dopt Opt_WarnOrphans dflags) 
	       (printDump (expectJust "mkIface" pp_orphs))
	; when (dopt Opt_D_dump_hi_diffs dflags) (printDump pp_diffs)
	; dumpIfSet_dyn dflags Opt_D_dump_hi "FINAL INTERFACE" 
			(pprModIface new_iface)

	; return (new_iface, no_change_at_all) }
  where
     r1 `le_rule`     r2 = ifRuleName      r1    <=    ifRuleName      r2
     i1 `le_inst`     i2 = ifDFun          i1 `le_occ` ifDFun          i2  
     i1 `le_fam_inst` i2 = ifFamInstTcName i1 `le_occ` ifFamInstTcName i2

     le_occ :: Name -> Name -> Bool
	-- Compare lexicographically by OccName, *not* by unique, because 
	-- the latter is not stable across compilations
     le_occ n1 n2 = nameOccName n1 <= nameOccName n2

     dflags = hsc_dflags hsc_env
     deliberatelyOmitted x = panic ("Deliberately omitted: " ++ x)
     ifFamInstTcName = ifaceTyConName . ifFamInstTyCon

-----------------------------
writeIfaceFile :: DynFlags -> ModLocation -> ModIface -> IO ()
writeIfaceFile dflags location new_iface
    = do createDirectoryHierarchy (directoryOf hi_file_path)
         writeBinIface dflags hi_file_path new_iface
    where hi_file_path = ml_hi_file location


-- -----------------------------------------------------------------------------
-- Look up parents and versions of Names

-- This is like a global version of the mi_ver_fn field in each ModIface.
-- Given a Name, it finds the ModIface, and then uses mi_ver_fn to get
-- the parent and version info.

mkParentVerFun
        :: HscEnv                       -- needed to look up versions
        -> ExternalPackageState         -- ditto
        -> (Name -> (OccName,Version))
mkParentVerFun hsc_env eps
  = \name -> 
      let 
        mod = nameModule name
        occ = nameOccName name
        iface = lookupIfaceByModule (hsc_dflags hsc_env) hpt pit mod `orElse` 
                   pprPanic "lookupVers2" (ppr mod <+> ppr occ)
      in  
        mi_ver_fn iface occ `orElse` 
                 pprPanic "lookupVers1" (ppr mod <+> ppr occ)
  where
      hpt = hsc_HPT hsc_env
      pit = eps_PIT eps

-----------------------------------------------------------------------------
-- Compute version numbers for local decls

addVersionInfo
        :: (Name -> (OccName,Version))  -- lookup parents and versions of names
        -> Maybe ModIface  -- The old interface, read from M.hi
        -> ModIface	   -- The new interface (lacking decls)
        -> [IfaceDecl]	   -- The new decls
        -> (ModIface,   -- Updated interface
            Bool,	   -- True <=> no changes at all; no need to write Iface
            SDoc,	   -- Differences
            Maybe SDoc) -- Warnings about orphans

addVersionInfo ver_fn Nothing new_iface new_decls
-- No old interface, so definitely write a new one!
  = (new_iface { mi_orphan = anyNothing ifInstOrph (mi_insts new_iface)
                                || anyNothing ifRuleOrph (mi_rules new_iface)
               , mi_finsts = not . null $ mi_fam_insts new_iface
               , mi_decls  = [(initialVersion, decl) | decl <- new_decls]
               , mi_ver_fn = mkIfaceVerCache (zip (repeat initialVersion) 
						  new_decls)
	       },
     False, 
     ptext SLIT("No old interface file"),
     pprOrphans orph_insts orph_rules)
  where
    orph_insts = filter (isNothing . ifInstOrph) (mi_insts new_iface)
    orph_rules = filter (isNothing . ifRuleOrph) (mi_rules new_iface)

addVersionInfo ver_fn (Just old_iface@(ModIface { 
                                           mi_mod_vers  = old_mod_vers, 
					   mi_exp_vers  = old_exp_vers, 
					   mi_rule_vers = old_rule_vers, 
				       	   mi_decls     = old_decls,
					   mi_ver_fn    = old_decl_vers,
				       	   mi_fix_fn    = old_fixities }))
	       new_iface@(ModIface { mi_fix_fn = new_fixities })
	       new_decls
 | no_change_at_all
 = (old_iface,  True,   ptext SLIT("Interface file unchanged"), pp_orphs)
 | otherwise
 = (final_iface, False, vcat [ptext SLIT("Interface file has changed"),
			      nest 2 pp_diffs], pp_orphs)
 where
    final_iface = new_iface { 
                mi_mod_vers  = bump_unless no_output_change old_mod_vers,
                mi_exp_vers  = bump_unless no_export_change old_exp_vers,
                mi_rule_vers = bump_unless no_rule_change   old_rule_vers,
                mi_orphan    = not (null new_orph_rules && null new_orph_insts),
                mi_finsts    = not . null $ mi_fam_insts new_iface,
                mi_decls     = decls_w_vers,
                mi_ver_fn    = mkIfaceVerCache decls_w_vers }

    decls_w_vers = [(add_vers decl, decl) | decl <- new_decls]

    -------------------
    (old_non_orph_insts, old_orph_insts) = 
        mkOrphMap ifInstOrph (mi_insts old_iface)
    (new_non_orph_insts, new_orph_insts) = 
        mkOrphMap ifInstOrph (mi_insts new_iface)
    old_fam_insts = mi_fam_insts old_iface
    new_fam_insts = mi_fam_insts new_iface
    same_insts occ = eqMaybeBy	(eqListBy eqIfInst) 
				(lookupOccEnv old_non_orph_insts occ)
				(lookupOccEnv new_non_orph_insts occ)
  
    (old_non_orph_rules, old_orph_rules) = 
        mkOrphMap ifRuleOrph (mi_rules old_iface)
    (new_non_orph_rules, new_orph_rules) = 
        mkOrphMap ifRuleOrph (mi_rules new_iface)
    same_rules occ = eqMaybeBy	(eqListBy eqIfRule)
				(lookupOccEnv old_non_orph_rules occ)
				(lookupOccEnv new_non_orph_rules occ)
    -------------------
    -- Computing what changed
    no_output_change = no_decl_change   && no_rule_change && 
    		       no_export_change && no_deprec_change
    no_export_change = mi_exports new_iface == mi_exports old_iface
                                -- Kept sorted
    no_decl_change   = isEmptyOccSet changed_occs
    no_rule_change   = not (changedWrtNames changed_occs (eqListBy eqIfRule old_orph_rules new_orph_rules)
	    		 || changedWrtNames changed_occs (eqListBy eqIfInst old_orph_insts new_orph_insts)
	    		 || changedWrtNames changed_occs (eqListBy eqIfFamInst old_fam_insts new_fam_insts))
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
    new_version = bumpVersion old_mod_vers
                        -- Start from the old module version, not from
                        -- zero so that if you remove f, and then add
                        -- it again, you don't thereby reduce f's
                        -- version number

    add_vers decl | occ `elemOccSet` changed_occs = new_version
		  | otherwise = snd (expectJust "add_vers" (old_decl_vers occ))
				-- If it's unchanged, there jolly well 
		  where		-- should be an old version number
		    occ = ifName decl

    -------------------
    -- Deciding which declarations have changed
            
    -- For each local decl, the IfaceEq gives the list of things that
    -- must be unchanged for the declaration as a whole to be unchanged.
    eq_info :: [(OccName, IfaceEq)]
    eq_info = map check_eq new_decls
    check_eq new_decl
         | Just old_decl <- lookupOccEnv old_decl_env occ 
	 = (occ, new_decl `eqIfDecl` old_decl &&& eq_indirects new_decl)
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
     
    -- The Occs of declarations that changed.
    changed_occs :: OccSet
    changed_occs = computeChangedOccs ver_fn (mi_module new_iface)
                         (mi_usages old_iface) eq_info

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
		    Just (EqBut names) -> sep [ppr occ <> colon, ptext SLIT("Free vars (only) changed:") <> ppr names,
					      nest 2 (braces (fsep (map ppr (occSetElts 
						(occs `intersectOccSet` changed_occs)))))]
                           where occs = mkOccSet (map nameOccName (nameSetToList names))
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

computeChangedOccs
        :: (Name -> (OccName,Version))     -- get parents and versions
        -> Module                       -- This module
        -> [Usage]                      -- Usages from old iface
        -> [(OccName, IfaceEq)]         -- decl names, equality conditions
        -> OccSet                       -- set of things that have changed
computeChangedOccs ver_fn this_module old_usages eq_info
  = foldl add_changes emptyOccSet (stronglyConnComp edges)
  where

    -- return True if an external name has changed
    name_changed :: Name -> Bool
    name_changed nm
        | Just ents <- lookupUFM usg_modmap (moduleName mod) 
        = case lookupUFM ents parent_occ of
                Nothing -> pprPanic "computeChangedOccs" (ppr nm)
                Just v  -> v < new_version
        | otherwise = False -- must be in another package
      where
         mod = nameModule nm
         (parent_occ, new_version) = ver_fn nm

    -- Turn the usages from the old ModIface into a mapping
    usg_modmap = listToUFM [ (usg_mod usg, listToUFM (usg_entities usg))
                           | usg <- old_usages ]

    get_local_eq_info :: GenIfaceEq NameSet -> GenIfaceEq OccSet
    get_local_eq_info Equal = Equal
    get_local_eq_info NotEqual = NotEqual
    get_local_eq_info (EqBut ns) = foldNameSet f Equal ns
        where f name eq | nameModule name == this_module =         
                          EqBut (unitOccSet (nameOccName name)) `and_occifeq` eq
                        | name_changed name = NotEqual
                        | otherwise = eq

    local_eq_infos = mapSnd get_local_eq_info eq_info

    edges :: [((OccName, OccIfaceEq), Unique, [Unique])]
    edges = [ (node, getUnique occ, map getUnique occs)
	    | node@(occ, iface_eq) <- local_eq_infos
	    , let occs = case iface_eq of
			   EqBut occ_set -> occSetElts occ_set
			   other -> [] ]

    -- Changes in declarations
    add_changes :: OccSet -> SCC (OccName, OccIfaceEq) -> OccSet
    add_changes so_far (AcyclicSCC (occ, iface_eq)) 
	| changedWrt so_far iface_eq -- This one has changed
	= extendOccSet so_far occ
    add_changes so_far (CyclicSCC pairs)
	| changedWrt so_far (foldr1 and_occifeq iface_eqs)
        	-- One of this group has changed
	= extendOccSetList so_far occs
        where (occs, iface_eqs) = unzip pairs
    add_changes so_far other = so_far

type OccIfaceEq = GenIfaceEq OccSet

changedWrt :: OccSet -> OccIfaceEq -> Bool
changedWrt so_far Equal        = False
changedWrt so_far NotEqual     = True
changedWrt so_far (EqBut kids) = so_far `intersectsOccSet` kids

changedWrtNames :: OccSet -> IfaceEq -> Bool
changedWrtNames so_far Equal        = False
changedWrtNames so_far NotEqual     = True
changedWrtNames so_far (EqBut kids) = 
  so_far `intersectsOccSet` mkOccSet (map nameOccName (nameSetToList kids))

and_occifeq :: OccIfaceEq -> OccIfaceEq -> OccIfaceEq
Equal       `and_occifeq` x 	    = x
NotEqual    `and_occifeq` x	    = NotEqual
EqBut nms   `and_occifeq` Equal       = EqBut nms
EqBut nms   `and_occifeq` NotEqual    = NotEqual
EqBut nms1  `and_occifeq` EqBut nms2  = EqBut (nms1 `unionOccSets` nms2)

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
	    -> ModuleEnv (Module, Bool, SrcSpan)
	    -> [(ModuleName, IsBootInterface)]
	    -> NameSet -> IO [Usage]
mkUsageInfo hsc_env dir_imp_mods dep_mods used_names
  = do	{ eps <- hscEPS hsc_env
	; let usages = mk_usage_info (eps_PIT eps) hsc_env 
				     dir_imp_mods dep_mods used_names
	; usages `seqList`  return usages }
	 -- seq the list of Usages returned: occasionally these
	 -- don't get evaluated for a while and we can end up hanging on to
	 -- the entire collection of Ifaces.

mk_usage_info pit hsc_env dir_imp_mods dep_mods used_names
  = mapCatMaybes mkUsage dep_mods
	-- ToDo: do we need to sort into canonical order?
  where
    hpt = hsc_HPT hsc_env
    dflags = hsc_dflags hsc_env

    -- ent_map groups together all the things imported and used
    -- from a particular module in this package
    ent_map :: ModuleEnv [OccName]
    ent_map  = foldNameSet add_mv emptyModuleEnv used_names
    add_mv name mv_map
        | isWiredInName name = mv_map  -- ignore wired-in names
        | otherwise
        = case nameModule_maybe name of
             Nothing  -> mv_map         -- ignore internal names
             Just mod -> extendModuleEnv_C add_item mv_map mod [occ]
    		   where
		     occ = nameOccName name
    		     add_item occs _ = occ:occs
    
    depend_on_exports mod = case lookupModuleEnv dir_imp_mods mod of
    				Just (_,no_imp,_) -> not no_imp
	    			Nothing		  -> True
    
    -- We want to create a Usage for a home module if 
    --	a) we used something from; has something in used_names
    --	b) we imported all of it, even if we used nothing from it
    --		(need to recompile if its export list changes: export_vers)
    --	c) is a home-package orphan or family-instance module (need to
    --	        recompile if its instance decls change: rules_vers)
    mkUsage :: (ModuleName, IsBootInterface) -> Maybe Usage
    mkUsage (mod_name, _)
      |  isNothing maybe_iface		-- We can't depend on it if we didn't
      || (null used_occs		-- load its interface.
	  && isNothing export_vers
	  && not orphan_mod
	  && not finsts_mod)
      = Nothing			-- Record no usage info
    
      | otherwise	
      = Just (Usage { usg_name     = mod_name,
    	  	      usg_mod      = mod_vers,
    		      usg_exports  = export_vers,
    		      usg_entities = fmToList ent_vers,
    		      usg_rules    = rules_vers })
      where
	maybe_iface  = lookupIfaceByModule dflags hpt pit mod
		-- In one-shot mode, the interfaces for home-package 
		-- modules accumulate in the PIT not HPT.  Sigh.

        mod = mkModule (thisPackage dflags) mod_name

        Just iface   = maybe_iface
	orphan_mod   = mi_orphan    iface
	finsts_mod   = mi_finsts    iface
        version_env  = mi_ver_fn    iface
        mod_vers     = mi_mod_vers  iface
        rules_vers   = mi_rule_vers iface
        export_vers | depend_on_exports mod = Just (mi_exp_vers iface)
    		    | otherwise 	    = Nothing
    
        used_occs = lookupModuleEnv ent_map mod `orElse` []

    	-- Making a FiniteMap here ensures that (a) we remove duplicates
        -- when we have usages on several subordinates of a single parent,
        -- and (b) that the usages emerge in a canonical order, which
        -- is why we use FiniteMap rather than OccEnv: FiniteMap works
        -- using Ord on the OccNames, which is a lexicographic ordering.
	ent_vers :: FiniteMap OccName Version
        ent_vers = listToFM (map lookup_occ used_occs)
        
        lookup_occ occ = 
            case version_env occ of
                Nothing -> pprTrace "hmm, strange" (ppr mod <+> ppr occ) $
                           (occ, initialVersion) -- does this ever happen?
                Just (parent, version) -> (parent, version)
\end{code}

\begin{code}
mkIfaceExports :: [AvailInfo]
               -> [(Module, [GenAvailInfo OccName])]
  -- Group by module and sort by occurrence
  -- This keeps the list in canonical order
mkIfaceExports exports
  = [ (mod, eltsFM avails)
    | (mod, avails) <- fmToList groupFM
    ]
  where
	-- Deliberately use FiniteMap rather than UniqFM so we
	-- get a canonical ordering
    groupFM :: ModuleEnv (FiniteMap FastString (GenAvailInfo OccName))
    groupFM = foldl add emptyModuleEnv exports

    add env avail
      = extendModuleEnv_C add_avail env mod (unitFM avail_fs avail_occ)
      where
	avail_occ = availToOccs avail
	mod  = nameModule (availName avail)
	avail_fs = occNameFS (availName avail_occ)
	add_avail avail_fm _ = addToFM avail_fm avail_fs avail_occ

    availToOccs (Avail n) = Avail (nameOccName n)
    availToOccs (AvailTC tc ns) = AvailTC (nameOccName tc) (map nameOccName ns)
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
	           ("Checking old interface for " ++ 
			showSDoc (ppr (ms_mod mod_summary))) ;

	; initIfaceCheck hsc_env $
	  check_old_iface hsc_env mod_summary source_unchanged maybe_iface
     }

check_old_iface hsc_env mod_summary source_unchanged maybe_iface
 =  do 	-- CHECK WHETHER THE SOURCE HAS CHANGED
    { ifM (not source_unchanged)
	   (traceHiDiffs (nest 4 (text "Source file changed or recompilation check turned off")))

     -- If the source has changed and we're in interactive mode, avoid reading
     -- an interface; just return the one we might have been supplied with.
    ; ghc_mode <- getGhcMode
    ; if (ghc_mode == Interactive || ghc_mode == JustTypecheck) 
	 && not source_unchanged then
         return (outOfDate, maybe_iface)
      else
      case maybe_iface of {
        Just old_iface -> do -- Use the one we already have
	  { traceIf (text "We already have the old interface for" <+> ppr (ms_mod mod_summary))
	  ; recomp <- checkVersions hsc_env source_unchanged old_iface
	  ; return (recomp, Just old_iface) }

      ; Nothing -> do

	-- Try and read the old interface for the current module
	-- from the .hi file left from the last time we compiled it
    { let iface_path = msHiFilePath mod_summary
    ; read_result <- readIface (ms_mod mod_summary) iface_path False
    ; case read_result of {
         Failed err -> do	-- Old interface file not found, or garbled; give up
		{ traceIf (text "FYI: cannot read old interface file:"
			   	 $$ nest 4 err)
	        ; return (outOfDate, Nothing) }

      ;  Succeeded iface -> do

	-- We have got the old iface; check its versions
    { traceIf (text "Read the interface file" <+> text iface_path)
    ; recomp <- checkVersions hsc_env source_unchanged iface
    ; returnM (recomp, Just iface)
    }}}}}
\end{code}

@recompileRequired@ is called from the HscMain.   It checks whether
a recompilation is required.  It needs access to the persistent state,
finder, etc, because it may have to load lots of interface files to
check their versions.

\begin{code}
type RecompileRequired = Bool
upToDate  = False	-- Recompile not required
outOfDate = True	-- Recompile required

checkVersions :: HscEnv
	      -> Bool		-- True <=> source unchanged
	      -> ModIface 	-- Old interface
	      -> IfG RecompileRequired
checkVersions hsc_env source_unchanged iface
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
	-- We do this regardless of compilation mode, although in --make mode
	-- all the dependent modules should be in the HPT already, so it's
	-- quite redundant
	; updateEps_ $ \eps  -> eps { eps_is_boot = mod_deps }

	; let this_pkg = thisPackage (hsc_dflags hsc_env)
	; checkList [checkModUsage this_pkg u | u <- mi_usages iface]
    }
  where
	-- This is a bit of a hack really
    mod_deps :: ModuleNameEnv (ModuleName, IsBootInterface)
    mod_deps = mkModDeps (dep_mods (mi_deps iface))

checkModUsage :: PackageId ->Usage -> IfG RecompileRequired
-- Given the usage information extracted from the old
-- M.hi file for the module being compiled, figure out
-- whether M needs to be recompiled.

checkModUsage this_pkg (Usage { usg_name = mod_name, usg_mod = old_mod_vers,
		       		usg_rules = old_rule_vers,
		       		usg_exports = maybe_old_export_vers, 
		       		usg_entities = old_decl_vers })
  = 	-- Load the imported interface is possible
    let
    	doc_str = sep [ptext SLIT("need version info for"), ppr mod_name]
    in
    traceHiDiffs (text "Checking usages for module" <+> ppr mod_name) `thenM_`

    let
	mod = mkModule this_pkg mod_name
    in
    loadInterface doc_str mod ImportBySystem		`thenM` \ mb_iface ->
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

	Just (_, new_vers) 	-- It's there, but is it up to date?
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
		Converting things to their Iface equivalents
%*				 					*
%************************************************************************

\begin{code}
tyThingToIfaceDecl :: TyThing -> IfaceDecl
-- Assumption: the thing is already tidied, so that locally-bound names
-- 	       (lambdas, for-alls) already have non-clashing OccNames
-- Reason: Iface stuff uses OccNames, and the conversion here does
--	   not do tidying on the way
tyThingToIfaceDecl (AnId id)
  = IfaceId { ifName   = getOccName id,
	      ifType   = toIfaceType (idType id),
	      ifIdInfo = info }
  where
    info = case toIfaceIdInfo (idInfo id) of
		[]    -> NoInfo
		items -> HasInfo items

tyThingToIfaceDecl (AClass clas)
  = IfaceClass { ifCtxt	  = toIfaceContext sc_theta,
		 ifName	  = getOccName clas,
		 ifTyVars = toIfaceTvBndrs clas_tyvars,
		 ifFDs    = map toIfaceFD clas_fds,
		 ifATs	  = map (tyThingToIfaceDecl . ATyCon) clas_ats,
		 ifSigs	  = map toIfaceClassOp op_stuff,
	  	 ifRec    = boolToRecFlag (isRecursiveTyCon tycon) }
  where
    (clas_tyvars, clas_fds, sc_theta, _, clas_ats, op_stuff) 
      = classExtraBigSig clas
    tycon = classTyCon clas

    toIfaceClassOp (sel_id, def_meth)
	= ASSERT(sel_tyvars == clas_tyvars)
	  IfaceClassOp (getOccName sel_id) def_meth (toIfaceType op_ty)
	where
		-- Be careful when splitting the type, because of things
		-- like  	class Foo a where
		--		  op :: (?x :: String) => a -> a
		-- and  	class Baz a where
		--		  op :: (Ord a) => a -> a
	  (sel_tyvars, rho_ty) = splitForAllTys (idType sel_id)
	  op_ty		       = funResultTy rho_ty

    toIfaceFD (tvs1, tvs2) = (map getFS tvs1, map getFS tvs2)

tyThingToIfaceDecl (ATyCon tycon)
  | isSynTyCon tycon
  = IfaceSyn {	ifName   = getOccName tycon,
		ifTyVars = toIfaceTvBndrs tyvars,
		ifOpenSyn = syn_isOpen,
		ifSynRhs  = toIfaceType syn_tyki }

  | isAlgTyCon tycon
  = IfaceData {	ifName    = getOccName tycon,
		ifTyVars  = toIfaceTvBndrs tyvars,
		ifCtxt    = toIfaceContext (tyConStupidTheta tycon),
		ifCons    = ifaceConDecls (algTyConRhs tycon),
	  	ifRec     = boolToRecFlag (isRecursiveTyCon tycon),
		ifGadtSyntax = isGadtSyntaxTyCon tycon,
		ifGeneric = tyConHasGenerics tycon,
		ifFamInst = famInstToIface (tyConFamInst_maybe tycon)}

  | isForeignTyCon tycon
  = IfaceForeign { ifName    = getOccName tycon,
	    	   ifExtName = tyConExtName tycon }

  | otherwise = pprPanic "toIfaceDecl" (ppr tycon)
  where
    tyvars = tyConTyVars tycon
    (syn_isOpen, syn_tyki) = case synTyConRhs tycon of
			       OpenSynTyCon ki -> (True , ki)
			       SynonymTyCon ty -> (False, ty)

    ifaceConDecls (NewTyCon { data_con = con })    = 
      IfNewTyCon  (ifaceConDecl con)
    ifaceConDecls (DataTyCon { data_cons = cons }) = 
      IfDataTyCon (map ifaceConDecl cons)
    ifaceConDecls OpenDataTyCon                    = IfOpenDataTyCon
    ifaceConDecls OpenNewTyCon                     = IfOpenNewTyCon
    ifaceConDecls AbstractTyCon			   = IfAbstractTyCon
	-- The last case happens when a TyCon has been trimmed during tidying
	-- Furthermore, tyThingToIfaceDecl is also used
	-- in TcRnDriver for GHCi, when browsing a module, in which case the
	-- AbstractTyCon case is perfectly sensible.

    ifaceConDecl data_con 
	= IfCon   { ifConOcc   	 = getOccName (dataConName data_con),
		    ifConInfix 	 = dataConIsInfix data_con,
		    ifConUnivTvs = toIfaceTvBndrs (dataConUnivTyVars data_con),
		    ifConExTvs   = toIfaceTvBndrs (dataConExTyVars data_con),
		    ifConEqSpec  = to_eq_spec (dataConEqSpec data_con),
		    ifConCtxt    = toIfaceContext (dataConTheta data_con),
		    ifConArgTys  = map toIfaceType (dataConOrigArgTys data_con),
		    ifConFields  = map getOccName 
				       (dataConFieldLabels data_con),
		    ifConStricts = dataConStrictMarks data_con }

    to_eq_spec spec = [(getOccName tv, toIfaceType ty) | (tv,ty) <- spec]

    famInstToIface Nothing                    = Nothing
    famInstToIface (Just (famTyCon, instTys)) = 
      Just (toIfaceTyCon famTyCon, map toIfaceType instTys)

tyThingToIfaceDecl (ADataCon dc)
 = pprPanic "toIfaceDecl" (ppr dc)	-- Should be trimmed out earlier


getFS x = occNameFS (getOccName x)

--------------------------
instanceToIfaceInst :: Instance -> IfaceInst
instanceToIfaceInst ispec@(Instance { is_dfun = dfun_id, is_flag = oflag,
				      is_cls = cls, is_tcs = mb_tcs, 
				      is_orph = orph })
  = IfaceInst { ifDFun    = getName dfun_id,
		ifOFlag   = oflag,
		ifInstCls = cls,
		ifInstTys = map do_rough mb_tcs,
		ifInstOrph = orph }
  where
    do_rough Nothing  = Nothing
    do_rough (Just n) = Just (toIfaceTyCon_name n)

--------------------------
famInstToIfaceFamInst :: FamInst -> IfaceFamInst
famInstToIfaceFamInst fi@(FamInst { fi_tycon = tycon,
					    fi_fam = fam, fi_tcs = mb_tcs })
  = IfaceFamInst { ifFamInstTyCon  = toIfaceTyCon tycon
		 , ifFamInstFam    = fam
		 , ifFamInstTys    = map do_rough mb_tcs }
  where
    do_rough Nothing  = Nothing
    do_rough (Just n) = Just (toIfaceTyCon_name n)

--------------------------
toIfaceIdInfo :: IdInfo -> [IfaceInfoItem]
toIfaceIdInfo id_info
  = catMaybes [arity_hsinfo, caf_hsinfo, strict_hsinfo, 
	       inline_hsinfo, wrkr_hsinfo,  unfold_hsinfo] 
  where
    ------------  Arity  --------------
    arity_info = arityInfo id_info
    arity_hsinfo | arity_info == 0 = Nothing
		 | otherwise       = Just (HsArity arity_info)

    ------------ Caf Info --------------
    caf_info   = cafInfo id_info
    caf_hsinfo = case caf_info of
		   NoCafRefs -> Just HsNoCafRefs
		   _other    -> Nothing

    ------------  Strictness  --------------
	-- No point in explicitly exporting TopSig
    strict_hsinfo = case newStrictnessInfo id_info of
			Just sig | not (isTopSig sig) -> Just (HsStrictness sig)
			_other			      -> Nothing

    ------------  Worker  --------------
    work_info   = workerInfo id_info
    has_worker  = case work_info of { HasWorker _ _ -> True; other -> False }
    wrkr_hsinfo = case work_info of
		    HasWorker work_id wrap_arity -> 
			Just (HsWorker ((idName work_id)) wrap_arity)
		    NoWorker -> Nothing

    ------------  Unfolding  --------------
    -- The unfolding is redundant if there is a worker
    unfold_info  = unfoldingInfo id_info
    rhs		 = unfoldingTemplate unfold_info
    no_unfolding = neverUnfold unfold_info
		  	-- The CoreTidy phase retains unfolding info iff
			-- we want to expose the unfolding, taking into account
			-- unconditional NOINLINE, etc.  See TidyPgm.addExternal
    unfold_hsinfo | no_unfolding = Nothing			
		  | has_worker   = Nothing	-- Unfolding is implicit
		  | otherwise	 = Just (HsUnfold (toIfaceExpr rhs))
					
    ------------  Inline prag  --------------
    inline_prag = inlinePragInfo id_info
    inline_hsinfo | isAlwaysActive inline_prag     = Nothing
		  | no_unfolding && not has_worker = Nothing
			-- If the iface file give no unfolding info, we 
			-- don't need to say when inlining is OK!
		  | otherwise			   = Just (HsInline inline_prag)

--------------------------
coreRuleToIfaceRule :: CoreRule -> IfaceRule
coreRuleToIfaceRule (BuiltinRule { ru_fn = fn})
  = pprTrace "toHsRule: builtin" (ppr fn) $
    bogusIfaceRule fn

coreRuleToIfaceRule (Rule { ru_name = name, ru_fn = fn, 
                            ru_act = act, ru_bndrs = bndrs,
	                    ru_args = args, ru_rhs = rhs, ru_orph = orph })
  = IfaceRule { ifRuleName  = name, ifActivation = act, 
		ifRuleBndrs = map toIfaceBndr bndrs,
		ifRuleHead  = fn, 
		ifRuleArgs  = map do_arg args,
		ifRuleRhs   = toIfaceExpr rhs,
		ifRuleOrph  = orph }
  where
	-- For type args we must remove synonyms from the outermost
	-- level.  Reason: so that when we read it back in we'll
	-- construct the same ru_rough field as we have right now;
	-- see tcIfaceRule
    do_arg (Type ty) = IfaceType (toIfaceType (deNoteType ty))
    do_arg arg       = toIfaceExpr arg

bogusIfaceRule :: Name -> IfaceRule
bogusIfaceRule id_name
  = IfaceRule { ifRuleName = FSLIT("bogus"), ifActivation = NeverActive,  
	ifRuleBndrs = [], ifRuleHead = id_name, ifRuleArgs = [], 
	ifRuleRhs = IfaceExt id_name, ifRuleOrph = Nothing }

---------------------
toIfaceExpr :: CoreExpr -> IfaceExpr
toIfaceExpr (Var v)       = toIfaceVar v
toIfaceExpr (Lit l)       = IfaceLit l
toIfaceExpr (Type ty)     = IfaceType (toIfaceType ty)
toIfaceExpr (Lam x b)     = IfaceLam (toIfaceBndr x) (toIfaceExpr b)
toIfaceExpr (App f a)     = toIfaceApp f [a]
toIfaceExpr (Case s x ty as) = IfaceCase (toIfaceExpr s) (getFS x) (toIfaceType ty) (map toIfaceAlt as)
toIfaceExpr (Let b e)     = IfaceLet (toIfaceBind b) (toIfaceExpr e)
toIfaceExpr (Cast e co)   = IfaceCast (toIfaceExpr e) (toIfaceType co)
toIfaceExpr (Note n e)    = IfaceNote (toIfaceNote n) (toIfaceExpr e)

---------------------
toIfaceNote (SCC cc)      = IfaceSCC cc
toIfaceNote InlineMe      = IfaceInlineMe
toIfaceNote (CoreNote s)  = IfaceCoreNote s

---------------------
toIfaceBind (NonRec b r) = IfaceNonRec (toIfaceIdBndr b) (toIfaceExpr r)
toIfaceBind (Rec prs)    = IfaceRec [(toIfaceIdBndr b, toIfaceExpr r) | (b,r) <- prs]

---------------------
toIfaceAlt (c,bs,r) = (toIfaceCon c, map getFS bs, toIfaceExpr r)

---------------------
toIfaceCon (DataAlt dc) | isTupleTyCon tc = IfaceTupleAlt (tupleTyConBoxity tc)
	   		| otherwise       = IfaceDataAlt (getName dc)
	   		where
	   		  tc = dataConTyCon dc
	   
toIfaceCon (LitAlt l) = IfaceLitAlt l
toIfaceCon DEFAULT    = IfaceDefault

---------------------
toIfaceApp (App f a) as = toIfaceApp f (a:as)
toIfaceApp (Var v) as
  = case isDataConWorkId_maybe v of
	-- We convert the *worker* for tuples into IfaceTuples
	Just dc |  isTupleTyCon tc && saturated 
		-> IfaceTuple (tupleTyConBoxity tc) tup_args
	  where
	    val_args  = dropWhile isTypeArg as
	    saturated = val_args `lengthIs` idArity v
	    tup_args  = map toIfaceExpr val_args
	    tc	      = dataConTyCon dc

        other -> mkIfaceApps (toIfaceVar v) as

toIfaceApp e as = mkIfaceApps (toIfaceExpr e) as

mkIfaceApps f as = foldl (\f a -> IfaceApp f (toIfaceExpr a)) f as

---------------------
toIfaceVar :: Id -> IfaceExpr
toIfaceVar v 
  | Just fcall <- isFCallId_maybe v = IfaceFCall fcall (toIfaceType (idType v))
	  -- Foreign calls have special syntax
  | isExternalName name		    = IfaceExt name
  | otherwise			    = IfaceLcl (getFS name)
  where
    name = idName v
\end{code}
