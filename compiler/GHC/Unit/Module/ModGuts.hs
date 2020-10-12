module GHC.Unit.Module.ModGuts
   ( ModGuts (..)
   , CgGuts (..)
   )
where

import GHC.Prelude

import GHC.ByteCode.Types
import GHC.ForeignSrcLang

import GHC.Hs

import GHC.Unit
import GHC.Unit.Module.Deps
import GHC.Unit.Module.Warnings

import GHC.Core.InstEnv ( InstEnv, ClsInst )
import GHC.Core.FamInstEnv
import GHC.Core         ( CoreProgram, CoreRule )
import GHC.Core.TyCon
import GHC.Core.PatSyn

import GHC.Linker.Types ( SptEntry(..) )

import GHC.Types.Annotations ( Annotation )
import GHC.Types.Avail
import GHC.Types.CompleteMatch
import GHC.Types.Fixity.Env
import GHC.Types.ForeignStubs
import GHC.Types.HpcInfo
import GHC.Types.Name.Reader
import GHC.Types.SafeHaskell
import GHC.Types.SourceFile ( HscSource(..) )
import GHC.Types.SrcLoc


-- | A ModGuts is carried through the compiler, accumulating stuff as it goes
-- There is only one ModGuts at any time, the one for the module
-- being compiled right now.  Once it is compiled, a 'ModIface' and
-- 'ModDetails' are extracted and the ModGuts is discarded.
data ModGuts
  = ModGuts {
        mg_module    :: !Module,         -- ^ Module being compiled
        mg_hsc_src   :: HscSource,       -- ^ Whether it's an hs-boot module
        mg_loc       :: SrcSpan,         -- ^ For error messages from inner passes
        mg_exports   :: ![AvailInfo],    -- ^ What it exports
        mg_deps      :: !Dependencies,   -- ^ What it depends on, directly or
                                         -- otherwise
        mg_usages    :: ![Usage],        -- ^ What was used?  Used for interfaces.

        mg_used_th   :: !Bool,           -- ^ Did we run a TH splice?
        mg_rdr_env   :: !GlobalRdrEnv,   -- ^ Top-level lexical environment

        -- These fields all describe the things **declared in this module**
        mg_fix_env   :: !FixityEnv,      -- ^ Fixities declared in this module.
                                         -- Used for creating interface files.
        mg_tcs       :: ![TyCon],        -- ^ TyCons declared in this module
                                         -- (includes TyCons for classes)
        mg_insts     :: ![ClsInst],      -- ^ Class instances declared in this module
        mg_fam_insts :: ![FamInst],
                                         -- ^ Family instances declared in this module
        mg_patsyns   :: ![PatSyn],       -- ^ Pattern synonyms declared in this module
        mg_rules     :: ![CoreRule],     -- ^ Before the core pipeline starts, contains
                                         -- See Note [Overall plumbing for rules] in "GHC.Core.Rules"
        mg_binds     :: !CoreProgram,    -- ^ Bindings for this module
        mg_foreign   :: !ForeignStubs,   -- ^ Foreign exports declared in this module
        mg_foreign_files :: ![(ForeignSrcLang, FilePath)],
        -- ^ Files to be compiled with the C compiler
        mg_warns     :: !Warnings,       -- ^ Warnings declared in the module
        mg_anns      :: [Annotation],    -- ^ Annotations declared in this module
        mg_complete_matches :: [CompleteMatch], -- ^ Complete Matches
        mg_hpc_info  :: !HpcInfo,        -- ^ Coverage tick boxes in the module
        mg_modBreaks :: !(Maybe ModBreaks), -- ^ Breakpoints for the module

                        -- The next two fields are unusual, because they give instance
                        -- environments for *all* modules in the home package, including
                        -- this module, rather than for *just* this module.
                        -- Reason: when looking up an instance we don't want to have to
                        --         look at each module in the home package in turn
        mg_inst_env     :: InstEnv,             -- ^ Class instance environment for
                                                -- /home-package/ modules (including this
                                                -- one); c.f. 'tcg_inst_env'
        mg_fam_inst_env :: FamInstEnv,          -- ^ Type-family instance environment for
                                                -- /home-package/ modules (including this
                                                -- one); c.f. 'tcg_fam_inst_env'

        mg_safe_haskell :: SafeHaskellMode,     -- ^ Safe Haskell mode
        mg_trust_pkg    :: Bool,                -- ^ Do we need to trust our
                                                -- own package for Safe Haskell?
                                                -- See Note [Trust Own Package]
                                                -- in "GHC.Rename.Names"

        mg_doc_hdr       :: !(Maybe HsDocString), -- ^ Module header.
        mg_decl_docs     :: !DeclDocMap,     -- ^ Docs on declarations.
        mg_arg_docs      :: !ArgDocMap       -- ^ Docs on arguments.
    }

-- The ModGuts takes on several slightly different forms:
--
-- After simplification, the following fields change slightly:
--      mg_rules        Orphan rules only (local ones now attached to binds)
--      mg_binds        With rules attached

---------------------------------------------------------
-- The Tidy pass forks the information about this module:
--      * one lot goes to interface file generation (ModIface)
--        and later compilations (ModDetails)
--      * the other lot goes to code generation (CgGuts)

-- | A restricted form of 'ModGuts' for code generation purposes
data CgGuts
  = CgGuts {
        cg_module    :: !Module,
                -- ^ Module being compiled

        cg_tycons    :: [TyCon],
                -- ^ Algebraic data types (including ones that started
                -- life as classes); generate constructors and info
                -- tables. Includes newtypes, just for the benefit of
                -- External Core

        cg_binds     :: CoreProgram,
                -- ^ The tidied main bindings, including
                -- previously-implicit bindings for record and class
                -- selectors, and data constructor wrappers.  But *not*
                -- data constructor workers; reason: we regard them
                -- as part of the code-gen of tycons

        cg_foreign   :: !ForeignStubs,   -- ^ Foreign export stubs
        cg_foreign_files :: ![(ForeignSrcLang, FilePath)],
        cg_dep_pkgs  :: ![UnitId], -- ^ Dependent packages, used to
                                            -- generate #includes for C code gen
        cg_hpc_info  :: !HpcInfo,           -- ^ Program coverage tick box information
        cg_modBreaks :: !(Maybe ModBreaks), -- ^ Module breakpoints
        cg_spt_entries :: [SptEntry]
                -- ^ Static pointer table entries for static forms defined in
                -- the module.
                -- See Note [Grand plan for static forms] in "GHC.Iface.Tidy.StaticPtrTable"
    }
