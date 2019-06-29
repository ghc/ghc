module GHC.Driver.Flags
   ( DumpFlag(..)
   , GeneralFlag(..)
   , WarningFlag(..)
   , WarnReason (..)
   , Language(..)
   , optimisationFlags
   )
where

import GhcPrelude
import Outputable
import EnumSet
import Json

-- | Debugging flags
data DumpFlag
-- See Note [Updating flag description in the User's Guide]

   -- debugging flags
   = Opt_D_dump_cmm
   | Opt_D_dump_cmm_from_stg
   | Opt_D_dump_cmm_raw
   | Opt_D_dump_cmm_verbose_by_proc
   -- All of the cmm subflags (there are a lot!) automatically
   -- enabled if you run -ddump-cmm-verbose-by-proc
   -- Each flag corresponds to exact stage of Cmm pipeline.
   | Opt_D_dump_cmm_verbose
   -- same as -ddump-cmm-verbose-by-proc but writes each stage
   -- to a separate file (if used with -ddump-to-file)
   | Opt_D_dump_cmm_cfg
   | Opt_D_dump_cmm_cbe
   | Opt_D_dump_cmm_switch
   | Opt_D_dump_cmm_proc
   | Opt_D_dump_cmm_sp
   | Opt_D_dump_cmm_sink
   | Opt_D_dump_cmm_caf
   | Opt_D_dump_cmm_procmap
   | Opt_D_dump_cmm_split
   | Opt_D_dump_cmm_info
   | Opt_D_dump_cmm_cps
   -- end cmm subflags
   | Opt_D_dump_cfg_weights -- ^ Dump the cfg used for block layout.
   | Opt_D_dump_asm
   | Opt_D_dump_asm_native
   | Opt_D_dump_asm_liveness
   | Opt_D_dump_asm_regalloc
   | Opt_D_dump_asm_regalloc_stages
   | Opt_D_dump_asm_conflicts
   | Opt_D_dump_asm_stats
   | Opt_D_dump_asm_expanded
   | Opt_D_dump_llvm
   | Opt_D_dump_core_stats
   | Opt_D_dump_deriv
   | Opt_D_dump_ds
   | Opt_D_dump_ds_preopt
   | Opt_D_dump_foreign
   | Opt_D_dump_inlinings
   | Opt_D_dump_rule_firings
   | Opt_D_dump_rule_rewrites
   | Opt_D_dump_simpl_trace
   | Opt_D_dump_occur_anal
   | Opt_D_dump_parsed
   | Opt_D_dump_parsed_ast
   | Opt_D_dump_rn
   | Opt_D_dump_rn_ast
   | Opt_D_dump_simpl
   | Opt_D_dump_simpl_iterations
   | Opt_D_dump_spec
   | Opt_D_dump_prep
   | Opt_D_dump_stg -- CoreToStg output
   | Opt_D_dump_stg_unarised -- STG after unarise
   | Opt_D_dump_stg_final -- STG after stg2stg
   | Opt_D_dump_call_arity
   | Opt_D_dump_exitify
   | Opt_D_dump_stranal
   | Opt_D_dump_str_signatures
   | Opt_D_dump_cpranal
   | Opt_D_dump_cpr_signatures
   | Opt_D_dump_tc
   | Opt_D_dump_tc_ast
   | Opt_D_dump_hie
   | Opt_D_dump_types
   | Opt_D_dump_rules
   | Opt_D_dump_cse
   | Opt_D_dump_worker_wrapper
   | Opt_D_dump_rn_trace
   | Opt_D_dump_rn_stats
   | Opt_D_dump_opt_cmm
   | Opt_D_dump_simpl_stats
   | Opt_D_dump_cs_trace -- Constraint solver in type checker
   | Opt_D_dump_tc_trace
   | Opt_D_dump_ec_trace -- Pattern match exhaustiveness checker
   | Opt_D_dump_if_trace
   | Opt_D_dump_vt_trace
   | Opt_D_dump_splices
   | Opt_D_th_dec_file
   | Opt_D_dump_BCOs
   | Opt_D_dump_ticked
   | Opt_D_dump_rtti
   | Opt_D_source_stats
   | Opt_D_verbose_stg2stg
   | Opt_D_dump_hi
   | Opt_D_dump_hi_diffs
   | Opt_D_dump_mod_cycles
   | Opt_D_dump_mod_map
   | Opt_D_dump_timings
   | Opt_D_dump_view_pattern_commoning
   | Opt_D_verbose_core2core
   | Opt_D_dump_debug
   | Opt_D_dump_json
   | Opt_D_ppr_debug
   | Opt_D_no_debug_output
   deriving (Eq, Show, Enum)

-- | Enumerates the simple on-or-off dynamic flags
data GeneralFlag
-- See Note [Updating flag description in the User's Guide]

   = Opt_DumpToFile                     -- ^ Append dump output to files instead of stdout.
   | Opt_D_faststring_stats
   | Opt_D_dump_minimal_imports
   | Opt_DoCoreLinting
   | Opt_DoStgLinting
   | Opt_DoCmmLinting
   | Opt_DoAsmLinting
   | Opt_DoAnnotationLinting
   | Opt_NoLlvmMangler                  -- hidden flag
   | Opt_FastLlvm                       -- hidden flag
   | Opt_NoTypeableBinds

   | Opt_WarnIsError                    -- -Werror; makes warnings fatal
   | Opt_ShowWarnGroups                 -- Show the group a warning belongs to
   | Opt_HideSourcePaths                -- Hide module source/object paths

   | Opt_PrintExplicitForalls
   | Opt_PrintExplicitKinds
   | Opt_PrintExplicitCoercions
   | Opt_PrintExplicitRuntimeReps
   | Opt_PrintEqualityRelations
   | Opt_PrintAxiomIncomps
   | Opt_PrintUnicodeSyntax
   | Opt_PrintExpandedSynonyms
   | Opt_PrintPotentialInstances
   | Opt_PrintTypecheckerElaboration

   -- optimisation opts
   | Opt_CallArity
   | Opt_Exitification
   | Opt_Strictness
   | Opt_LateDmdAnal                    -- #6087
   | Opt_KillAbsence
   | Opt_KillOneShot
   | Opt_FullLaziness
   | Opt_FloatIn
   | Opt_LateSpecialise
   | Opt_Specialise
   | Opt_SpecialiseAggressively
   | Opt_CrossModuleSpecialise
   | Opt_StaticArgumentTransformation
   | Opt_CSE
   | Opt_StgCSE
   | Opt_StgLiftLams
   | Opt_LiberateCase
   | Opt_SpecConstr
   | Opt_SpecConstrKeen
   | Opt_DoLambdaEtaExpansion
   | Opt_IgnoreAsserts
   | Opt_DoEtaReduction
   | Opt_CaseMerge
   | Opt_CaseFolding                    -- Constant folding through case-expressions
   | Opt_UnboxStrictFields
   | Opt_UnboxSmallStrictFields
   | Opt_DictsCheap
   | Opt_EnableRewriteRules             -- Apply rewrite rules during simplification
   | Opt_EnableThSpliceWarnings         -- Enable warnings for TH splices
   | Opt_RegsGraph                      -- do graph coloring register allocation
   | Opt_RegsIterative                  -- do iterative coalescing graph coloring register allocation
   | Opt_PedanticBottoms                -- Be picky about how we treat bottom
   | Opt_LlvmTBAA                       -- Use LLVM TBAA infrastructure for improving AA (hidden flag)
   | Opt_LlvmFillUndefWithGarbage       -- Testing for undef bugs (hidden flag)
   | Opt_IrrefutableTuples
   | Opt_CmmSink
   | Opt_CmmElimCommonBlocks
   | Opt_AsmShortcutting
   | Opt_OmitYields
   | Opt_FunToThunk               -- allow GHC.Core.Op.WorkWrap.Lib.mkWorkerArgs to remove all value lambdas
   | Opt_DictsStrict                     -- be strict in argument dictionaries
   | Opt_DmdTxDictSel              -- use a special demand transformer for dictionary selectors
   | Opt_Loopification                  -- See Note [Self-recursive tail calls]
   | Opt_CfgBlocklayout             -- ^ Use the cfg based block layout algorithm.
   | Opt_WeightlessBlocklayout         -- ^ Layout based on last instruction per block.
   | Opt_CprAnal
   | Opt_WorkerWrapper
   | Opt_SolveConstantDicts
   | Opt_AlignmentSanitisation
   | Opt_CatchBottoms
   | Opt_NumConstantFolding

   -- PreInlining is on by default. The option is there just to see how
   -- bad things get if you turn it off!
   | Opt_SimplPreInlining

   -- Interface files
   | Opt_IgnoreInterfacePragmas
   | Opt_OmitInterfacePragmas
   | Opt_ExposeAllUnfoldings
   | Opt_WriteInterface -- forces .hi files to be written even with -fno-code
   | Opt_WriteHie -- generate .hie files

   -- profiling opts
   | Opt_AutoSccsOnIndividualCafs
   | Opt_ProfCountEntries

   -- misc opts
   | Opt_Pp
   | Opt_ForceRecomp
   | Opt_IgnoreOptimChanges
   | Opt_IgnoreHpcChanges
   | Opt_ExcessPrecision
   | Opt_EagerBlackHoling
   | Opt_NoHsMain
   | Opt_SplitSections
   | Opt_StgStats
   | Opt_HideAllPackages
   | Opt_HideAllPluginPackages
   | Opt_PrintBindResult
   | Opt_Haddock
   | Opt_HaddockOptions
   | Opt_BreakOnException
   | Opt_BreakOnError
   | Opt_PrintEvldWithShow
   | Opt_PrintBindContents
   | Opt_GenManifest
   | Opt_EmbedManifest
   | Opt_SharedImplib
   | Opt_BuildingCabalPackage
   | Opt_IgnoreDotGhci
   | Opt_GhciSandbox
   | Opt_GhciHistory
   | Opt_GhciLeakCheck
   | Opt_ValidateHie
   | Opt_LocalGhciHistory
   | Opt_NoIt
   | Opt_HelpfulErrors
   | Opt_DeferTypeErrors
   | Opt_DeferTypedHoles
   | Opt_DeferOutOfScopeVariables
   | Opt_PIC                         -- ^ @-fPIC@
   | Opt_PIE                         -- ^ @-fPIE@
   | Opt_PICExecutable               -- ^ @-pie@
   | Opt_ExternalDynamicRefs
   | Opt_SccProfilingOn
   | Opt_Ticky
   | Opt_Ticky_Allocd
   | Opt_Ticky_LNE
   | Opt_Ticky_Dyn_Thunk
   | Opt_RPath
   | Opt_RelativeDynlibPaths
   | Opt_Hpc
   | Opt_FlatCache
   | Opt_ExternalInterpreter
   | Opt_OptimalApplicativeDo
   | Opt_VersionMacros
   | Opt_WholeArchiveHsLibs
   -- copy all libs into a single folder prior to linking binaries
   -- this should elivate the excessive command line limit restrictions
   -- on windows, by only requiring a single -L argument instead of
   -- one for each dependency.  At the time of this writing, gcc
   -- forwards all -L flags to the collect2 command without using a
   -- response file and as such breaking apart.
   | Opt_SingleLibFolder
   | Opt_KeepCAFs
   | Opt_KeepGoing
   | Opt_ByteCode

   -- output style opts
   | Opt_ErrorSpans -- Include full span info in error messages,
                    -- instead of just the start position.
   | Opt_DeferDiagnostics
   | Opt_DiagnosticsShowCaret -- Show snippets of offending code
   | Opt_PprCaseAsLet
   | Opt_PprShowTicks
   | Opt_ShowHoleConstraints
    -- Options relating to the display of valid hole fits
    -- when generating an error message for a typed hole
    -- See Note [Valid hole fits include] in TcHoleErrors.hs
   | Opt_ShowValidHoleFits
   | Opt_SortValidHoleFits
   | Opt_SortBySizeHoleFits
   | Opt_SortBySubsumHoleFits
   | Opt_AbstractRefHoleFits
   | Opt_UnclutterValidHoleFits
   | Opt_ShowTypeAppOfHoleFits
   | Opt_ShowTypeAppVarsOfHoleFits
   | Opt_ShowDocsOfHoleFits
   | Opt_ShowTypeOfHoleFits
   | Opt_ShowProvOfHoleFits
   | Opt_ShowMatchesOfHoleFits

   | Opt_ShowLoadedModules
   | Opt_HexWordLiterals -- See Note [Print Hexadecimal Literals]

   -- Suppress all coercions, them replacing with '...'
   | Opt_SuppressCoercions
   | Opt_SuppressVarKinds
   -- Suppress module id prefixes on variables.
   | Opt_SuppressModulePrefixes
   -- Suppress type applications.
   | Opt_SuppressTypeApplications
   -- Suppress info such as arity and unfoldings on identifiers.
   | Opt_SuppressIdInfo
   -- Suppress separate type signatures in core, but leave types on
   -- lambda bound vars
   | Opt_SuppressUnfoldings
   -- Suppress the details of even stable unfoldings
   | Opt_SuppressTypeSignatures
   -- Suppress unique ids on variables.
   -- Except for uniques, as some simplifier phases introduce new
   -- variables that have otherwise identical names.
   | Opt_SuppressUniques
   | Opt_SuppressStgExts
   | Opt_SuppressTicks     -- Replaces Opt_PprShowTicks
   | Opt_SuppressTimestamps -- ^ Suppress timestamps in dumps

   -- temporary flags
   | Opt_AutoLinkPackages
   | Opt_ImplicitImportQualified

   -- keeping stuff
   | Opt_KeepHscppFiles
   | Opt_KeepHiDiffs
   | Opt_KeepHcFiles
   | Opt_KeepSFiles
   | Opt_KeepTmpFiles
   | Opt_KeepRawTokenStream
   | Opt_KeepLlvmFiles
   | Opt_KeepHiFiles
   | Opt_KeepOFiles

   | Opt_BuildDynamicToo

   -- safe haskell flags
   | Opt_DistrustAllPackages
   | Opt_PackageTrust
   | Opt_PluginTrustworthy

   | Opt_G_NoStateHack
   | Opt_G_NoOptCoercion
   deriving (Eq, Show, Enum)

-- Check whether a flag should be considered an "optimisation flag"
-- for purposes of recompilation avoidance (see
-- Note [Ignoring some flag changes] in FlagChecker). Being listed here is
-- not a guarantee that the flag has no other effect. We could, and
-- perhaps should, separate out the flags that have some minor impact on
-- program semantics and/or error behavior (e.g., assertions), but
-- then we'd need to go to extra trouble (and an additional flag)
-- to allow users to ignore the optimisation level even though that
-- means ignoring some change.
optimisationFlags :: EnumSet GeneralFlag
optimisationFlags = EnumSet.fromList
   [ Opt_CallArity
   , Opt_Strictness
   , Opt_LateDmdAnal
   , Opt_KillAbsence
   , Opt_KillOneShot
   , Opt_FullLaziness
   , Opt_FloatIn
   , Opt_LateSpecialise
   , Opt_Specialise
   , Opt_SpecialiseAggressively
   , Opt_CrossModuleSpecialise
   , Opt_StaticArgumentTransformation
   , Opt_CSE
   , Opt_StgCSE
   , Opt_StgLiftLams
   , Opt_LiberateCase
   , Opt_SpecConstr
   , Opt_SpecConstrKeen
   , Opt_DoLambdaEtaExpansion
   , Opt_IgnoreAsserts
   , Opt_DoEtaReduction
   , Opt_CaseMerge
   , Opt_CaseFolding
   , Opt_UnboxStrictFields
   , Opt_UnboxSmallStrictFields
   , Opt_DictsCheap
   , Opt_EnableRewriteRules
   , Opt_RegsGraph
   , Opt_RegsIterative
   , Opt_PedanticBottoms
   , Opt_LlvmTBAA
   , Opt_LlvmFillUndefWithGarbage
   , Opt_IrrefutableTuples
   , Opt_CmmSink
   , Opt_CmmElimCommonBlocks
   , Opt_AsmShortcutting
   , Opt_OmitYields
   , Opt_FunToThunk
   , Opt_DictsStrict
   , Opt_DmdTxDictSel
   , Opt_Loopification
   , Opt_CfgBlocklayout
   , Opt_WeightlessBlocklayout
   , Opt_CprAnal
   , Opt_WorkerWrapper
   , Opt_SolveConstantDicts
   , Opt_CatchBottoms
   , Opt_IgnoreAsserts
   ]

data WarningFlag =
-- See Note [Updating flag description in the User's Guide]
     Opt_WarnDuplicateExports
   | Opt_WarnDuplicateConstraints
   | Opt_WarnRedundantConstraints
   | Opt_WarnHiShadows
   | Opt_WarnImplicitPrelude
   | Opt_WarnIncompletePatterns
   | Opt_WarnIncompleteUniPatterns
   | Opt_WarnIncompletePatternsRecUpd
   | Opt_WarnOverflowedLiterals
   | Opt_WarnEmptyEnumerations
   | Opt_WarnMissingFields
   | Opt_WarnMissingImportList
   | Opt_WarnMissingMethods
   | Opt_WarnMissingSignatures
   | Opt_WarnMissingLocalSignatures
   | Opt_WarnNameShadowing
   | Opt_WarnOverlappingPatterns
   | Opt_WarnTypeDefaults
   | Opt_WarnMonomorphism
   | Opt_WarnUnusedTopBinds
   | Opt_WarnUnusedLocalBinds
   | Opt_WarnUnusedPatternBinds
   | Opt_WarnUnusedImports
   | Opt_WarnUnusedMatches
   | Opt_WarnUnusedTypePatterns
   | Opt_WarnUnusedForalls
   | Opt_WarnUnusedRecordWildcards
   | Opt_WarnRedundantRecordWildcards
   | Opt_WarnWarningsDeprecations
   | Opt_WarnDeprecatedFlags
   | Opt_WarnMissingMonadFailInstances -- since 8.0, has no effect since 8.8
   | Opt_WarnSemigroup -- since 8.0
   | Opt_WarnDodgyExports
   | Opt_WarnDodgyImports
   | Opt_WarnOrphans
   | Opt_WarnAutoOrphans
   | Opt_WarnIdentities
   | Opt_WarnTabs
   | Opt_WarnUnrecognisedPragmas
   | Opt_WarnDodgyForeignImports
   | Opt_WarnUnusedDoBind
   | Opt_WarnWrongDoBind
   | Opt_WarnAlternativeLayoutRuleTransitional
   | Opt_WarnUnsafe
   | Opt_WarnSafe
   | Opt_WarnTrustworthySafe
   | Opt_WarnMissedSpecs
   | Opt_WarnAllMissedSpecs
   | Opt_WarnUnsupportedCallingConventions
   | Opt_WarnUnsupportedLlvmVersion
   | Opt_WarnMissedExtraSharedLib
   | Opt_WarnInlineRuleShadowing
   | Opt_WarnTypedHoles
   | Opt_WarnPartialTypeSignatures
   | Opt_WarnMissingExportedSignatures
   | Opt_WarnUntickedPromotedConstructors
   | Opt_WarnDerivingTypeable
   | Opt_WarnDeferredTypeErrors
   | Opt_WarnDeferredOutOfScopeVariables
   | Opt_WarnNonCanonicalMonadInstances   -- since 8.0
   | Opt_WarnNonCanonicalMonadFailInstances   -- since 8.0, removed 8.8
   | Opt_WarnNonCanonicalMonoidInstances  -- since 8.0
   | Opt_WarnMissingPatternSynonymSignatures -- since 8.0
   | Opt_WarnUnrecognisedWarningFlags     -- since 8.0
   | Opt_WarnSimplifiableClassConstraints -- Since 8.2
   | Opt_WarnCPPUndef                     -- Since 8.2
   | Opt_WarnUnbangedStrictPatterns       -- Since 8.2
   | Opt_WarnMissingHomeModules           -- Since 8.2
   | Opt_WarnPartialFields                -- Since 8.4
   | Opt_WarnMissingExportList
   | Opt_WarnInaccessibleCode
   | Opt_WarnStarIsType                   -- Since 8.6
   | Opt_WarnStarBinder                   -- Since 8.6
   | Opt_WarnImplicitKindVars             -- Since 8.6
   | Opt_WarnSpaceAfterBang
   | Opt_WarnMissingDerivingStrategies    -- Since 8.8
   | Opt_WarnPrepositiveQualifiedModule   -- Since TBD
   | Opt_WarnUnusedPackages               -- Since 8.10
   | Opt_WarnInferredSafeImports          -- Since 8.10
   | Opt_WarnMissingSafeHaskellMode       -- Since 8.10
   | Opt_WarnCompatUnqualifiedImports     -- Since 8.10
   | Opt_WarnDerivingDefaults
   deriving (Eq, Show, Enum)

-- | Used when outputting warnings: if a reason is given, it is
-- displayed. If a warning isn't controlled by a flag, this is made
-- explicit at the point of use.
data WarnReason
  = NoReason
  -- | Warning was enabled with the flag
  | Reason !WarningFlag
  -- | Warning was made an error because of -Werror or -Werror=WarningFlag
  | ErrReason !(Maybe WarningFlag)
  deriving Show

instance Outputable WarnReason where
  ppr = text . show

instance ToJson WarnReason where
  json NoReason = JSNull
  json (Reason wf) = JSString (show wf)
  json (ErrReason Nothing) = JSString "Opt_WarnIsError"
  json (ErrReason (Just wf)) = JSString (show wf)


data Language = Haskell98 | Haskell2010
   deriving (Eq, Enum, Show)

instance Outputable Language where
    ppr = text . show

