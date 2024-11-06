module GHC.Driver.Flags
   ( DumpFlag(..)
   , getDumpFlagFrom
   , enabledIfVerbose
   , GeneralFlag(..)
   , Language(..)
   , optimisationFlags
   , codeGenFlags

   -- * Warnings
   , WarningFlag(..)
   , warnFlagNames
   , warningGroups
   , warningHierarchies
   , smallestWarningGroups
   , standardWarnings
   , minusWOpts
   , minusWallOpts
   , minusWeverythingOpts
   , minusWcompatOpts
   , unusedBindsFlags
   )
where

import GHC.Prelude
import GHC.Utils.Outputable
import GHC.Utils.Binary
import GHC.Data.EnumSet as EnumSet

import Control.DeepSeq
import Control.Monad (guard)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe,mapMaybe)

data Language = Haskell98 | Haskell2010 | GHC2021
   deriving (Eq, Enum, Show, Bounded)

instance Outputable Language where
    ppr = text . show

instance Binary Language where
  put_ bh = put_ bh . fromEnum
  get bh = toEnum <$> get bh

instance NFData Language where
  rnf x = x `seq` ()

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
   | Opt_D_dump_cmm_thread_sanitizer
   -- end cmm subflags
   | Opt_D_dump_cfg_weights -- ^ Dump the cfg used for block layout.
   | Opt_D_dump_asm
   | Opt_D_dump_asm_native
   | Opt_D_dump_asm_liveness
   | Opt_D_dump_asm_regalloc
   | Opt_D_dump_asm_regalloc_stages
   | Opt_D_dump_asm_conflicts
   | Opt_D_dump_asm_stats
   | Opt_D_dump_c_backend
   | Opt_D_dump_llvm
   | Opt_D_dump_js
   | Opt_D_dump_core_stats
   | Opt_D_dump_deriv
   | Opt_D_dump_ds
   | Opt_D_dump_ds_preopt
   | Opt_D_dump_foreign
   | Opt_D_dump_inlinings
   | Opt_D_dump_verbose_inlinings
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
   | Opt_D_dump_late_cc
   | Opt_D_dump_stg_from_core -- ^ Initial STG (CoreToStg output)
   | Opt_D_dump_stg_unarised  -- ^ STG after unarise
   | Opt_D_dump_stg_cg        -- ^ STG (after stg2stg)
   | Opt_D_dump_stg_tags      -- ^ Result of tag inference analysis.
   | Opt_D_dump_stg_final     -- ^ Final STG (before cmm gen)
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
   | Opt_D_dump_faststrings
   | Opt_D_faststring_stats
   | Opt_D_ipe_stats
   deriving (Eq, Show, Enum)

-- | Helper function to query whether a given `DumpFlag` is enabled or not.
getDumpFlagFrom
  :: (a -> Int) -- ^ Getter for verbosity setting
  -> (a -> EnumSet DumpFlag) -- ^ Getter for the set of enabled dump flags
  -> DumpFlag -> a -> Bool
getDumpFlagFrom getVerbosity getFlags f x
  =  (f `EnumSet.member` getFlags x)
  || (getVerbosity x >= 4 && enabledIfVerbose f)

-- | Is the flag implicitly enabled when the verbosity is high enough?
enabledIfVerbose :: DumpFlag -> Bool
enabledIfVerbose Opt_D_dump_tc_trace               = False
enabledIfVerbose Opt_D_dump_rn_trace               = False
enabledIfVerbose Opt_D_dump_cs_trace               = False
enabledIfVerbose Opt_D_dump_if_trace               = False
enabledIfVerbose Opt_D_dump_tc                     = False
enabledIfVerbose Opt_D_dump_rn                     = False
enabledIfVerbose Opt_D_dump_rn_stats               = False
enabledIfVerbose Opt_D_dump_hi_diffs               = False
enabledIfVerbose Opt_D_verbose_core2core           = False
enabledIfVerbose Opt_D_verbose_stg2stg             = False
enabledIfVerbose Opt_D_dump_splices                = False
enabledIfVerbose Opt_D_th_dec_file                 = False
enabledIfVerbose Opt_D_dump_rule_firings           = False
enabledIfVerbose Opt_D_dump_rule_rewrites          = False
enabledIfVerbose Opt_D_dump_simpl_trace            = False
enabledIfVerbose Opt_D_dump_rtti                   = False
enabledIfVerbose Opt_D_dump_inlinings              = False
enabledIfVerbose Opt_D_dump_verbose_inlinings      = False
enabledIfVerbose Opt_D_dump_core_stats             = False
enabledIfVerbose Opt_D_dump_asm_stats              = False
enabledIfVerbose Opt_D_dump_types                  = False
enabledIfVerbose Opt_D_dump_simpl_iterations       = False
enabledIfVerbose Opt_D_dump_ticked                 = False
enabledIfVerbose Opt_D_dump_view_pattern_commoning = False
enabledIfVerbose Opt_D_dump_mod_cycles             = False
enabledIfVerbose Opt_D_dump_mod_map                = False
enabledIfVerbose Opt_D_dump_ec_trace               = False
enabledIfVerbose _                                 = True

-- | Enumerates the simple on-or-off dynamic flags
data GeneralFlag
-- See Note [Updating flag description in the User's Guide]

   = Opt_DumpToFile                     -- ^ Append dump output to files instead of stdout.
   | Opt_DumpWithWays                   -- ^ Use foo.ways.<dumpFlag> instead of foo.<dumpFlag>
   | Opt_D_dump_minimal_imports
   | Opt_DoCoreLinting
   | Opt_DoLinearCoreLinting
   | Opt_DoStgLinting
   | Opt_DoCmmLinting
   | Opt_DoAsmLinting
   | Opt_DoAnnotationLinting
   | Opt_DoBoundsChecking
   | Opt_NoLlvmMangler                  -- hidden flag
   | Opt_FastLlvm                       -- hidden flag
   | Opt_NoTypeableBinds

   | Opt_DistinctConstructorTables
   | Opt_InfoTableMap
   | Opt_InfoTableMapWithFallback
   | Opt_InfoTableMapWithStack

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
   | Opt_PrintRedundantPromotionTicks
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
   | Opt_LocalFloatOut -- ^ Enable floating out of let-bindings in the
                      --   simplifier
   | Opt_LocalFloatOutTopLevel -- ^ Enable floating out of let-bindings at the
                               --   top level in the simplifier
                               --   N.B. See Note [RHS Floating]
   | Opt_LateSpecialise
   | Opt_Specialise
   | Opt_SpecialiseAggressively
   | Opt_CrossModuleSpecialise
   | Opt_PolymorphicSpecialisation
   | Opt_InlineGenerics
   | Opt_InlineGenericsAggressively
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
   | Opt_CmmStaticPred
   | Opt_CmmElimCommonBlocks
   | Opt_CmmControlFlow
   | Opt_AsmShortcutting
   | Opt_OmitYields
   | Opt_FunToThunk               -- deprecated
   | Opt_DictsStrict                     -- be strict in argument dictionaries
   | Opt_DmdTxDictSel              -- ^ deprecated, no effect and behaviour is now default.
                                   -- Allowed switching of a special demand transformer for dictionary selectors
   | Opt_Loopification                  -- See Note [Self-recursive tail calls]
   | Opt_CfgBlocklayout             -- ^ Use the cfg based block layout algorithm.
   | Opt_WeightlessBlocklayout         -- ^ Layout based on last instruction per block.
   | Opt_CprAnal
   | Opt_WorkerWrapper
   | Opt_WorkerWrapperUnlift  -- ^ Do W/W split for unlifting even if we won't unbox anything.
   | Opt_SolveConstantDicts
   | Opt_AlignmentSanitisation
   | Opt_CatchNonexhaustiveCases
   | Opt_NumConstantFolding
   | Opt_CoreConstantFolding
   | Opt_FastPAPCalls                  -- #6084
   | Opt_SpecEval
   | Opt_SpecEvalDictFun   -- See Note [Controlling Speculative Evaluation]


   -- Inference flags
   | Opt_DoTagInferenceChecks

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
   | Opt_ProfLateInlineCcs
   | Opt_ProfLateCcs
   | Opt_ProfManualCcs -- ^ Ignore manual SCC annotations

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
   | Opt_DeferTypeErrors             -- Since 7.6
   | Opt_DeferTypedHoles             -- Since 7.10
   | Opt_DeferOutOfScopeVariables
   | Opt_PIC                         -- ^ @-fPIC@
   | Opt_PIE                         -- ^ @-fPIE@
   | Opt_PICExecutable               -- ^ @-pie@
   | Opt_ExternalDynamicRefs
   | Opt_Ticky
   | Opt_Ticky_Allocd
   | Opt_Ticky_LNE
   | Opt_Ticky_Dyn_Thunk
   | Opt_Ticky_Tag
   | Opt_Ticky_AP                    -- ^ Use regular thunks even when we could use std ap thunks in order to get entry counts
   | Opt_CmmThreadSanitizer
   | Opt_RPath
   | Opt_RelativeDynlibPaths
   | Opt_CompactUnwind               -- ^ @-fcompact-unwind@
   | Opt_Hpc
   | Opt_FamAppCache
   | Opt_ExternalInterpreter
   | Opt_OptimalApplicativeDo
   | Opt_VersionMacros
   | Opt_WholeArchiveHsLibs
   -- copy all libs into a single folder prior to linking binaries
   -- this should alleviate the excessive command line limit restrictions
   -- on windows, by only requiring a single -L argument instead of
   -- one for each dependency.  At the time of this writing, gcc
   -- forwards all -L flags to the collect2 command without using a
   -- response file and as such breaking apart.
   | Opt_SingleLibFolder
   | Opt_ExposeInternalSymbols
   | Opt_KeepCAFs
   | Opt_KeepGoing
   | Opt_ByteCode
   | Opt_ByteCodeAndObjectCode
   | Opt_LinkRts

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
    -- See Note [Valid hole fits include ...] in GHC.Tc.Errors.Hole
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

   -- Suppress a coercions inner structure, replacing it with '...'
   | Opt_SuppressCoercions
   -- Suppress the type of a coercion as well
   | Opt_SuppressCoercionTypes
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
   | Opt_SuppressStgReps
   | Opt_SuppressTicks     -- Replaces Opt_PprShowTicks
   | Opt_SuppressTimestamps -- ^ Suppress timestamps in dumps
   | Opt_SuppressCoreSizes  -- ^ Suppress per binding Core size stats in dumps

   -- Error message suppression
   | Opt_ShowErrorContext

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
   | Opt_WriteIfSimplifiedCore
   | Opt_UseBytecodeRatherThanObjects

   -- safe haskell flags
   | Opt_DistrustAllPackages
   | Opt_PackageTrust
   | Opt_PluginTrustworthy

   | Opt_G_NoStateHack
   | Opt_G_NoOptCoercion
   deriving (Eq, Show, Enum)

-- | The set of flags which affect optimisation for the purposes of
-- recompilation avoidance. Specifically, these include flags which
-- affect code generation but not the semantics of the program.
--
-- See Note [Ignoring some flag changes] in GHC.Iface.Recomp.Flags)
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
   , Opt_LlvmTBAA
   , Opt_IrrefutableTuples
   , Opt_CmmSink
   , Opt_CmmElimCommonBlocks
   , Opt_AsmShortcutting
   , Opt_FunToThunk
   , Opt_DmdTxDictSel
   , Opt_Loopification
   , Opt_CfgBlocklayout
   , Opt_WeightlessBlocklayout
   , Opt_CprAnal
   , Opt_WorkerWrapper
   , Opt_WorkerWrapperUnlift
   , Opt_SolveConstantDicts
   , Opt_SpecEval
   , Opt_SpecEvalDictFun
   ]

-- | The set of flags which affect code generation and can change a program's
-- runtime behavior (other than performance). These include flags which affect:
--
--  * user visible debugging information (e.g. info table provenance)
--  * the ability to catch runtime errors (e.g. -fignore-asserts)
--  * the runtime result of the program (e.g. -fomit-yields)
--  * which code or interface file declarations are emitted
--
-- We also considered placing flags which affect asympototic space behavior
-- (e.g. -ffull-laziness) however this would mean that changing optimisation
-- levels would trigger recompilation even with -fignore-optim-changes,
-- regressing #13604.
--
-- Also, arguably Opt_IgnoreAsserts should be here as well; however, we place
-- it instead in 'optimisationFlags' since it is implied by @-O[12]@ and
-- therefore would also break #13604.
--
-- See #23369.
codeGenFlags :: EnumSet GeneralFlag
codeGenFlags = EnumSet.fromList
   [ -- Flags that affect runtime result
     Opt_EagerBlackHoling
   , Opt_ExcessPrecision
   , Opt_DictsStrict
   , Opt_PedanticBottoms
   , Opt_OmitYields

     -- Flags that affect generated code
   , Opt_ExposeAllUnfoldings
   , Opt_NoTypeableBinds

     -- Flags that affect catching of runtime errors
   , Opt_CatchNonexhaustiveCases
   , Opt_LlvmFillUndefWithGarbage
   , Opt_DoTagInferenceChecks

     -- Flags that affect debugging information
   , Opt_DistinctConstructorTables
   , Opt_InfoTableMap
   , Opt_InfoTableMapWithStack
   , Opt_InfoTableMapWithFallback
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
   | Opt_WarnRedundantBangPatterns
   | Opt_WarnRedundantRecordWildcards
   | Opt_WarnWarningsDeprecations
   | Opt_WarnDeprecatedFlags
   | Opt_WarnMissingMonadFailInstances               -- since 8.0, has no effect since 8.8
   | Opt_WarnSemigroup                               -- since 8.0
   | Opt_WarnDodgyExports
   | Opt_WarnDodgyImports
   | Opt_WarnOrphans
   | Opt_WarnAutoOrphans
   | Opt_WarnIdentities
   | Opt_WarnTabs
   | Opt_WarnUnrecognisedPragmas
   | Opt_WarnMisplacedPragmas
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
   | Opt_WarnNonCanonicalMonadInstances              -- since 8.0
   | Opt_WarnNonCanonicalMonadFailInstances          -- since 8.0, removed 8.8
   | Opt_WarnNonCanonicalMonoidInstances             -- since 8.0
   | Opt_WarnMissingPatternSynonymSignatures         -- since 8.0
   | Opt_WarnUnrecognisedWarningFlags                -- since 8.0
   | Opt_WarnSimplifiableClassConstraints            -- Since 8.2
   | Opt_WarnCPPUndef                                -- Since 8.2
   | Opt_WarnUnbangedStrictPatterns                  -- Since 8.2
   | Opt_WarnMissingHomeModules                      -- Since 8.2
   | Opt_WarnPartialFields                           -- Since 8.4
   | Opt_WarnMissingExportList
   | Opt_WarnInaccessibleCode
   | Opt_WarnStarIsType                              -- Since 8.6
   | Opt_WarnStarBinder                              -- Since 8.6
   | Opt_WarnImplicitKindVars                        -- Since 8.6
   | Opt_WarnSpaceAfterBang
   | Opt_WarnMissingDerivingStrategies               -- Since 8.8
   | Opt_WarnPrepositiveQualifiedModule              -- Since 8.10
   | Opt_WarnUnusedPackages                          -- Since 8.10
   | Opt_WarnInferredSafeImports                     -- Since 8.10
   | Opt_WarnMissingSafeHaskellMode                  -- Since 8.10
   | Opt_WarnCompatUnqualifiedImports                -- Since 8.10
   | Opt_WarnDerivingDefaults
   | Opt_WarnInvalidHaddock                          -- Since 9.0
   | Opt_WarnOperatorWhitespaceExtConflict           -- Since 9.2
   | Opt_WarnOperatorWhitespace                      -- Since 9.2
   | Opt_WarnAmbiguousFields                         -- Since 9.2
   | Opt_WarnImplicitLift                            -- Since 9.2
   | Opt_WarnMissingKindSignatures                   -- Since 9.2
   | Opt_WarnMissingExportedPatternSynonymSignatures -- since 9.2
   | Opt_WarnRedundantStrictnessFlags                -- Since 9.4
   | Opt_WarnForallIdentifier                        -- Since 9.4
   | Opt_WarnUnicodeBidirectionalFormatCharacters    -- Since 9.0.2
   | Opt_WarnGADTMonoLocalBinds                      -- Since 9.4
   | Opt_WarnTypeEqualityOutOfScope                  -- Since 9.4
   | Opt_WarnTypeEqualityRequiresOperators           -- Since 9.4
   | Opt_WarnLoopySuperclassSolve                    -- Since 9.6
   deriving (Eq, Ord, Show, Enum, Bounded)

-- | Return the names of a WarningFlag
--
-- One flag may have several names because of US/UK spelling.  The first one is
-- the "preferred one" that will be displayed in warning messages.
warnFlagNames :: WarningFlag -> NonEmpty String
warnFlagNames wflag = case wflag of
  Opt_WarnAlternativeLayoutRuleTransitional       -> "alternative-layout-rule-transitional" :| []
  Opt_WarnAmbiguousFields                         -> "ambiguous-fields" :| []
  Opt_WarnAutoOrphans                             -> "auto-orphans" :| []
  Opt_WarnCPPUndef                                -> "cpp-undef" :| []
  Opt_WarnUnbangedStrictPatterns                  -> "unbanged-strict-patterns" :| []
  Opt_WarnDeferredTypeErrors                      -> "deferred-type-errors" :| []
  Opt_WarnDeferredOutOfScopeVariables             -> "deferred-out-of-scope-variables" :| []
  Opt_WarnWarningsDeprecations                    -> "deprecations" :| ["warnings-deprecations"]
  Opt_WarnDeprecatedFlags                         -> "deprecated-flags" :| []
  Opt_WarnDerivingDefaults                        -> "deriving-defaults" :| []
  Opt_WarnDerivingTypeable                        -> "deriving-typeable" :| []
  Opt_WarnDodgyExports                            -> "dodgy-exports" :| []
  Opt_WarnDodgyForeignImports                     -> "dodgy-foreign-imports" :| []
  Opt_WarnDodgyImports                            -> "dodgy-imports" :| []
  Opt_WarnEmptyEnumerations                       -> "empty-enumerations" :| []
  Opt_WarnDuplicateConstraints                    -> "duplicate-constraints" :| []
  Opt_WarnRedundantConstraints                    -> "redundant-constraints" :| []
  Opt_WarnDuplicateExports                        -> "duplicate-exports" :| []
  Opt_WarnHiShadows                               -> "hi-shadowing" :| []
  Opt_WarnInaccessibleCode                        -> "inaccessible-code" :| []
  Opt_WarnImplicitPrelude                         -> "implicit-prelude" :| []
  Opt_WarnImplicitKindVars                        -> "implicit-kind-vars" :| []
  Opt_WarnIncompletePatterns                      -> "incomplete-patterns" :| []
  Opt_WarnIncompletePatternsRecUpd                -> "incomplete-record-updates" :| []
  Opt_WarnIncompleteUniPatterns                   -> "incomplete-uni-patterns" :| []
  Opt_WarnInlineRuleShadowing                     -> "inline-rule-shadowing" :| []
  Opt_WarnIdentities                              -> "identities" :| []
  Opt_WarnMissingFields                           -> "missing-fields" :| []
  Opt_WarnMissingImportList                       -> "missing-import-lists" :| []
  Opt_WarnMissingExportList                       -> "missing-export-lists" :| []
  Opt_WarnMissingLocalSignatures                  -> "missing-local-signatures" :| []
  Opt_WarnMissingMethods                          -> "missing-methods" :| []
  Opt_WarnMissingMonadFailInstances               -> "missing-monadfail-instances" :| []
  Opt_WarnSemigroup                               -> "semigroup" :| []
  Opt_WarnMissingSignatures                       -> "missing-signatures" :| []
  Opt_WarnMissingKindSignatures                   -> "missing-kind-signatures" :| []
  Opt_WarnMissingExportedSignatures               -> "missing-exported-signatures" :| []
  Opt_WarnMonomorphism                            -> "monomorphism-restriction" :| []
  Opt_WarnNameShadowing                           -> "name-shadowing" :| []
  Opt_WarnNonCanonicalMonadInstances              -> "noncanonical-monad-instances" :| []
  Opt_WarnNonCanonicalMonadFailInstances          -> "noncanonical-monadfail-instances" :| []
  Opt_WarnNonCanonicalMonoidInstances             -> "noncanonical-monoid-instances" :| []
  Opt_WarnOrphans                                 -> "orphans" :| []
  Opt_WarnOverflowedLiterals                      -> "overflowed-literals" :| []
  Opt_WarnOverlappingPatterns                     -> "overlapping-patterns" :| []
  Opt_WarnMissedSpecs                             -> "missed-specialisations" :| ["missed-specializations"]
  Opt_WarnAllMissedSpecs                          -> "all-missed-specialisations" :| ["all-missed-specializations"]
  Opt_WarnSafe                                    -> "safe" :| []
  Opt_WarnTrustworthySafe                         -> "trustworthy-safe" :| []
  Opt_WarnInferredSafeImports                     -> "inferred-safe-imports" :| []
  Opt_WarnMissingSafeHaskellMode                  -> "missing-safe-haskell-mode" :| []
  Opt_WarnTabs                                    -> "tabs" :| []
  Opt_WarnTypeDefaults                            -> "type-defaults" :| []
  Opt_WarnTypedHoles                              -> "typed-holes" :| []
  Opt_WarnPartialTypeSignatures                   -> "partial-type-signatures" :| []
  Opt_WarnUnrecognisedPragmas                     -> "unrecognised-pragmas" :| []
  Opt_WarnMisplacedPragmas                        -> "misplaced-pragmas" :| []
  Opt_WarnUnsafe                                  -> "unsafe" :| []
  Opt_WarnUnsupportedCallingConventions           -> "unsupported-calling-conventions" :| []
  Opt_WarnUnsupportedLlvmVersion                  -> "unsupported-llvm-version" :| []
  Opt_WarnMissedExtraSharedLib                    -> "missed-extra-shared-lib" :| []
  Opt_WarnUntickedPromotedConstructors            -> "unticked-promoted-constructors" :| []
  Opt_WarnUnusedDoBind                            -> "unused-do-bind" :| []
  Opt_WarnUnusedForalls                           -> "unused-foralls" :| []
  Opt_WarnUnusedImports                           -> "unused-imports" :| []
  Opt_WarnUnusedLocalBinds                        -> "unused-local-binds" :| []
  Opt_WarnUnusedMatches                           -> "unused-matches" :| []
  Opt_WarnUnusedPatternBinds                      -> "unused-pattern-binds" :| []
  Opt_WarnUnusedTopBinds                          -> "unused-top-binds" :| []
  Opt_WarnUnusedTypePatterns                      -> "unused-type-patterns" :| []
  Opt_WarnUnusedRecordWildcards                   -> "unused-record-wildcards" :| []
  Opt_WarnRedundantBangPatterns                   -> "redundant-bang-patterns" :| []
  Opt_WarnRedundantRecordWildcards                -> "redundant-record-wildcards" :| []
  Opt_WarnRedundantStrictnessFlags                -> "redundant-strictness-flags" :| []
  Opt_WarnWrongDoBind                             -> "wrong-do-bind" :| []
  Opt_WarnMissingPatternSynonymSignatures         -> "missing-pattern-synonym-signatures" :| []
  Opt_WarnMissingDerivingStrategies               -> "missing-deriving-strategies" :| []
  Opt_WarnSimplifiableClassConstraints            -> "simplifiable-class-constraints" :| []
  Opt_WarnMissingHomeModules                      -> "missing-home-modules" :| []
  Opt_WarnUnrecognisedWarningFlags                -> "unrecognised-warning-flags" :| []
  Opt_WarnStarBinder                              -> "star-binder" :| []
  Opt_WarnStarIsType                              -> "star-is-type" :| []
  Opt_WarnSpaceAfterBang                          -> "missing-space-after-bang" :| []
  Opt_WarnPartialFields                           -> "partial-fields" :| []
  Opt_WarnPrepositiveQualifiedModule              -> "prepositive-qualified-module" :| []
  Opt_WarnUnusedPackages                          -> "unused-packages" :| []
  Opt_WarnCompatUnqualifiedImports                -> "compat-unqualified-imports" :| []
  Opt_WarnInvalidHaddock                          -> "invalid-haddock" :| []
  Opt_WarnOperatorWhitespaceExtConflict           -> "operator-whitespace-ext-conflict" :| []
  Opt_WarnOperatorWhitespace                      -> "operator-whitespace" :| []
  Opt_WarnImplicitLift                            -> "implicit-lift" :| []
  Opt_WarnMissingExportedPatternSynonymSignatures -> "missing-exported-pattern-synonym-signatures" :| []
  Opt_WarnForallIdentifier                        -> "forall-identifier" :| []
  Opt_WarnUnicodeBidirectionalFormatCharacters    -> "unicode-bidirectional-format-characters" :| []
  Opt_WarnGADTMonoLocalBinds                      -> "gadt-mono-local-binds" :| []
  Opt_WarnTypeEqualityOutOfScope                  -> "type-equality-out-of-scope" :| []
  Opt_WarnLoopySuperclassSolve                    -> "loopy-superclass-solve" :| []
  Opt_WarnTypeEqualityRequiresOperators           -> "type-equality-requires-operators" :| []

-- -----------------------------------------------------------------------------
-- Standard sets of warning options

-- Note [Documenting warning flags]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If you change the list of warning enabled by default
-- please remember to update the User's Guide. The relevant file is:
--
--  docs/users_guide/using-warnings.rst

-- | Warning groups.
--
-- As all warnings are in the Weverything set, it is ignored when
-- displaying to the user which group a warning is in.
warningGroups :: [(String, [WarningFlag])]
warningGroups =
    [ ("compat",       minusWcompatOpts)
    , ("unused-binds", unusedBindsFlags)
    , ("default",      standardWarnings)
    , ("extra",        minusWOpts)
    , ("all",          minusWallOpts)
    , ("everything",   minusWeverythingOpts)
    ]

-- | Warning group hierarchies, where there is an explicit inclusion
-- relation.
--
-- Each inner list is a hierarchy of warning groups, ordered from
-- smallest to largest, where each group is a superset of the one
-- before it.
--
-- Separating this from 'warningGroups' allows for multiple
-- hierarchies with no inherent relation to be defined.
--
-- The special-case Weverything group is not included.
warningHierarchies :: [[String]]
warningHierarchies = hierarchies ++ map (:[]) rest
  where
    hierarchies = [["default", "extra", "all"]]
    rest = filter (`notElem` "everything" : concat hierarchies) $
           map fst warningGroups

-- | Find the smallest group in every hierarchy which a warning
-- belongs to, excluding Weverything.
smallestWarningGroups :: WarningFlag -> [String]
smallestWarningGroups flag = mapMaybe go warningHierarchies where
    -- Because each hierarchy is arranged from smallest to largest,
    -- the first group we find in a hierarchy which contains the flag
    -- is the smallest.
    go (group:rest) = fromMaybe (go rest) $ do
        flags <- lookup group warningGroups
        guard (flag `elem` flags)
        pure (Just group)
    go [] = Nothing

-- | Warnings enabled unless specified otherwise
standardWarnings :: [WarningFlag]
standardWarnings -- see Note [Documenting warning flags]
    = [ Opt_WarnOverlappingPatterns,
        Opt_WarnWarningsDeprecations,
        Opt_WarnDeprecatedFlags,
        Opt_WarnDeferredTypeErrors,
        Opt_WarnTypedHoles,
        Opt_WarnDeferredOutOfScopeVariables,
        Opt_WarnPartialTypeSignatures,
        Opt_WarnUnrecognisedPragmas,
        Opt_WarnMisplacedPragmas,
        Opt_WarnDuplicateExports,
        Opt_WarnDerivingDefaults,
        Opt_WarnOverflowedLiterals,
        Opt_WarnEmptyEnumerations,
        Opt_WarnAmbiguousFields,
        Opt_WarnMissingFields,
        Opt_WarnMissingMethods,
        Opt_WarnWrongDoBind,
        Opt_WarnUnsupportedCallingConventions,
        Opt_WarnDodgyForeignImports,
        Opt_WarnInlineRuleShadowing,
        Opt_WarnAlternativeLayoutRuleTransitional,
        Opt_WarnUnsupportedLlvmVersion,
        Opt_WarnMissedExtraSharedLib,
        Opt_WarnTabs,
        Opt_WarnUnrecognisedWarningFlags,
        Opt_WarnSimplifiableClassConstraints,
        Opt_WarnStarBinder,
        Opt_WarnStarIsType,
        Opt_WarnInaccessibleCode,
        Opt_WarnSpaceAfterBang,
        Opt_WarnNonCanonicalMonadInstances,
        Opt_WarnNonCanonicalMonoidInstances,
        Opt_WarnOperatorWhitespaceExtConflict,
        Opt_WarnForallIdentifier,
        Opt_WarnUnicodeBidirectionalFormatCharacters,
        Opt_WarnGADTMonoLocalBinds,
        Opt_WarnLoopySuperclassSolve,
        Opt_WarnTypeEqualityRequiresOperators
      ]

-- | Things you get with -W
minusWOpts :: [WarningFlag]
minusWOpts
    = standardWarnings ++
      [ Opt_WarnUnusedTopBinds,
        Opt_WarnUnusedLocalBinds,
        Opt_WarnUnusedPatternBinds,
        Opt_WarnUnusedMatches,
        Opt_WarnUnusedForalls,
        Opt_WarnUnusedImports,
        Opt_WarnIncompletePatterns,
        Opt_WarnDodgyExports,
        Opt_WarnDodgyImports,
        Opt_WarnUnbangedStrictPatterns
      ]

-- | Things you get with -Wall
minusWallOpts :: [WarningFlag]
minusWallOpts
    = minusWOpts ++
      [ Opt_WarnTypeDefaults,
        Opt_WarnNameShadowing,
        Opt_WarnMissingSignatures,
        Opt_WarnHiShadows,
        Opt_WarnOrphans,
        Opt_WarnUnusedDoBind,
        Opt_WarnTrustworthySafe,
        Opt_WarnMissingPatternSynonymSignatures,
        Opt_WarnUnusedRecordWildcards,
        Opt_WarnRedundantRecordWildcards,
        Opt_WarnIncompleteUniPatterns,
        Opt_WarnIncompletePatternsRecUpd
      ]

-- | Things you get with -Weverything, i.e. *all* known warnings flags
minusWeverythingOpts :: [WarningFlag]
minusWeverythingOpts = [ toEnum 0 .. ]

-- | Things you get with -Wcompat.
--
-- This is intended to group together warnings that will be enabled by default
-- at some point in the future, so that library authors eager to make their
-- code future compatible to fix issues before they even generate warnings.
minusWcompatOpts :: [WarningFlag]
minusWcompatOpts
    = [ Opt_WarnSemigroup
      , Opt_WarnNonCanonicalMonoidInstances
      , Opt_WarnCompatUnqualifiedImports
      , Opt_WarnTypeEqualityOutOfScope
      ]

-- | Things you get with -Wunused-binds
unusedBindsFlags :: [WarningFlag]
unusedBindsFlags = [ Opt_WarnUnusedTopBinds
                   , Opt_WarnUnusedLocalBinds
                   , Opt_WarnUnusedPatternBinds
                   ]
