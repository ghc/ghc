{-# LANGUAGE LambdaCase #-}

module GHC.Driver.Flags
   ( DumpFlag(..)
   , getDumpFlagFrom
   , enabledIfVerbose
   , GeneralFlag(..)
   , Language(..)
   , defaultLanguage
   , optimisationFlags
   , codeGenFlags

   -- * Warnings
   , WarningGroup(..)
   , warningGroupName
   , warningGroupFlags
   , warningGroupIncludesExtendedWarnings
   , WarningFlag(..)
   , warnFlagNames
   , warningGroups
   , warningHierarchies
   , smallestWarningGroups
   , smallestWarningGroupsForCategory

   , standardWarnings
   , minusWOpts
   , minusWallOpts
   , minusWeverythingOpts
   , minusWcompatOpts
   , unusedBindsFlags

   , OnOff(..)
   , TurnOnFlag
   , turnOn
   , turnOff
   , impliedXFlags
   , validHoleFitsImpliedGFlags
   , impliedGFlags
   , impliedOffGFlags
   , glasgowExtsFlags

   , ExtensionDeprecation(..)
   , Deprecation(..)
   , extensionDeprecation
   , deprecation
   , extensionNames
   , extensionName
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

import qualified GHC.LanguageExtensions as LangExt

data Language = Haskell98 | Haskell2010 | GHC2021 | GHC2024
   deriving (Eq, Enum, Show, Bounded)

-- | The default Language is used if one is not specified explicitly, by both
-- GHC and GHCi.
defaultLanguage :: Language
defaultLanguage = GHC2021

instance Outputable Language where
    ppr = text . show

instance Binary Language where
  put_ bh = put_ bh . fromEnum
  get bh = toEnum <$> get bh

instance NFData Language where
  rnf Haskell98 = ()
  rnf Haskell2010 = ()
  rnf GHC2021 = ()
  rnf GHC2024 = ()

data OnOff a = On a
             | Off a
  deriving (Eq, Show)

instance Outputable a => Outputable (OnOff a) where
  ppr (On x)  = text "On" <+> ppr x
  ppr (Off x) = text "Off" <+> ppr x

type TurnOnFlag = Bool   -- True  <=> we are turning the flag on
                         -- False <=> we are turning the flag off
turnOn  :: TurnOnFlag; turnOn  = True
turnOff :: TurnOnFlag; turnOff = False

data Deprecation = NotDeprecated | Deprecated deriving (Eq, Ord)

data ExtensionDeprecation
  = ExtensionNotDeprecated
  | ExtensionDeprecatedFor [LangExt.Extension]
  | ExtensionFlagDeprecatedCond TurnOnFlag String
  | ExtensionFlagDeprecated String
  deriving Eq

-- | Always returns 'Deprecated' even when the flag is
-- only conditionally deprecated.
deprecation :: ExtensionDeprecation -> Deprecation
deprecation ExtensionNotDeprecated = NotDeprecated
deprecation _ = Deprecated

extensionDeprecation :: LangExt.Extension -> ExtensionDeprecation
extensionDeprecation = \case
  LangExt.TypeInType           -> ExtensionDeprecatedFor [LangExt.DataKinds, LangExt.PolyKinds]
  LangExt.NullaryTypeClasses   -> ExtensionDeprecatedFor [LangExt.MultiParamTypeClasses]
  LangExt.RelaxedPolyRec       -> ExtensionFlagDeprecatedCond turnOff
                                    "You can't turn off RelaxedPolyRec any more"
  LangExt.DatatypeContexts     -> ExtensionFlagDeprecatedCond turnOn
                                    "It was widely considered a misfeature, and has been removed from the Haskell language."
  LangExt.AutoDeriveTypeable   -> ExtensionFlagDeprecatedCond turnOn
                                    "Typeable instances are created automatically for all types since GHC 8.2."
  LangExt.OverlappingInstances -> ExtensionFlagDeprecated
                                    "instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS"
  _                            -> ExtensionNotDeprecated


extensionName :: LangExt.Extension -> String
extensionName = \case
  LangExt.Cpp -> "CPP"
  LangExt.OverlappingInstances -> "OverlappingInstances"
  LangExt.UndecidableInstances -> "UndecidableInstances"
  LangExt.IncoherentInstances -> "IncoherentInstances"
  LangExt.UndecidableSuperClasses -> "UndecidableSuperClasses"
  LangExt.MonomorphismRestriction -> "MonomorphismRestriction"
  LangExt.MonoLocalBinds -> "MonoLocalBinds"
  LangExt.DeepSubsumption -> "DeepSubsumption"
  LangExt.RelaxedPolyRec -> "RelaxedPolyRec"           -- Deprecated
  LangExt.ExtendedDefaultRules -> "ExtendedDefaultRules"     -- Use GHC's extended rules for defaulting
  LangExt.NamedDefaults -> "NamedDefaults"
  LangExt.ForeignFunctionInterface -> "ForeignFunctionInterface"
  LangExt.UnliftedFFITypes -> "UnliftedFFITypes"
  LangExt.InterruptibleFFI -> "InterruptibleFFI"
  LangExt.CApiFFI -> "CApiFFI"
  LangExt.GHCForeignImportPrim -> "GHCForeignImportPrim"
  LangExt.JavaScriptFFI -> "JavaScriptFFI"
  LangExt.ParallelArrays -> "ParallelArrays"           -- Syntactic support for parallel arrays
  LangExt.Arrows -> "Arrows"                   -- Arrow-notation syntax
  LangExt.TemplateHaskell -> "TemplateHaskell"
  LangExt.TemplateHaskellQuotes -> "TemplateHaskellQuotes"    -- subset of TH supported by stage1, no splice
  LangExt.QualifiedDo -> "QualifiedDo"
  LangExt.QuasiQuotes -> "QuasiQuotes"
  LangExt.ImplicitParams -> "ImplicitParams"
  LangExt.ImplicitPrelude -> "ImplicitPrelude"
  LangExt.ScopedTypeVariables -> "ScopedTypeVariables"
  LangExt.AllowAmbiguousTypes -> "AllowAmbiguousTypes"
  LangExt.UnboxedTuples -> "UnboxedTuples"
  LangExt.UnboxedSums -> "UnboxedSums"
  LangExt.UnliftedNewtypes -> "UnliftedNewtypes"
  LangExt.UnliftedDatatypes -> "UnliftedDatatypes"
  LangExt.BangPatterns -> "BangPatterns"
  LangExt.TypeFamilies -> "TypeFamilies"
  LangExt.TypeFamilyDependencies -> "TypeFamilyDependencies"
  LangExt.TypeInType -> "TypeInType"               -- Deprecated
  LangExt.OverloadedStrings -> "OverloadedStrings"
  LangExt.OverloadedLists -> "OverloadedLists"
  LangExt.NumDecimals -> "NumDecimals"
  LangExt.OrPatterns -> "OrPatterns"
  LangExt.DisambiguateRecordFields -> "DisambiguateRecordFields"
  LangExt.RecordWildCards -> "RecordWildCards"
  LangExt.NamedFieldPuns -> "NamedFieldPuns"
  LangExt.ViewPatterns -> "ViewPatterns"
  LangExt.GADTs -> "GADTs"
  LangExt.GADTSyntax -> "GADTSyntax"
  LangExt.NPlusKPatterns -> "NPlusKPatterns"
  LangExt.DoAndIfThenElse -> "DoAndIfThenElse"
  LangExt.BlockArguments -> "BlockArguments"
  LangExt.RebindableSyntax -> "RebindableSyntax"
  LangExt.ConstraintKinds -> "ConstraintKinds"
  LangExt.PolyKinds -> "PolyKinds"                -- Kind polymorphism
  LangExt.DataKinds -> "DataKinds"                -- Datatype promotion
  LangExt.TypeData -> "TypeData"                 -- allow @type data@ definitions
  LangExt.InstanceSigs -> "InstanceSigs"
  LangExt.ApplicativeDo -> "ApplicativeDo"
  LangExt.LinearTypes -> "LinearTypes"
  LangExt.RequiredTypeArguments -> "RequiredTypeArguments"    -- Visible forall (VDQ) in types of terms
  LangExt.StandaloneDeriving -> "StandaloneDeriving"
  LangExt.DeriveDataTypeable -> "DeriveDataTypeable"
  LangExt.AutoDeriveTypeable -> "AutoDeriveTypeable"       -- Automatic derivation of Typeable
  LangExt.DeriveFunctor -> "DeriveFunctor"
  LangExt.DeriveTraversable -> "DeriveTraversable"
  LangExt.DeriveFoldable -> "DeriveFoldable"
  LangExt.DeriveGeneric -> "DeriveGeneric"            -- Allow deriving Generic/1
  LangExt.DefaultSignatures -> "DefaultSignatures"        -- Allow extra signatures for defmeths
  LangExt.DeriveAnyClass -> "DeriveAnyClass"           -- Allow deriving any class
  LangExt.DeriveLift -> "DeriveLift"               -- Allow deriving Lift
  LangExt.DerivingStrategies -> "DerivingStrategies"
  LangExt.DerivingVia -> "DerivingVia"              -- Derive through equal representation
  LangExt.TypeSynonymInstances -> "TypeSynonymInstances"
  LangExt.FlexibleContexts -> "FlexibleContexts"
  LangExt.FlexibleInstances -> "FlexibleInstances"
  LangExt.ConstrainedClassMethods -> "ConstrainedClassMethods"
  LangExt.MultiParamTypeClasses -> "MultiParamTypeClasses"
  LangExt.NullaryTypeClasses -> "NullaryTypeClasses"
  LangExt.FunctionalDependencies -> "FunctionalDependencies"
  LangExt.UnicodeSyntax -> "UnicodeSyntax"
  LangExt.ExistentialQuantification -> "ExistentialQuantification"
  LangExt.MagicHash -> "MagicHash"
  LangExt.EmptyDataDecls -> "EmptyDataDecls"
  LangExt.KindSignatures -> "KindSignatures"
  LangExt.RoleAnnotations -> "RoleAnnotations"
  LangExt.ParallelListComp -> "ParallelListComp"
  LangExt.TransformListComp -> "TransformListComp"
  LangExt.MonadComprehensions -> "MonadComprehensions"
  LangExt.GeneralizedNewtypeDeriving -> "GeneralizedNewtypeDeriving"
  LangExt.RecursiveDo -> "RecursiveDo"
  LangExt.PostfixOperators -> "PostfixOperators"
  LangExt.TupleSections -> "TupleSections"
  LangExt.PatternGuards -> "PatternGuards"
  LangExt.LiberalTypeSynonyms -> "LiberalTypeSynonyms"
  LangExt.RankNTypes -> "RankNTypes"
  LangExt.ImpredicativeTypes -> "ImpredicativeTypes"
  LangExt.TypeOperators -> "TypeOperators"
  LangExt.ExplicitNamespaces -> "ExplicitNamespaces"
  LangExt.PackageImports -> "PackageImports"
  LangExt.ExplicitForAll -> "ExplicitForAll"
  LangExt.AlternativeLayoutRule -> "AlternativeLayoutRule"
  LangExt.AlternativeLayoutRuleTransitional -> "AlternativeLayoutRuleTransitional"
  LangExt.DatatypeContexts -> "DatatypeContexts"
  LangExt.NondecreasingIndentation -> "NondecreasingIndentation"
  LangExt.RelaxedLayout -> "RelaxedLayout"
  LangExt.TraditionalRecordSyntax -> "TraditionalRecordSyntax"
  LangExt.LambdaCase -> "LambdaCase"
  LangExt.MultiWayIf -> "MultiWayIf"
  LangExt.BinaryLiterals -> "BinaryLiterals"
  LangExt.NegativeLiterals -> "NegativeLiterals"
  LangExt.HexFloatLiterals -> "HexFloatLiterals"
  LangExt.DuplicateRecordFields -> "DuplicateRecordFields"
  LangExt.OverloadedLabels -> "OverloadedLabels"
  LangExt.EmptyCase -> "EmptyCase"
  LangExt.PatternSynonyms -> "PatternSynonyms"
  LangExt.PartialTypeSignatures -> "PartialTypeSignatures"
  LangExt.NamedWildCards -> "NamedWildCards"
  LangExt.StaticPointers -> "StaticPointers"
  LangExt.TypeApplications -> "TypeApplications"
  LangExt.Strict -> "Strict"
  LangExt.StrictData -> "StrictData"
  LangExt.EmptyDataDeriving -> "EmptyDataDeriving"
  LangExt.NumericUnderscores -> "NumericUnderscores"
  LangExt.QuantifiedConstraints -> "QuantifiedConstraints"
  LangExt.StarIsType -> "StarIsType"
  LangExt.ImportQualifiedPost -> "ImportQualifiedPost"
  LangExt.CUSKs -> "CUSKs"
  LangExt.StandaloneKindSignatures -> "StandaloneKindSignatures"
  LangExt.LexicalNegation -> "LexicalNegation"
  LangExt.FieldSelectors -> "FieldSelectors"
  LangExt.OverloadedRecordDot -> "OverloadedRecordDot"
  LangExt.OverloadedRecordUpdate -> "OverloadedRecordUpdate"
  LangExt.TypeAbstractions -> "TypeAbstractions"
  LangExt.ExtendedLiterals -> "ExtendedLiterals"
  LangExt.ListTuplePuns -> "ListTuplePuns"
  LangExt.MultilineStrings -> "MultilineStrings"

-- | Is this extension known by any other names? For example
-- -XGeneralizedNewtypeDeriving is accepted
extensionAlternateNames :: LangExt.Extension -> [String]
extensionAlternateNames = \case
  LangExt.GeneralizedNewtypeDeriving -> ["GeneralisedNewtypeDeriving"]
  LangExt.RankNTypes                 -> ["Rank2Types", "PolymorphicComponents"]
  _ -> []

extensionDeprecatedNames :: LangExt.Extension -> [String]
extensionDeprecatedNames = \case
  LangExt.RecursiveDo         -> ["DoRec"]
  LangExt.NamedFieldPuns      -> ["RecordPuns"]
  LangExt.ScopedTypeVariables -> ["PatternSignatures"]
  _ -> []

-- | All the names by which an extension is known.
extensionNames :: LangExt.Extension -> [ (ExtensionDeprecation, String) ]
extensionNames ext = mk (extensionDeprecation ext)     (extensionName ext : extensionAlternateNames ext)
                  ++ mk (ExtensionDeprecatedFor [ext]) (extensionDeprecatedNames ext)
  where mk depr = map (\name -> (depr, name))

impliedXFlags :: [(LangExt.Extension, OnOff LangExt.Extension)]
impliedXFlags
-- See Note [Updating flag description in the User's Guide]
  = [ (LangExt.RankNTypes,                On LangExt.ExplicitForAll)
    , (LangExt.QuantifiedConstraints,     On LangExt.ExplicitForAll)
    , (LangExt.ScopedTypeVariables,       On LangExt.ExplicitForAll)
    , (LangExt.LiberalTypeSynonyms,       On LangExt.ExplicitForAll)
    , (LangExt.ExistentialQuantification, On LangExt.ExplicitForAll)
    , (LangExt.FlexibleInstances,         On LangExt.TypeSynonymInstances)
    , (LangExt.FunctionalDependencies,    On LangExt.MultiParamTypeClasses)
    , (LangExt.MultiParamTypeClasses,     On LangExt.ConstrainedClassMethods)  -- c.f. #7854
    , (LangExt.TypeFamilyDependencies,    On LangExt.TypeFamilies)

    , (LangExt.RebindableSyntax, Off LangExt.ImplicitPrelude)      -- NB: turn off!

    , (LangExt.DerivingVia, On LangExt.DerivingStrategies)

    , (LangExt.GADTs,            On LangExt.GADTSyntax)
    , (LangExt.GADTs,            On LangExt.MonoLocalBinds)
    , (LangExt.TypeFamilies,     On LangExt.MonoLocalBinds)

    , (LangExt.TypeFamilies,     On LangExt.KindSignatures)  -- Type families use kind signatures
    , (LangExt.PolyKinds,        On LangExt.KindSignatures)  -- Ditto polymorphic kinds

    -- TypeInType is now just a synonym for a couple of other extensions.
    , (LangExt.TypeInType,       On LangExt.DataKinds)
    , (LangExt.TypeInType,       On LangExt.PolyKinds)
    , (LangExt.TypeInType,       On LangExt.KindSignatures)

    -- Standalone kind signatures are a replacement for CUSKs.
    , (LangExt.StandaloneKindSignatures, Off LangExt.CUSKs)

    -- AutoDeriveTypeable is not very useful without DeriveDataTypeable
    , (LangExt.AutoDeriveTypeable, On LangExt.DeriveDataTypeable)

    -- We turn this on so that we can export associated type
    -- type synonyms in subordinates (e.g. MyClass(type AssocType))
    , (LangExt.TypeFamilies,     On LangExt.ExplicitNamespaces)
    , (LangExt.TypeOperators, On LangExt.ExplicitNamespaces)

    , (LangExt.ImpredicativeTypes,  On LangExt.RankNTypes)

        -- Record wild-cards implies field disambiguation
        -- Otherwise if you write (C {..}) you may well get
        -- stuff like " 'a' not in scope ", which is a bit silly
        -- if the compiler has just filled in field 'a' of constructor 'C'
    , (LangExt.RecordWildCards,     On LangExt.DisambiguateRecordFields)

    , (LangExt.ParallelArrays, On LangExt.ParallelListComp)

    , (LangExt.JavaScriptFFI, On LangExt.InterruptibleFFI)

    , (LangExt.DeriveTraversable, On LangExt.DeriveFunctor)
    , (LangExt.DeriveTraversable, On LangExt.DeriveFoldable)

    -- Duplicate record fields require field disambiguation
    , (LangExt.DuplicateRecordFields, On LangExt.DisambiguateRecordFields)

    , (LangExt.TemplateHaskell, On LangExt.TemplateHaskellQuotes)
    , (LangExt.Strict, On LangExt.StrictData)

    -- Historically only UnboxedTuples was required for unboxed sums to work.
    -- To avoid breaking code, we make UnboxedTuples imply UnboxedSums.
    , (LangExt.UnboxedTuples, On LangExt.UnboxedSums)

    -- The extensions needed to declare an H98 unlifted data type
    , (LangExt.UnliftedDatatypes, On LangExt.DataKinds)
    , (LangExt.UnliftedDatatypes, On LangExt.StandaloneKindSignatures)

    -- See (NVP3) in Note [Non-variable pattern bindings aren't linear] in GHC.Tc.Gen.Bind
    , (LangExt.LinearTypes, On LangExt.MonoLocalBinds)
  ]


validHoleFitsImpliedGFlags :: [(GeneralFlag, TurnOnFlag, GeneralFlag)]
validHoleFitsImpliedGFlags
  = [ (Opt_UnclutterValidHoleFits, turnOff, Opt_ShowTypeAppOfHoleFits)
    , (Opt_UnclutterValidHoleFits, turnOff, Opt_ShowTypeAppVarsOfHoleFits)
    , (Opt_UnclutterValidHoleFits, turnOff, Opt_ShowDocsOfHoleFits)
    , (Opt_ShowTypeAppVarsOfHoleFits, turnOff, Opt_ShowTypeAppOfHoleFits)
    , (Opt_UnclutterValidHoleFits, turnOff, Opt_ShowProvOfHoleFits) ]

-- | General flags that are switched on/off when other general flags are switched
-- on
impliedGFlags :: [(GeneralFlag, TurnOnFlag, GeneralFlag)]
impliedGFlags = [(Opt_DeferTypeErrors, turnOn, Opt_DeferTypedHoles)
                ,(Opt_DeferTypeErrors, turnOn, Opt_DeferOutOfScopeVariables)
                ,(Opt_DoLinearCoreLinting, turnOn, Opt_DoCoreLinting)
                ,(Opt_Strictness, turnOn, Opt_WorkerWrapper)
                ,(Opt_WriteIfSimplifiedCore, turnOn, Opt_WriteInterface)
                ,(Opt_ByteCodeAndObjectCode, turnOn, Opt_WriteIfSimplifiedCore)
                ,(Opt_InfoTableMap, turnOn, Opt_InfoTableMapWithStack)
                ,(Opt_InfoTableMap, turnOn, Opt_InfoTableMapWithFallback)
                ] ++ validHoleFitsImpliedGFlags

-- | General flags that are switched on/off when other general flags are switched
-- off
impliedOffGFlags :: [(GeneralFlag, TurnOnFlag, GeneralFlag)]
impliedOffGFlags = [(Opt_Strictness, turnOff, Opt_WorkerWrapper)]

-- Please keep @docs/users_guide/what_glasgow_exts_does.rst@ up to date with this list.
glasgowExtsFlags :: [LangExt.Extension]
glasgowExtsFlags = [
             LangExt.ConstrainedClassMethods
           , LangExt.DeriveDataTypeable
           , LangExt.DeriveFoldable
           , LangExt.DeriveFunctor
           , LangExt.DeriveGeneric
           , LangExt.DeriveTraversable
           , LangExt.EmptyDataDecls
           , LangExt.ExistentialQuantification
           , LangExt.ExplicitNamespaces
           , LangExt.FlexibleContexts
           , LangExt.FlexibleInstances
           , LangExt.ForeignFunctionInterface
           , LangExt.FunctionalDependencies
           , LangExt.GeneralizedNewtypeDeriving
           , LangExt.ImplicitParams
           , LangExt.KindSignatures
           , LangExt.LiberalTypeSynonyms
           , LangExt.MagicHash
           , LangExt.MultiParamTypeClasses
           , LangExt.ParallelListComp
           , LangExt.PatternGuards
           , LangExt.PostfixOperators
           , LangExt.RankNTypes
           , LangExt.RecursiveDo
           , LangExt.ScopedTypeVariables
           , LangExt.StandaloneDeriving
           , LangExt.TypeOperators
           , LangExt.TypeSynonymInstances
           , LangExt.UnboxedTuples
           , LangExt.UnicodeSyntax
           , LangExt.UnliftedFFITypes ]

-- | Debugging flags
data DumpFlag
-- See Note [Updating flag description in the User's Guide] in GHC.Driver.Session

   -- debugging flags
   = Opt_D_dump_cmm
   | Opt_D_dump_cmm_from_stg
   | Opt_D_dump_cmm_raw
   | Opt_D_dump_cmm_verbose_by_proc
   -- All of the cmm subflags (there are a lot!) automatically
   -- enabled if you run -ddump-cmm-verbose-by-proc
   -- Each flag corresponds to exact stage of Cmm pipeline.
   | Opt_D_dump_cmm_verbose
   -- ^ same as -ddump-cmm-verbose-by-proc but writes each stage
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
   | Opt_D_dump_spec_constr
   | Opt_D_dump_prep
   | Opt_D_dump_late_cc
   | Opt_D_dump_stg_from_core -- ^ Initial STG (CoreToStg output)
   | Opt_D_dump_stg_unarised  -- ^ STG after unarise
   | Opt_D_dump_stg_cg        -- ^ STG (after stg2stg)
   | Opt_D_dump_stg_tags      -- ^ Result of tag inference analysis.
   | Opt_D_dump_stg_final     -- ^ Final STG (before cmm gen)
   | Opt_D_dump_stg_from_js_sinker -- ^ STG after JS sinker
   | Opt_D_dump_call_arity
   | Opt_D_dump_exitify
   | Opt_D_dump_dmdanal
   | Opt_D_dump_dmd_signatures
   | Opt_D_dump_cpranal
   | Opt_D_dump_cpr_signatures
   | Opt_D_dump_tc
   | Opt_D_dump_tc_ast
   | Opt_D_dump_hie
   | Opt_D_dump_types
   | Opt_D_dump_rules
   | Opt_D_dump_cse
   | Opt_D_dump_float_out
   | Opt_D_dump_float_in
   | Opt_D_dump_liberate_case
   | Opt_D_dump_static_argument_transformation
   | Opt_D_dump_worker_wrapper
   | Opt_D_dump_rn_trace
   | Opt_D_dump_rn_stats
   | Opt_D_dump_opt_cmm
   | Opt_D_dump_simpl_stats
   | Opt_D_dump_cs_trace -- ^ Constraint solver in type checker
   | Opt_D_dump_tc_trace
   | Opt_D_dump_ec_trace -- ^ Pattern match exhaustiveness checker
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
-- See Note [Updating flag description in the User's Guide] in GHC.Driver.Session

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
   | Opt_AddBcoName
   | Opt_NoLlvmMangler                  -- hidden flag
   | Opt_FastLlvm                       -- hidden flag
   | Opt_NoTypeableBinds

   | Opt_DistinctConstructorTables
   | Opt_InfoTableMap
   | Opt_InfoTableMapWithFallback
   | Opt_InfoTableMapWithStack

   | Opt_WarnIsError
   -- ^ @-Werror@; makes all warnings fatal.
   -- See 'wopt_set_fatal' for making individual warnings fatal as in @-Werror=foo@.
   | Opt_ShowWarnGroups
   -- ^ Show the group a warning belongs to.
   | Opt_HideSourcePaths
   -- ^ @-fhide-source-paths@; hide module source/object paths.

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
   | Opt_SpecialiseIncoherents
   | Opt_DoLambdaEtaExpansion
   | Opt_DoCleverArgEtaExpansion        -- See Note [Eta expansion of arguments in CorePrep]
   | Opt_IgnoreAsserts
   | Opt_DoEtaReduction
   | Opt_CaseMerge
   | Opt_CaseFolding                    -- ^ Constant folding through case-expressions
   | Opt_UnboxStrictFields
   | Opt_UnboxSmallStrictFields
   | Opt_DictsCheap
   | Opt_EnableRewriteRules             -- ^ Apply rewrite rules during simplification
   | Opt_EnableThSpliceWarnings         -- ^ Enable warnings for TH splices
   | Opt_RegsGraph                      -- ^ Do graph coloring register allocation
   | Opt_RegsIterative                  -- ^ Do iterative coalescing graph coloring register allocation
   | Opt_PedanticBottoms                -- ^ Be picky about how we treat bottom
   | Opt_LlvmFillUndefWithGarbage       -- Testing for undef bugs (hidden flag)
   | Opt_IrrefutableTuples
   | Opt_CmmSink
   | Opt_CmmStaticPred
   | Opt_CmmElimCommonBlocks
   | Opt_CmmControlFlow
   | Opt_AsmShortcutting
   | Opt_OmitYields
   | Opt_FunToThunk                -- deprecated
   | Opt_DictsStrict               -- ^ Be strict in argument dictionaries
   | Opt_DmdTxDictSel              -- ^ deprecated, no effect and behaviour is now default.
                                   -- Allowed switching of a special demand transformer for dictionary selectors
   | Opt_Loopification             -- See Note [Self-recursive tail calls]
   | Opt_CfgBlocklayout            -- ^ Use the cfg based block layout algorithm.
   | Opt_WeightlessBlocklayout     -- ^ Layout based on last instruction per block.
   | Opt_CprAnal
   | Opt_WorkerWrapper
   | Opt_WorkerWrapperUnlift  -- ^ Do W/W split for unlifting even if we won't unbox anything.
   | Opt_SolveConstantDicts
   | Opt_AlignmentSanitisation
   | Opt_CatchNonexhaustiveCases
   | Opt_NumConstantFolding   -- ^ See Note [Constant folding through nested expressions] in GHC.Core.Opt.ConstantFold
   | Opt_CoreConstantFolding
   | Opt_FastPAPCalls                  -- #6084
   | Opt_SpecEval
   | Opt_SpecEvalDictFun   -- See Note [Controlling Speculative Evaluation]


   -- Inference flags
   | Opt_DoTagInferenceChecks

   -- | PreInlining is on by default. The option is there just to see how
   -- bad things get if you turn it off!
   | Opt_SimplPreInlining

   -- Interface files
   | Opt_IgnoreInterfacePragmas
   | Opt_OmitInterfacePragmas
   | Opt_ExposeAllUnfoldings
   | Opt_ExposeOverloadedUnfoldings
   | Opt_KeepAutoRules -- ^ Keep auto-generated rules even if they seem to have become useless
   | Opt_WriteInterface -- ^ Forces .hi files to be written even with -fno-code
   | Opt_WriteSelfRecompInfo
   | Opt_WriteSelfRecompFlags -- ^ Include detailed flag information for self-recompilation debugging
   | Opt_WriteHie -- ^ Generate .hie files

   -- JavaScript opts
   | Opt_DisableJsMinifier -- ^ Render JavaScript pretty-printed instead of minified (compacted)
   | Opt_DisableJsCsources -- ^ Don't link C sources (compiled to JS) with Haskell code (compiled to JS)

   -- profiling opts
   | Opt_AutoSccsOnIndividualCafs
   | Opt_ProfCountEntries
   | Opt_ProfLateInlineCcs
   | Opt_ProfLateCcs
   | Opt_ProfLateOverloadedCcs
   | Opt_ProfLateoverloadedCallsCCs
   | Opt_ProfManualCcs -- ^ Ignore manual SCC annotations

   -- misc opts
   | Opt_Pp
   | Opt_ForceRecomp
   | Opt_IgnoreOptimChanges
   | Opt_IgnoreHpcChanges
   | Opt_ExcessPrecision
   | Opt_EagerBlackHoling
   | Opt_OrigThunkInfo
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
   | Opt_InsertBreakpoints
   | Opt_GhciHistory
   | Opt_GhciLeakCheck
   | Opt_ValidateHie
   | Opt_LocalGhciHistory
   | Opt_NoIt

   -- wasm ghci browser mode
   | Opt_GhciBrowser
   | Opt_GhciBrowserRedirectWasiConsole

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
   | Opt_UnoptimizedCoreForInterpreter
   | Opt_LinkRts

   -- output style opts
   | Opt_ErrorSpans -- ^ Include full span info in error messages,
                    -- instead of just the start position.
   | Opt_DeferDiagnostics
   | Opt_DiagnosticsAsJSON  -- ^ Dump diagnostics as JSON
   | Opt_DiagnosticsShowCaret -- ^ Show snippets of offending code
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

   -- | Suppress a coercions inner structure, replacing it with '...'
   | Opt_SuppressCoercions
   -- | Suppress the type of a coercion as well
   | Opt_SuppressCoercionTypes
   | Opt_SuppressVarKinds
   -- | Suppress module id prefixes on variables.
   | Opt_SuppressModulePrefixes
   -- | Suppress type applications.
   | Opt_SuppressTypeApplications
   -- | Suppress info such as arity and unfoldings on identifiers.
   | Opt_SuppressIdInfo
   -- | Suppress separate type signatures in core, but leave types on
   -- lambda bound vars
   | Opt_SuppressUnfoldings
   -- | Suppress the details of even stable unfoldings
   | Opt_SuppressTypeSignatures
   -- | Suppress unique ids on variables.
   -- Except for uniques, as some simplifier phases introduce new
   -- variables that have otherwise identical names.
   | Opt_SuppressUniques
   | Opt_SuppressStgExts
   | Opt_SuppressStgReps
   | Opt_SuppressTicks      -- ^ Replaces Opt_PprShowTicks
   | Opt_SuppressTimestamps -- ^ Suppress timestamps in dumps
   | Opt_SuppressCoreSizes  -- ^ Suppress per binding Core size stats in dumps

   -- Error message suppression
   | Opt_ShowErrorContext

   -- Object code determinism
   | Opt_ObjectDeterminism

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
   , Opt_ExposeOverloadedUnfoldings
   , Opt_NoTypeableBinds
   , Opt_ObjectDeterminism
   , Opt_Haddock

     -- Flags that affect catching of runtime errors
   , Opt_CatchNonexhaustiveCases
   , Opt_LlvmFillUndefWithGarbage
   , Opt_DoTagInferenceChecks

     -- Flags that affect debugging information
   , Opt_DistinctConstructorTables
   , Opt_InfoTableMap
   , Opt_InfoTableMapWithStack
   , Opt_InfoTableMapWithFallback
   , Opt_OrigThunkInfo
   ]

data WarningFlag =
-- See Note [Updating flag description in the User's Guide] in GHC.Driver.Session
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
   | Opt_WarnDeprecatedFlags
   | Opt_WarnMissingMonadFailInstances               -- since 8.0, has no effect since 8.8
   | Opt_WarnSemigroup                               -- since 8.0, has no effect since 9.8
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
   | Opt_WarnNonCanonicalMonadInstances              -- ^ @since 8.0
   | Opt_WarnNonCanonicalMonadFailInstances          -- ^ @since 8.0, has no effect since 8.8
   | Opt_WarnNonCanonicalMonoidInstances             -- ^ @since 8.0
   | Opt_WarnMissingPatternSynonymSignatures         -- ^ @since 8.0
   | Opt_WarnUnrecognisedWarningFlags                -- ^ @since 8.0
   | Opt_WarnSimplifiableClassConstraints            -- ^ @since 8.2
   | Opt_WarnCPPUndef                                -- ^ @since 8.2
   | Opt_WarnUnbangedStrictPatterns                  -- ^ @since 8.2
   | Opt_WarnMissingHomeModules                      -- ^ @since 8.2
   | Opt_WarnPartialFields                           -- ^ @since 8.4
   | Opt_WarnMissingExportList
   | Opt_WarnInaccessibleCode
   | Opt_WarnStarIsType                              -- ^ @since 8.6
   | Opt_WarnStarBinder                              -- ^ @since 8.6
   | Opt_WarnImplicitKindVars                        -- ^ @since 8.6
   | Opt_WarnSpaceAfterBang
   | Opt_WarnMissingDerivingStrategies               -- ^ @since 8.8
   | Opt_WarnPrepositiveQualifiedModule              -- ^ @since 8.10
   | Opt_WarnUnusedPackages                          -- ^ @since 8.10
   | Opt_WarnInferredSafeImports                     -- ^ @since 8.10
   | Opt_WarnMissingSafeHaskellMode                  -- ^ @since 8.10
   | Opt_WarnCompatUnqualifiedImports                -- ^ @since 8.10
   | Opt_WarnDerivingDefaults
   | Opt_WarnInvalidHaddock                          -- ^ @since 9.0
   | Opt_WarnOperatorWhitespaceExtConflict           -- ^ @since 9.2
   | Opt_WarnOperatorWhitespace                      -- ^ @since 9.2
   | Opt_WarnAmbiguousFields                         -- ^ @since 9.2
   | Opt_WarnImplicitLift                            -- ^ @since 9.2
   | Opt_WarnMissingKindSignatures                   -- ^ @since 9.2
   | Opt_WarnMissingPolyKindSignatures               -- ^ @since 9.8
   | Opt_WarnMissingExportedPatternSynonymSignatures -- ^ @since 9.2
   | Opt_WarnRedundantStrictnessFlags                -- ^ @since 9.4
   | Opt_WarnForallIdentifier                        -- ^ @since 9.4
   | Opt_WarnUnicodeBidirectionalFormatCharacters    -- ^ @since 9.0.2
   | Opt_WarnGADTMonoLocalBinds                      -- ^ @since 9.4
   | Opt_WarnTypeEqualityOutOfScope                  -- ^ @since 9.4
   | Opt_WarnTypeEqualityRequiresOperators           -- ^ @since 9.4
   | Opt_WarnLoopySuperclassSolve                    -- ^ @since 9.6, has no effect since 9.10
   | Opt_WarnTermVariableCapture                     -- ^ @since 9.8
   | Opt_WarnMissingRoleAnnotations                  -- ^ @since 9.8
   | Opt_WarnImplicitRhsQuantification               -- ^ @since 9.8
   | Opt_WarnIncompleteExportWarnings                -- ^ @since 9.8
   | Opt_WarnIncompleteRecordSelectors               -- ^ @since 9.10
   | Opt_WarnBadlyStagedTypes                        -- ^ @since 9.10
   | Opt_WarnInconsistentFlags                       -- ^ @since 9.8
   | Opt_WarnDataKindsTC                             -- ^ @since 9.10
   | Opt_WarnDefaultedExceptionContext               -- ^ @since 9.10
   | Opt_WarnViewPatternSignatures                   -- ^ @since 9.12
   | Opt_WarnUselessSpecialisations                  -- ^ @since 9.14
   | Opt_WarnDeprecatedPragmas                       -- ^ @since 9.14
   | Opt_WarnRuleLhsEqualities
       -- ^ @since 9.14, scheduled to be removed in 9.18
       --
       -- See Note [Quantifying over equalities in RULES] in GHC.Tc.Gen.Sig
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
  Opt_WarnTermVariableCapture                     -> "term-variable-capture" :| []
  Opt_WarnCPPUndef                                -> "cpp-undef" :| []
  Opt_WarnUnbangedStrictPatterns                  -> "unbanged-strict-patterns" :| []
  Opt_WarnDeferredTypeErrors                      -> "deferred-type-errors" :| []
  Opt_WarnDeferredOutOfScopeVariables             -> "deferred-out-of-scope-variables" :| []
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
  Opt_WarnMissingPolyKindSignatures               -> "missing-poly-kind-signatures" :| []
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
  Opt_WarnMissingRoleAnnotations                  -> "missing-role-annotations" :| []
  Opt_WarnImplicitRhsQuantification               -> "implicit-rhs-quantification" :| []
  Opt_WarnIncompleteExportWarnings                -> "incomplete-export-warnings" :| []
  Opt_WarnIncompleteRecordSelectors               -> "incomplete-record-selectors" :| []
  Opt_WarnBadlyStagedTypes                        -> "badly-staged-types" :| []
  Opt_WarnInconsistentFlags                       -> "inconsistent-flags" :| []
  Opt_WarnDataKindsTC                             -> "data-kinds-tc" :| []
  Opt_WarnDefaultedExceptionContext               -> "defaulted-exception-context" :| []
  Opt_WarnViewPatternSignatures                   -> "view-pattern-signatures" :| []
  Opt_WarnUselessSpecialisations                  -> "useless-specialisations" :| ["useless-specializations"]
  Opt_WarnDeprecatedPragmas                       -> "deprecated-pragmas" :| []
  Opt_WarnRuleLhsEqualities                       -> "rule-lhs-equalities" :| []

-- -----------------------------------------------------------------------------
-- Standard sets of warning options

-- Note [Documenting warning flags]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If you change the list of warnings enabled by default
-- please remember to update the User's Guide. The relevant file is:
--
--  docs/users_guide/using-warnings.rst


-- | A group of warning flags that can be enabled or disabled collectively,
-- e.g. using @-Wcompat@ to enable all warnings in the 'W_compat' group.
data WarningGroup = W_compat
                  | W_unused_binds
                  | W_extended_warnings
                  | W_default
                  | W_extra
                  | W_all
                  | W_everything
  deriving (Bounded, Enum, Eq)

warningGroupName :: WarningGroup -> String
warningGroupName W_compat            = "compat"
warningGroupName W_unused_binds      = "unused-binds"
warningGroupName W_extended_warnings = "extended-warnings"
warningGroupName W_default           = "default"
warningGroupName W_extra             = "extra"
warningGroupName W_all               = "all"
warningGroupName W_everything        = "everything"

warningGroupFlags :: WarningGroup -> [WarningFlag]
warningGroupFlags W_compat            = minusWcompatOpts
warningGroupFlags W_unused_binds      = unusedBindsFlags
warningGroupFlags W_extended_warnings = []
warningGroupFlags W_default           = standardWarnings
warningGroupFlags W_extra             = minusWOpts
warningGroupFlags W_all               = minusWallOpts
warningGroupFlags W_everything        = minusWeverythingOpts

-- | Does this warning group contain (all) extended warning categories?  See
-- Note [Warning categories] in GHC.Unit.Module.Warnings.
--
-- The 'W_extended_warnings' group contains extended warnings but no
-- 'WarningFlag's, but extended warnings are also treated as part of 'W_default'
-- and every warning group that includes it.
warningGroupIncludesExtendedWarnings :: WarningGroup -> Bool
warningGroupIncludesExtendedWarnings W_compat            = False
warningGroupIncludesExtendedWarnings W_unused_binds      = False
warningGroupIncludesExtendedWarnings W_extended_warnings = True
warningGroupIncludesExtendedWarnings W_default           = True
warningGroupIncludesExtendedWarnings W_extra             = True
warningGroupIncludesExtendedWarnings W_all               = True
warningGroupIncludesExtendedWarnings W_everything        = True

-- | Warning groups.
--
-- As all warnings are in the 'W_everything' set, it is ignored when
-- displaying to the user which group a warning is in.
warningGroups :: [WarningGroup]
warningGroups = [minBound..maxBound]

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
-- The special-case 'W_everything' group is not included.
warningHierarchies :: [[WarningGroup]]
warningHierarchies = hierarchies ++ map (:[]) rest
  where
    hierarchies = [[W_default, W_extra, W_all]]
    rest = filter (`notElem` W_everything : concat hierarchies) warningGroups

-- | Find the smallest group in every hierarchy which a warning
-- belongs to, excluding Weverything.
smallestWarningGroups :: WarningFlag -> [WarningGroup]
smallestWarningGroups flag = mapMaybe go warningHierarchies where
    -- Because each hierarchy is arranged from smallest to largest,
    -- the first group we find in a hierarchy which contains the flag
    -- is the smallest.
    go (group:rest) = fromMaybe (go rest) $ do
        guard (flag `elem` warningGroupFlags group)
        pure (Just group)
    go [] = Nothing

-- | The smallest group in every hierarchy to which a custom warning
-- category belongs is currently always @-Wextended-warnings@.
-- See Note [Warning categories] in "GHC.Unit.Module.Warnings".
smallestWarningGroupsForCategory :: [WarningGroup]
smallestWarningGroupsForCategory = [W_extended_warnings]

-- | Warnings enabled unless specified otherwise
standardWarnings :: [WarningFlag]
standardWarnings -- see Note [Documenting warning flags]
    = [ Opt_WarnOverlappingPatterns,
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
        Opt_WarnUnicodeBidirectionalFormatCharacters,
        Opt_WarnGADTMonoLocalBinds,
        Opt_WarnBadlyStagedTypes,
        Opt_WarnTypeEqualityRequiresOperators,
        Opt_WarnInconsistentFlags,
        Opt_WarnDataKindsTC,
        Opt_WarnTypeEqualityOutOfScope,
        Opt_WarnViewPatternSignatures,
        Opt_WarnUselessSpecialisations,
        Opt_WarnDeprecatedPragmas,
        Opt_WarnRuleLhsEqualities
      ]

-- | Things you get with @-W@.
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

-- | Things you get with @-Wall@.
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
        Opt_WarnIncompletePatternsRecUpd,
        Opt_WarnIncompleteExportWarnings,
        Opt_WarnIncompleteRecordSelectors,
        Opt_WarnDerivingTypeable
      ]

-- | Things you get with @-Weverything@, i.e. *all* known warnings flags.
minusWeverythingOpts :: [WarningFlag]
minusWeverythingOpts = [ toEnum 0 .. ]

-- | Things you get with @-Wcompat@.
--
-- This is intended to group together warnings that will be enabled by default
-- at some point in the future, so that library authors eager to make their
-- code future compatible to fix issues before they even generate warnings.
minusWcompatOpts :: [WarningFlag]
minusWcompatOpts
    = [ Opt_WarnImplicitRhsQuantification
      ]

-- | Things you get with @-Wunused-binds@.
unusedBindsFlags :: [WarningFlag]
unusedBindsFlags = [ Opt_WarnUnusedTopBinds
                   , Opt_WarnUnusedLocalBinds
                   , Opt_WarnUnusedPatternBinds
                   ]
