{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic {DriverMessage, GhcMessage}

module GHC.Driver.Errors.Ppr (
  -- This module only exports Diagnostic instances.
  ) where

import GHC.Prelude

import GHC.Driver.Errors.Types
import GHC.Driver.Flags
import GHC.Driver.DynFlags
import GHC.HsToCore.Errors.Ppr () -- instance Diagnostic DsMessage
import GHC.Parser.Errors.Ppr () -- instance Diagnostic PsMessage
import GHC.Types.Error
import GHC.Types.Error.Codes
import GHC.Unit.Types
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Unit.Module
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary
import GHC.Unit.State
import GHC.Types.Hint
import GHC.Types.SrcLoc
import Data.Version

import Language.Haskell.Syntax.Decls (RuleDecl(..))
import GHC.Tc.Errors.Types (TcRnMessage)
import GHC.HsToCore.Errors.Types (DsMessage)
import GHC.Iface.Errors.Types
import GHC.Tc.Errors.Ppr () -- instance Diagnostic TcRnMessage
import GHC.Iface.Errors.Ppr () -- instance Diagnostic IfaceMessage
import GHC.CmmToLlvm.Version (llvmVersionStr, supportedLlvmVersionLowerBound, supportedLlvmVersionUpperBound)

--
-- Suggestions
--

-- | Suggests a list of 'InstantiationSuggestion' for the '.hsig' file to the user.
suggestInstantiatedWith :: ModuleName -> GenInstantiations UnitId -> [InstantiationSuggestion]
suggestInstantiatedWith pi_mod_name insts =
  [ InstantiationSuggestion k v | (k,v) <- ((pi_mod_name, mkHoleModule pi_mod_name) : insts) ]

instance HasDefaultDiagnosticOpts GhcMessageOpts where
  defaultOpts = GhcMessageOpts (defaultDiagnosticOpts @PsMessage)
                                         (defaultDiagnosticOpts @TcRnMessage)
                                         (defaultDiagnosticOpts @DsMessage)
                                         (defaultDiagnosticOpts @DriverMessage)

instance Diagnostic GhcMessage where
  type DiagnosticOpts GhcMessage = GhcMessageOpts
  diagnosticMessage opts = \case
    GhcPsMessage m
      -> diagnosticMessage (psMessageOpts opts) m
    GhcTcRnMessage m
      -> diagnosticMessage (tcMessageOpts opts) m
    GhcDsMessage m
      -> diagnosticMessage (dsMessageOpts opts) m
    GhcDriverMessage m
      -> diagnosticMessage (driverMessageOpts opts) m
    GhcUnknownMessage (UnknownDiagnostic f _ m)
      -> diagnosticMessage (f opts) m

  diagnosticReason = \case
    GhcPsMessage m
      -> diagnosticReason m
    GhcTcRnMessage m
      -> diagnosticReason m
    GhcDsMessage m
      -> diagnosticReason m
    GhcDriverMessage m
      -> diagnosticReason m
    GhcUnknownMessage m
      -> diagnosticReason m

  diagnosticHints = \case
    GhcPsMessage m
      -> diagnosticHints m
    GhcTcRnMessage m
      -> diagnosticHints m
    GhcDsMessage m
      -> diagnosticHints m
    GhcDriverMessage m
      -> diagnosticHints m
    GhcUnknownMessage m
      -> diagnosticHints m

  diagnosticCode = constructorCode @GHC

instance HasDefaultDiagnosticOpts DriverMessageOpts where
  defaultOpts = DriverMessageOpts (defaultDiagnosticOpts @PsMessage) (defaultDiagnosticOpts @IfaceMessage)

instance Diagnostic DriverMessage where
  type DiagnosticOpts DriverMessage = DriverMessageOpts
  diagnosticMessage opts = \case
    DriverUnknownMessage (UnknownDiagnostic f _ m)
      -> diagnosticMessage (f opts) m
    DriverPsHeaderMessage m
      -> diagnosticMessage (psDiagnosticOpts opts) m
    DriverMissingHomeModules uid missing buildingCabalPackage
      -> let msg | buildingCabalPackage == YesBuildingCabalPackage
                 = hang
                     (text "These modules are needed for compilation but not listed in your .cabal file's other-modules for" <+> quotes (ppr uid) <+> text ":")
                     4
                     (sep (map ppr missing))
                 | otherwise
                 =
                   hang
                     (text "Modules are not listed in options for"
                        <+> quotes (ppr uid) <+> text "but needed for compilation:")
                     4
                     (sep (map ppr missing))
         in mkSimpleDecorated msg
    DriverUnknownHiddenModules uid missing
      -> let msg = hang
                     (text "Modules are listed as hidden in options for" <+> quotes (ppr uid) <+> text "but not part of the unit:")
                     4
                     (sep (map ppr missing))
         in mkSimpleDecorated msg
    DriverUnknownReexportedModules uid missing
      -> let msg = hang
                     (text "Modules are listed as reexported in options for" <+> quotes (ppr uid) <+> text "but can't be found in any dependency:")
                     4
                     (sep (map ppr missing))
         in mkSimpleDecorated msg
    DriverUnusedPackages unusedArgs
      -> let msg = vcat [ text "The following packages were specified" <+>
                          text "via -package or -package-id flags,"
                        , text "but were not needed for compilation:"
                        , nest 2 (vcat (map (withDash . displayOneUnused) unusedArgs))
                        ]
         in mkSimpleDecorated msg
         where
            withDash :: SDoc -> SDoc
            withDash = (<+>) (text "-")

            displayOneUnused (_uid, pn , v, f) =
              ppr pn <> text "-"  <> text (showVersion v)
                     <+> parens (suffix f)

            suffix f = text "exposed by flag" <+> pprUnusedArg f

            pprUnusedArg :: PackageArg -> SDoc
            pprUnusedArg (PackageArg str) = text "-package" <+> text str
            pprUnusedArg (UnitIdArg uid) = text "-package-id" <+> ppr uid

    DriverUnnecessarySourceImports mod
      -> mkSimpleDecorated (text "{-# SOURCE #-} unnecessary in import of " <+> quotes (ppr mod))
    DriverDuplicatedModuleDeclaration mod files
      -> mkSimpleDecorated $
           text "module" <+> quotes (ppr mod) <+>
           text "is defined in multiple files:" <+>
           sep (map text files)
    DriverModuleNotFound mod
      -> mkSimpleDecorated (text "module" <+> quotes (ppr mod) <+> text "cannot be found locally")
    DriverFileModuleNameMismatch actual expected
      -> mkSimpleDecorated $
           text "File name does not match module name:"
           $$ text "Saw     :" <+> quotes (ppr actual)
           $$ text "Expected:" <+> quotes (ppr expected)

    DriverUnexpectedSignature pi_mod_name _buildingCabalPackage _instantiations
      -> mkSimpleDecorated $ text "Unexpected signature:" <+> quotes (ppr pi_mod_name)
    DriverFileNotFound hsFilePath
      -> mkSimpleDecorated (text "Can't find" <+> text hsFilePath)
    DriverStaticPointersNotSupported
      -> mkSimpleDecorated (text "StaticPointers is not supported in GHCi interactive expressions.")
    DriverBackpackModuleNotFound modname
      -> mkSimpleDecorated (text "module" <+> ppr modname <+> text "was not found")
    DriverUserDefinedRuleIgnored (HsRule { rd_name = n })
      -> mkSimpleDecorated $
            text "Rule \"" <> ftext (unLoc n) <> text "\" ignored" $+$
            text "Defining user rules is disabled under Safe Haskell"
    DriverMixedSafetyImport modName
      -> mkSimpleDecorated $
           text "Module" <+> ppr modName <+> text ("is imported both as a safe and unsafe import!")
    DriverCannotLoadInterfaceFile m
      -> mkSimpleDecorated $
           text "Can't load the interface file for" <+> ppr m
           <> text ", to check that it can be safely imported"
    DriverInferredSafeModule m
      -> mkSimpleDecorated $
           quotes (ppr $ moduleName m) <+> text "has been inferred as safe!"
    DriverInferredSafeImport m
      -> mkSimpleDecorated $
           sep
             [ text "Importing Safe-Inferred module "
                 <> ppr (moduleName m)
                 <> text " from explicitly Safe module"
             ]
    DriverMarkedTrustworthyButInferredSafe m
      -> mkSimpleDecorated $
           quotes (ppr $ moduleName m) <+> text "is marked as Trustworthy but has been inferred as safe!"
    DriverCannotImportUnsafeModule m
      -> mkSimpleDecorated $
           sep [ ppr (moduleName m)
                   <> text ": Can't be safely imported!"
               , text "The module itself isn't safe." ]
    DriverMissingSafeHaskellMode modName
      -> mkSimpleDecorated $
           ppr modName <+> text "is missing Safe Haskell mode"
    DriverPackageNotTrusted state pkg
      -> mkSimpleDecorated $
           pprWithUnitState state
             $ text "The package ("
                <> ppr pkg
                <> text ") is required to be trusted but it isn't!"
    DriverCannotImportFromUntrustedPackage state m
      -> mkSimpleDecorated $
           sep [ ppr (moduleName m)
                   <> text ": Can't be safely imported!"
               , text "The package ("
                   <> (pprWithUnitState state $ ppr (moduleUnit m))
                   <> text ") the module resides in isn't trusted."
               ]
    DriverRedirectedNoMain mod_name
      -> mkSimpleDecorated $ (text
                       ("Output was redirected with -o, " ++
                       "but no output will be generated.") $$
                       (text "There is no module named" <+>
                       quotes (ppr mod_name) <> text "."))
    DriverHomePackagesNotClosed needed_unit_ids
      -> mkSimpleDecorated $ vcat ([text "Home units are not closed."
                                  , text "It is necessary to also load the following units:" ]
                                  ++ map (\uid -> text "-" <+> ppr uid) needed_unit_ids)
    DriverInterfaceError reason -> diagnosticMessage (ifaceDiagnosticOpts opts) reason

    DriverInconsistentDynFlags msg
      -> mkSimpleDecorated $ text msg
    DriverSafeHaskellIgnoredExtension ext
      -> let arg = text "-X" <> ppr ext
         in mkSimpleDecorated $ arg <+> text "is not allowed in Safe Haskell; ignoring" <+> arg
    DriverPackageTrustIgnored
      -> mkSimpleDecorated $ text "-fpackage-trust ignored; must be specified with a Safe Haskell flag"

    DriverUnrecognisedFlag arg
      -> mkSimpleDecorated $ text $ "unrecognised warning flag: -" ++ arg
    DriverDeprecatedFlag arg msg
      -> mkSimpleDecorated $ text $ arg ++ " is deprecated: " ++ msg
    DriverModuleGraphCycle path
      -> mkSimpleDecorated $ vcat
        [ text "Module graph contains a cycle:"
        , nest 2 (show_path path) ]
      where
        show_path :: [ModuleGraphNode] -> SDoc
        show_path []  = panic "show_path"
        show_path [m] = ppr_node m <+> text "imports itself"
        show_path (m1:m2:ms) = vcat ( nest 14 (ppr_node m1)
                                    : nest 6 (text "imports" <+> ppr_node m2)
                                    : go ms )
           where
             go []     = [text "which imports" <+> ppr_node m1]
             go (m:ms) = (text "which imports" <+> ppr_node m) : go ms

        ppr_node :: ModuleGraphNode -> SDoc
        ppr_node (ModuleNode _deps m) = text "module" <+> ppr_ms m
        ppr_node (InstantiationNode _uid u) = text "instantiated unit" <+> ppr u
        ppr_node (LinkNode uid _) = pprPanic "LinkNode should not be in a cycle" (ppr uid)
        ppr_node (UnitNode uid _) = pprPanic "UnitNode should not be in a cycle" (ppr uid)

        ppr_ms :: ModSummary -> SDoc
        ppr_ms ms = quotes (ppr (moduleName (ms_mod ms))) <+>
                    (parens (text (msHsFilePath ms)))
    DriverInstantiationNodeInDependencyGeneration node ->
      mkSimpleDecorated $
        vcat [ text "Unexpected backpack instantiation in dependency graph while constructing Makefile:"
             , nest 2 $ ppr node ]
    DriverNoConfiguredLLVMToolchain ->
      mkSimpleDecorated $
        text "GHC was not configured with a supported LLVM toolchain" $$
          text ("Make sure you have installed LLVM between ["
            ++ llvmVersionStr supportedLlvmVersionLowerBound
            ++ " and "
            ++ llvmVersionStr supportedLlvmVersionUpperBound
            ++ ") and reinstall GHC to make -fllvm work")

  diagnosticReason = \case
    DriverUnknownMessage m
      -> diagnosticReason m
    DriverPsHeaderMessage {}
      -> ErrorWithoutFlag
    DriverMissingHomeModules{}
      -> WarningWithFlag Opt_WarnMissingHomeModules
    DriverUnknownHiddenModules {}
      -> ErrorWithoutFlag
    DriverUnknownReexportedModules {}
      -> ErrorWithoutFlag
    DriverUnusedPackages{}
      -> WarningWithFlag Opt_WarnUnusedPackages
    DriverUnnecessarySourceImports{}
      -> WarningWithFlag Opt_WarnUnusedImports
    DriverDuplicatedModuleDeclaration{}
      -> ErrorWithoutFlag
    DriverModuleNotFound{}
      -> ErrorWithoutFlag
    DriverFileModuleNameMismatch{}
      -> ErrorWithoutFlag
    DriverUnexpectedSignature{}
      -> ErrorWithoutFlag
    DriverFileNotFound{}
      -> ErrorWithoutFlag
    DriverStaticPointersNotSupported
      -> WarningWithoutFlag
    DriverBackpackModuleNotFound{}
      -> ErrorWithoutFlag
    DriverUserDefinedRuleIgnored{}
      -> WarningWithoutFlag
    DriverMixedSafetyImport{}
      -> ErrorWithoutFlag
    DriverCannotLoadInterfaceFile{}
      -> ErrorWithoutFlag
    DriverInferredSafeModule{}
      -> WarningWithFlag Opt_WarnSafe
    DriverMarkedTrustworthyButInferredSafe{}
      ->WarningWithFlag Opt_WarnTrustworthySafe
    DriverInferredSafeImport{}
      -> WarningWithFlag Opt_WarnInferredSafeImports
    DriverCannotImportUnsafeModule{}
      -> ErrorWithoutFlag
    DriverMissingSafeHaskellMode{}
      -> WarningWithFlag Opt_WarnMissingSafeHaskellMode
    DriverPackageNotTrusted{}
      -> ErrorWithoutFlag
    DriverCannotImportFromUntrustedPackage{}
      -> ErrorWithoutFlag
    DriverRedirectedNoMain {}
      -> ErrorWithoutFlag
    DriverHomePackagesNotClosed {}
      -> ErrorWithoutFlag
    DriverInterfaceError reason -> diagnosticReason reason
    DriverInconsistentDynFlags {}
      -> WarningWithFlag Opt_WarnInconsistentFlags
    DriverSafeHaskellIgnoredExtension {}
      -> WarningWithoutFlag
    DriverPackageTrustIgnored {}
      -> WarningWithoutFlag
    DriverUnrecognisedFlag {}
      -> WarningWithFlag Opt_WarnUnrecognisedWarningFlags
    DriverDeprecatedFlag {}
      -> WarningWithFlag Opt_WarnDeprecatedFlags
    DriverModuleGraphCycle {}
      -> ErrorWithoutFlag
    DriverInstantiationNodeInDependencyGeneration {}
      -> ErrorWithoutFlag
    DriverNoConfiguredLLVMToolchain
      -> ErrorWithoutFlag

  diagnosticHints = \case
    DriverUnknownMessage m
      -> diagnosticHints m
    DriverPsHeaderMessage psMsg
      -> diagnosticHints psMsg
    DriverMissingHomeModules{}
      -> noHints
    DriverUnknownHiddenModules {}
      -> noHints
    DriverUnknownReexportedModules {}
      -> noHints
    DriverUnusedPackages{}
      -> noHints
    DriverUnnecessarySourceImports{}
      -> noHints
    DriverDuplicatedModuleDeclaration{}
      -> noHints
    DriverModuleNotFound{}
      -> noHints
    DriverFileModuleNameMismatch{}
      -> noHints
    DriverUnexpectedSignature pi_mod_name buildingCabalPackage instantiations
      -> if buildingCabalPackage == YesBuildingCabalPackage
           then [SuggestAddSignatureCabalFile pi_mod_name]
           else [SuggestSignatureInstantiations pi_mod_name (suggestInstantiatedWith pi_mod_name instantiations)]
    DriverFileNotFound{}
      -> noHints
    DriverStaticPointersNotSupported
      -> noHints
    DriverBackpackModuleNotFound{}
      -> noHints
    DriverUserDefinedRuleIgnored{}
      -> noHints
    DriverMixedSafetyImport{}
      -> noHints
    DriverCannotLoadInterfaceFile{}
      -> noHints
    DriverInferredSafeModule{}
      -> noHints
    DriverInferredSafeImport{}
      -> noHints
    DriverCannotImportUnsafeModule{}
      -> noHints
    DriverMissingSafeHaskellMode{}
      -> noHints
    DriverPackageNotTrusted{}
      -> noHints
    DriverMarkedTrustworthyButInferredSafe{}
      -> noHints
    DriverCannotImportFromUntrustedPackage{}
      -> noHints
    DriverRedirectedNoMain {}
      -> noHints
    DriverHomePackagesNotClosed {}
      -> noHints
    DriverInterfaceError reason -> diagnosticHints reason
    DriverInconsistentDynFlags {}
      -> noHints
    DriverSafeHaskellIgnoredExtension {}
      -> noHints
    DriverPackageTrustIgnored {}
      -> noHints
    DriverUnrecognisedFlag {}
      -> noHints
    DriverDeprecatedFlag {}
      -> noHints
    DriverModuleGraphCycle {}
      -> noHints
    DriverInstantiationNodeInDependencyGeneration {}
      -> noHints
    DriverNoConfiguredLLVMToolchain
      -> noHints

  diagnosticCode = constructorCode @GHC
