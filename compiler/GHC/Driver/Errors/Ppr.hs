{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic {DriverMessage, GhcMessage}

module GHC.Driver.Errors.Ppr where

import GHC.Prelude

import GHC.Driver.Errors.Types
import GHC.Driver.Flags
import GHC.Driver.Session
import GHC.HsToCore.Errors.Ppr ()
import GHC.Parser.Errors.Ppr ()
import GHC.Tc.Errors.Ppr ()
import GHC.Types.Error
import GHC.Unit.Types
import GHC.Utils.Outputable
import GHC.Unit.Module
import GHC.Unit.State
import GHC.Types.Hint
import GHC.Types.SrcLoc
import Data.Version

import Language.Haskell.Syntax.Decls (RuleDecl(..))

--
-- Suggestions
--

-- | Suggests a list of 'InstantiationSuggestion' for the '.hsig' file to the user.
suggestInstantiatedWith :: ModuleName -> GenInstantiations UnitId -> [InstantiationSuggestion]
suggestInstantiatedWith pi_mod_name insts =
  [ InstantiationSuggestion k v | (k,v) <- ((pi_mod_name, mkHoleModule pi_mod_name) : insts) ]


instance Diagnostic GhcMessage where
  diagnosticMessage = \case
    GhcPsMessage m
      -> diagnosticMessage m
    GhcTcRnMessage m
      -> diagnosticMessage m
    GhcDsMessage m
      -> diagnosticMessage m
    GhcDriverMessage m
      -> diagnosticMessage m
    GhcUnknownMessage m
      -> diagnosticMessage m

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

instance Diagnostic DriverMessage where
  diagnosticMessage = \case
    DriverUnknownMessage m
      -> diagnosticMessage m
    DriverPsHeaderMessage m
      -> diagnosticMessage m
    DriverMissingHomeModules missing buildingCabalPackage
      -> let msg | buildingCabalPackage == YesBuildingCabalPackage
                 = hang
                     (text "These modules are needed for compilation but not listed in your .cabal file's other-modules: ")
                     4
                     (sep (map ppr missing))
                 | otherwise
                 =
                   hang
                     (text "Modules are not listed in command line but needed for compilation: ")
                     4
                     (sep (map ppr missing))
         in mkSimpleDecorated msg
    DriverUnknownHiddenModules missing
      -> let msg = hang
                     (text "Modules are listened as hidden but not part of the unit: ")
                     4
                     (sep (map ppr missing))
         in mkSimpleDecorated msg
    DriverUnknownReexportedModules missing
      -> let msg = hang
                     (text "Modules are listened as reexported but can't be found in any dependency: ")
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
            text "Rule \"" <> ftext (snd $ unLoc n) <> text "\" ignored" $+$
            text "User defined rules are disabled under Safe Haskell"
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
