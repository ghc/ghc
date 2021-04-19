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
import GHC.Types.Hint

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
    DriverUnusedPackages unusedArgs
      -> let msg = vcat [ text "The following packages were specified" <+>
                          text "via -package or -package-id flags,"
                        , text "but were not needed for compilation:"
                        , nest 2 (vcat (map (withDash . pprUnusedArg) unusedArgs))
                        ]
         in mkSimpleDecorated msg
         where
            withDash :: SDoc -> SDoc
            withDash = (<+>) (text "-")

            pprUnusedArg :: PackageArg -> SDoc
            pprUnusedArg (PackageArg str) = text str
            pprUnusedArg (UnitIdArg uid) = ppr uid
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

  diagnosticReason = \case
    DriverUnknownMessage m
      -> diagnosticReason m
    DriverPsHeaderMessage {}
      -> ErrorWithoutFlag
    DriverMissingHomeModules{}
      -> WarningWithFlag Opt_WarnMissingHomeModules
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

  diagnosticHints = \case
    DriverUnknownMessage m
      -> diagnosticHints m
    DriverPsHeaderMessage psMsg
      -> diagnosticHints psMsg
    DriverMissingHomeModules{}
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
