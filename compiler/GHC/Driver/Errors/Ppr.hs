{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic {DriverMessage, GhcMessage}

module GHC.Driver.Errors.Ppr where

import GHC.Prelude

import GHC.Driver.Errors.Types
import GHC.Driver.Flags
import GHC.Driver.Session
import GHC.HsToCore.Errors.Ppr ()
import GHC.Parser.Errors.Ppr (pprPsError)
import GHC.Tc.Errors.Ppr ()
import GHC.Types.Error
import GHC.Unit.Types
import GHC.Utils.Outputable
import GHC.Unit.Module

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

instance Diagnostic DriverMessage where
  diagnosticMessage = \case
    DriverUnknownMessage m
      -> diagnosticMessage m
    DriverPsHeaderMessage desc hints
      -> mkSimpleDecorated $ pprPsError desc hints
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

    DriverUnexpectedSignature pi_mod_name buildingCabalPackage suggestions
      -> let suggested_instantiated_with =
               hcat (punctuate comma $
                   [ ppr k <> text "=" <> ppr v
                   | InstantiationSuggestion k v <- suggestions
                   ])
             msg = text "Unexpected signature:" <+> quotes (ppr pi_mod_name)
                   $$ if buildingCabalPackage == YesBuildingCabalPackage
                       then parens (text "Try adding" <+> quotes (ppr pi_mod_name)
                               <+> text "to the"
                               <+> quotes (text "signatures")
                               <+> text "field in your Cabal file.")
                       else parens (text "Try passing -instantiated-with=\"" <>
                                    suggested_instantiated_with <> text "\"" $$
                                       text "replacing <" <> ppr pi_mod_name <> text "> as necessary.")
         in mkSimpleDecorated msg
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
