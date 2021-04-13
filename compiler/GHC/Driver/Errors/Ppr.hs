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
import GHC.Types.SrcLoc
import GHC.Utils.Error
import GHC.Utils.Outputable

-- | Constructs a new 'GhcMessage' out of 'DriverMessage'.
-- N.B. Due to circular dependencies we can't define this function inside
-- 'GHC.Driver.Errors.Types', where it would naturally belong.
ghcDriverMessage :: DynFlags -> SrcSpan -> DriverMessage -> MsgEnvelope GhcMessage
ghcDriverMessage dflags locn msg = GhcDriverMessage <$> mkMsgEnvelope dflags locn alwaysQualify msg

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
    DriverMissingHomeModules missing (BuildingCabalPackage buildingCabalPackage)
      -> let msg | buildingCabalPackage
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
         in mkDecorated [msg]
    DriverUnusedPackages unusedArgs
      -> let msg = vcat [ text "The following packages were specified" <+>
                          text "via -package or -package-id flags,"
                        , text "but were not needed for compilation:"
                        , nest 2 (vcat (map (withDash . pprUnusedArg) unusedArgs))
                        ]
         in mkDecorated [msg]
         where
            withDash :: SDoc -> SDoc
            withDash = (<+>) (text "-")

            pprUnusedArg :: PackageArg -> SDoc
            pprUnusedArg (PackageArg str) = text str
            pprUnusedArg (UnitIdArg uid) = ppr uid

  diagnosticReason = \case
    DriverUnknownMessage m
      -> diagnosticReason m
    DriverMissingHomeModules{}
      -> WarningWithFlag Opt_WarnMissingHomeModules
    DriverUnusedPackages{}
      -> WarningWithFlag Opt_WarnUnusedPackages
