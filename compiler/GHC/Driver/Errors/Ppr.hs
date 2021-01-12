{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Driver.Errors.Ppr where

import GHC.Prelude

import GHC.Core.InstEnv ( is_flag, is_dfun )
import GHC.Driver.Session
import GHC.Types.Basic
import GHC.Types.Error
import GHC.Types.Name ( nameSrcSpan, getName )
import GHC.Driver.Errors.Types
import GHC.Iface.Load  ( cannotFindModule )
import GHC.Unit.State
import GHC.Utils.Error ( pprErrMsgBagWithLoc )
import GHC.Utils.Outputable

import GHC.Parser.Errors.Ppr ()
import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr ()
import GHC.HsToCore.Errors.Ppr ()

instance RenderableDiagnostic GhcMessage where
  renderDiagnostic = \case
    GhcPsMessage m
      -> renderDiagnostic m
    GhcTcRnMessage m
      -> renderDiagnostic m
    GhcDsMessage m
      -> renderDiagnostic m
    GhcDriverMessage m
      -> renderDiagnostic m
    GhcUnknownMessage m
      -> renderDiagnostic m

instance RenderableDiagnostic DriverMessage where
  renderDiagnostic = \case

    DriverUnknownMessage d
      -> d

    DriverCannotFindModule env m res
      -> errDoc [cannotFindModule env m res] [] []

    DriverNotAnExpression str
      -> errDoc [text "not an expression:" <+> quotes (text str)] [] []

    DriverParseErrorImport
      -> errDoc [text "parse error in import declaration"] [] []

    DriverPkgRequiredTrusted state pkg
      -> errDoc [ pprWithUnitState state $
                  text "The package ("
                     <> ppr pkg
                     <> text ") is required to be trusted but it isn't!"
                ] [] []

    DriverCantLoadIfaceForSafe m
      -> errDoc [ text "Can't load the interface file for" <+> ppr m
               <> text ", to check that it can be safely imported"
               ] [] []

    DriverWarnModuleInferredUnsafe df modName badInsts whyUnsafe
      -> errDoc [ vcat [ quotes (ppr modName) <+> text "has been inferred as unsafe!"
                       , text "Reason:"
                       , nest 4 $ (vcat $ badFlags df) $+$
                                  (vcat $ getReasons whyUnsafe) $+$
                                  (vcat $ concatMap badInst badInsts)
                       ]
               ] [] []
         where
           getReasons :: Messages TcRnMessage -> [SDoc]
           getReasons = pprErrMsgBagWithLoc . getMessages . fmap renderDiagnostic

           badFlags df   = concatMap (badFlag df) unsafeFlagsForInfer
           badFlag df (str,loc,on,_)
               | on df     = [mkLocMessage SevOutput (loc df) $
                                   text str <+> text "is not allowed in Safe Haskell"]
               | otherwise = []

           badInst ins | checkOverlap (overlapMode (is_flag ins))
                       = [mkLocMessage SevOutput (nameSrcSpan $ getName $ is_dfun ins) $
                             ppr (overlapMode $ is_flag ins) <+>
                             text "overlap mode isn't allowed in Safe Haskell"]
                       | otherwise = []

           checkOverlap (NoOverlap _) = False
           checkOverlap _             = True

    DriverWarnInferredSafeImports modName
      -> errDoc [ sep [ text "Importing Safe-Inferred module "
                      <> ppr modName
                      <> text " from explicitly Safe module"
                      ]
                ] [] []
