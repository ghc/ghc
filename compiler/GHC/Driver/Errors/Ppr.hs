{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Driver.Errors.Ppr () where

import GHC.Prelude

import GHC.Core.InstEnv ( is_flag, is_dfun )
import GHC.Driver.Session ( unitState, unsafeFlagsForInfer )
import GHC.Driver.Errors.Types
import GHC.Parser.Errors.Ppr ()
import GHC.Types.Basic
import GHC.Types.Error ( RenderableDiagnostic
                       , renderDiagnostic
                       , errDoc
                       , Severity(..)
                       , mkLocMessage
                       , getWarningMessages
                       )
import GHC.Types.Name ( nameSrcSpan, getName )
import GHC.Unit.Finder ( cannotFindModule )
import GHC.Unit.State  ( pprWithUnitState )
import GHC.Utils.Outputable
import GHC.Utils.Error ( pprErrMsgBagWithLoc )

instance RenderableDiagnostic GhcWarning where
  renderDiagnostic = \case
    GhcWarningPs w      -> renderDiagnostic w
    GhcWarningCmdLine w -> renderDiagnostic w
    GhcWarningTcRn w    -> renderDiagnostic w
    GhcWarningDs w      -> renderDiagnostic w
    GhcWarningDriver w  -> renderDiagnostic w
    GhcWarningRaw d     -> d

instance RenderableDiagnostic GhcError where
  renderDiagnostic = \case
    GhcErrorPs e      -> renderDiagnostic e
    GhcErrorTcRn e    -> renderDiagnostic e
    GhcErrorDs e      -> renderDiagnostic e
    GhcErrorDriver e  -> renderDiagnostic e
    GhcFatalWarning e -> renderDiagnostic e
    GhcErrorRaw d     -> d

instance RenderableDiagnostic DriverError where
  renderDiagnostic = \case

    DriverError d
      -> d

    DriverCannotFindModule dflags m res
      -> errDoc [cannotFindModule dflags m res] [] []

    DriverNotAnExpression str
      -> errDoc [text "not an expression:" <+> quotes (text str)] [] []

    DriverParseErrorImport
      -> errDoc [text "parse error in import declaration"] [] []

    DriverPkgRequiredTrusted dflags pkg
      -> errDoc [ pprWithUnitState (unitState dflags) $
                  text "The package ("
                     <> ppr pkg
                     <> text ") is required to be trusted but it isn't!"
                ] [] []

    DriverCantLoadIfaceForSafe m
      -> errDoc [ text "Can't load the interface file for" <+> ppr m
               <> text ", to check that it can be safely imported"
               ] [] []

instance RenderableDiagnostic DriverWarning where
  renderDiagnostic = \case
    WarnModuleInferredUnsafe df modName badInsts whyUnsafe
      -> errDoc [ vcat [ quotes (ppr modName) <+> text "has been inferred as unsafe!"
                       , text "Reason:"
                       , nest 4 $ (vcat $ badFlags df) $+$
                                  (vcat $ getReasons whyUnsafe) $+$
                                  (vcat $ concatMap badInst badInsts)
                       ]
               ] [] []
         where
           getReasons = pprErrMsgBagWithLoc . getWarningMessages . fmap renderDiagnostic

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
