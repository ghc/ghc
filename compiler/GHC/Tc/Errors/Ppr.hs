{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- instance Diagnostic {TcRnDsMessage, TcRnMessage}

module GHC.Tc.Errors.Ppr (
    dodgyMsg
  , dodgyMsgInsert
  ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Driver.Flags
import GHC.Hs
import GHC.HsToCore.Errors.Ppr () -- instance Diagnostic DsMessage
import GHC.Tc.Errors.Types
import GHC.Types.Error
import GHC.Utils.Outputable

instance Diagnostic TcRnDsMessage where
  diagnosticMessage (TcRnDsMessage m) = either diagnosticMessage diagnosticMessage m
  diagnosticReason  (TcRnDsMessage m) = either diagnosticReason  diagnosticReason m

instance Diagnostic TcRnMessage where
  diagnosticMessage = \case
    TcRnUnknownMessage m
      -> diagnosticMessage m
    TcRnImplicitLift id_or_name errInfo
      -> mkDecorated [text "The variable" <+> quotes (ppr id_or_name) <+>
                      text "is implicitly lifted in the TH quotation"
                     , getErrInfo errInfo
                     ]
    TcRnUnusedPatternBinds bind
      -> mkDecorated [hang (text "This pattern-binding binds no variables:") 2 (ppr bind)]
    TcRnDodgyExports name
      -> mkDecorated [dodgyMsg (text "export") name (dodgyMsgInsert name :: IE GhcRn)]
  diagnosticReason = \case
    TcRnUnknownMessage m
      -> diagnosticReason m
    TcRnImplicitLift{}
      -> WarningWithFlag Opt_WarnImplicitLift
    TcRnUnusedPatternBinds{}
      -> WarningWithFlag Opt_WarnUnusedPatternBinds
    TcRnDodgyExports{}
      -> WarningWithFlag Opt_WarnDodgyExports

dodgyMsg :: (Outputable a, Outputable b) => SDoc -> a -> b -> SDoc
dodgyMsg kind tc ie
  = sep [ text "The" <+> kind <+> ptext (sLit "item")
                     <+> quotes (ppr ie)
                <+> text "suggests that",
          quotes (ppr tc) <+> text "has (in-scope) constructors or class methods,",
          text "but it has none" ]

dodgyMsgInsert :: forall p . IdP (GhcPass p) -> IE (GhcPass p)
dodgyMsgInsert tc = IEThingAll noAnn ii
  where
    ii :: LIEWrappedName (IdP (GhcPass p))
    ii = noLocA (IEName $ noLocA tc)
