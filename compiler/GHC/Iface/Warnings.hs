module GHC.Iface.Warnings
  ( toIfaceWarnings
  , toIfaceWarningTxt
  )
where

import GHC.Prelude

import GHC.Hs

import GHC.Iface.Syntax

import GHC.Types.SrcLoc ( unLoc )

import GHC.Unit.Module.Warnings

toIfaceWarnings :: Warnings GhcRn -> IfaceWarnings
toIfaceWarnings (WarnAll txt) = IfWarnAll (toIfaceWarningTxt txt)
toIfaceWarnings (WarnSome vs ds) = IfWarnSome vs' ds'
  where
    vs' = [(occ, toIfaceWarningTxt txt) | (occ, txt) <- vs]
    ds' = [(occ, toIfaceWarningTxt txt) | (occ, txt) <- ds]

toIfaceWarningTxt :: WarningTxt GhcRn -> IfaceWarningTxt
toIfaceWarningTxt (WarningTxt (src, _) mb_cat strs) = IfWarningTxt src (unLoc . iwc_wc . unLoc <$> mb_cat) (map (toIfaceStringLiteralWithNames . unLoc) strs)
toIfaceWarningTxt (DeprecatedTxt (src, _) strs) = IfDeprecatedTxt src (map (toIfaceStringLiteralWithNames . unLoc) strs)

toIfaceStringLiteralWithNames :: WithHsDocIdentifiers (StringLiteral GhcRn) GhcRn -> (IfaceStringLiteral, [IfExtName])
toIfaceStringLiteralWithNames (WithHsDocIdentifiers src names) = (toIfaceStringLiteral src, map unLoc names)

toIfaceStringLiteral :: StringLiteral GhcRn -> IfaceStringLiteral
toIfaceStringLiteral sLit = IfStringLiteral (stringLitSourceText sLit) (sl_fs sLit)
