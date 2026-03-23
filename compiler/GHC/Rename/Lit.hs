{-# LANGUAGE RecordWildCards #-}

module GHC.Rename.Lit
  ( rnQualLit
  , rnOverLitVal
  , rnFractionalLit
  , rnIntegralLit
  , rnStringLit
  ) where

import GHC.Prelude

import GHC.Builtin.KnownOccs
import GHC.Hs.Extension (GhcPs, GhcRn)
import GHC.Hs.Lit
import GHC.Parser.Annotation (noAnn)
import GHC.Rename.Env (lookupNameWithQualifier)
import GHC.Rename.Utils (genHsApps, genLHsLit)
import GHC.Tc.Utils.Monad (RnM)
import GHC.Types.Name.Set (FreeNames)
import GHC.Types.SrcLoc (GenLocated(..))

import Language.Haskell.Syntax.Expr (HsExpr)

rnQualLit :: HsQualLit GhcPs -> RnM ((HsQualLit GhcRn, HsExpr GhcRn), FreeNames)
rnQualLit QualLit{..} = do
  let (funNameBase, hsLit, ql_val') =
        case ql_val of
          -- See Note [Implementation of QualifiedStrings]
          HsQualString st s -> (fromStringClassOpOcc, HsString st s, HsQualString st s)
  (funName, fvs) <- lookupNameWithQualifier ql_mod funNameBase
  let lit = QualLit{ql_ext = L noAnn funName, ql_val = ql_val', ..}
  let expr = genHsApps funName [genLHsLit hsLit]
  pure ((lit, expr), fvs)
