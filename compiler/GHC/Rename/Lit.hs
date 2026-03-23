{-# LANGUAGE RecordWildCards #-}

module GHC.Rename.Lit where

import GHC.Prelude

import GHC.Builtin.Names
import GHC.Hs
import GHC.Rename.Env (lookupNameWithQualifier)
import GHC.Rename.Utils (genHsApps, genLHsLit)
import GHC.Tc.Utils.Monad (RnM)
import GHC.Types.Name.Set (FreeNames)
import GHC.Types.SrcLoc (GenLocated(..))

rnQualLit :: HsQualLit GhcPs -> RnM ((HsQualLit GhcRn, HsExpr GhcRn), FreeNames)
rnQualLit QualLit{..} = do
  let (funNameBase, hsLit) =
        case ql_val of
          -- See Note [Implementation of QualifiedStrings]
          HsQualString st s -> (fromStringClassOpKey, HsString st s)
  (funName, fvs) <- lookupNameWithQualifier ql_mod funNameBase
  let lit = QualLit{ql_ext = L noAnn funName, ..}
  let expr = genHsApps funName [genLHsLit hsLit]
  pure ((lit, expr), fvs)
