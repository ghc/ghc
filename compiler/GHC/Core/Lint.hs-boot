module GHC.Core.Lint
  ( LintConfig
  , WarnsAndErrs
  , lintCoercion_InScope
  ) where

import GHC.Core.TyCo.Rep ( Coercion )
import GHC.Data.Bag ( Bag )
import GHC.Types.Var.Env ( InScopeSet )
import GHC.Utils.Outputable ( SDoc )

data LintConfig

type WarnsAndErrs = (Bag SDoc, Bag SDoc)

lintCoercion_InScope :: LintConfig -> InScopeSet -> Coercion -> WarnsAndErrs
