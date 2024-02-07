{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module RewritePlugin where
-- Rewriting type family applications.

-- base
import Data.Maybe
  ( catMaybes )

-- ghc
import GHC.Builtin.Types
  ( unitTy )
import GHC.Core
  ( Expr(Coercion) )
import GHC.Core.Coercion
  ( Coercion, mkUnivCo )
import GHC.Core.Predicate
  ( EqRel(NomEq), Pred(EqPred)
  , classifyPredType
  )
import GHC.Core.Reduction
  ( Reduction(..) )
import GHC.Core.TyCo.Rep
  ( Type, UnivCoProvenance(PluginProv) )
import GHC.Core.TyCon
  ( TyCon )
import GHC.Core.TyCo.Compare
  ( eqType )
import GHC.Core.Type
  ( mkTyConApp, splitTyConApp_maybe )
import GHC.Plugins
  ( Plugin )
import GHC.Tc.Plugin
  ( TcPluginM
  , unsafeTcPluginTcM
  )
import GHC.Tc.Types
  ( RewriteEnv
  , TcPluginRewriter, TcPluginRewriteResult(..)
  )
import GHC.Tc.Types.Constraint
  ( Ct(..), CanEqLHS(..)
  , ctPred
  )
import GHC.Tc.Types.Evidence
  ( EvTerm(EvExpr), Role(Nominal) )
import GHC.Types.Unique.DSet
  ( emptyUniqDSet )
import GHC.Types.Unique.FM
  ( UniqFM, listToUFM )

-- common
import Common
  ( PluginDefs(..)
  , mkPlugin, don'tSolve
  )

--------------------------------------------------------------------------------

-- This plugin rewrites @Add a Zero@ to @a@ and @Add Zero a@ to @a@,
-- by using the plugin rewriting functionality,
-- and not the constraint solver plugin functionality.

plugin :: Plugin
plugin = mkPlugin don'tSolve rewriter

rewriter :: [String]
         -> PluginDefs
         -> UniqFM TyCon TcPluginRewriter
rewriter _args defs@( PluginDefs { add } ) =
  listToUFM
    [ ( add, rewriteAdd defs ) ]

rewriteAdd :: PluginDefs -> RewriteEnv -> [ Ct ] -> [ Type ] -> TcPluginM TcPluginRewriteResult
rewriteAdd ( PluginDefs { .. } ) _env givens args@[ arg1, arg2 ]
  | Just ( tyCon, [] ) <- splitTyConApp_maybe arg1
  , tyCon == zero
  = pure $ TcPluginRewriteTo ( mkTyFamReduction add args arg2 ) []
  | Just ( tyCon, [] ) <- splitTyConApp_maybe arg2
  , tyCon == zero
  = pure $ TcPluginRewriteTo ( mkTyFamReduction add args arg1 ) []
rewriteAdd _ _ _ _ = pure TcPluginNoRewrite


mkTyFamReduction :: TyCon -> [ Type ] -> Type -> Reduction
mkTyFamReduction tyCon args res = Reduction co res
  where
    co :: Coercion
    co = mkUnivCo ( PluginProv "RewritePlugin" emptyUniqDSet) Nominal  -- Empty is fine. This plugin does not use "givens".
           ( mkTyConApp tyCon args ) res
