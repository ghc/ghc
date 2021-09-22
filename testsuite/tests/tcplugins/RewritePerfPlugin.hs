{-# LANGUAGE TupleSections #-}

module RewritePerfPlugin where
-- Testing performance of rewriting type-family applications.

-- ghc
import GHC.Core
  ( Expr(Coercion) )
import GHC.Core.Coercion
  ( mkUnivCo )
import GHC.Core.TyCo.Rep
  ( Type, UnivCoProvenance(PluginProv) )
import GHC.Core.TyCon
  ( TyCon )
import GHC.Core.Type
  ( eqType, mkTyConApp, splitTyConApp_maybe )
import GHC.Plugins
  ( Plugin(..), defaultPlugin, purePlugin )
import GHC.Tc.Plugin
  ( TcPluginM
  , findImportedModule, lookupOrig
  , tcLookupClass, tcLookupDataCon, tcLookupTyCon
  , unsafeTcPluginTcM
  )
import GHC.Tc.Types
  ( TcPlugin(..)
  , TcPluginSolveResult(..), TcPluginRewriteResult(..)
  , TcPluginRewriter, RewriteEnv
  )
import GHC.Tc.Types.Constraint
  ( Ct(..), CanEqLHS(..)
  , ctPred
  )
import GHC.Types.Name.Occurrence
  ( mkTcOcc )
import GHC.Types.Unique.FM
  ( UniqFM, listToUFM )
import GHC.Types.PkgQual
import GHC.Unit.Finder
  ( FindResult(..) )
import GHC.Unit.Module
  ( Module
  , mkModuleName
  )

--------------------------------------------------------------------------------

-- In this test, we write a plugin which returns "TcPluginNoRewrite"
-- for all the type families in RewritePerfDefs.
--
-- Comparing the result with T9872b gives an indication of the performance
-- impact of rewriting plugins in code that heavily rewrites type families.

type PluginDefs = [ TyCon ]

definitionsModule :: TcPluginM Module
definitionsModule = do
  findResult <- findImportedModule ( mkModuleName "RewritePerfDefs" ) NoPkgQual
  case findResult of
    Found _ res     -> pure res
    FoundMultiple _ -> error $ "RewritePerfPlugin: found multiple modules named 'RewritePerfDefs'."
    _               -> error $ "RewritePerfPlugin: could not find any module named 'RewritePerfDefs'."

lookupDefs :: TcPluginM PluginDefs
lookupDefs = do
  defs <- definitionsModule
  traverse ( \ tyConName -> lookupOrig defs ( mkTcOcc tyConName ) >>= tcLookupTyCon )
    [ "And", "NE", "EQ", "All", "ListConcat", "AppendIf", "Apply"
    , "Map", "MapAppend", "MapAppend2", "MapAppend3"
    , "Iterate2", "Iterate3", "Iterate4"
    , "Orientations", "Compatible", "Allowed"
    , "MatchingOrientations", "AllowedCombinations"
    , "Solutions"
    ]

plugin :: Plugin
plugin =
  defaultPlugin
    { tcPlugin        = \ _args -> Just $ rewritingPlugin
    , pluginRecompile = purePlugin
    }

rewritingPlugin :: TcPlugin
rewritingPlugin =
  TcPlugin
    { tcPluginInit    = lookupDefs
    , tcPluginSolve   = \ _ _ _ _ _ -> pure $ TcPluginOk [] []
    , tcPluginRewrite = rewriter
    , tcPluginStop    = \ _ -> pure ()
    }

rewriter :: PluginDefs -> UniqFM TyCon TcPluginRewriter
rewriter tyCons =
  listToUFM $ map ( , don'tRewrite ) tyCons

don'tRewrite :: RewriteEnv -> [ Ct ] -> [ Type ] -> TcPluginM TcPluginRewriteResult
don'tRewrite _ _ _ = pure TcPluginNoRewrite
