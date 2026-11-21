{-# LANGUAGE TemplateHaskellQuotes #-}

module LocatePlugin (plugin) where

import BoundaryEvidence (Locate, here, there)
import Data.Maybe (mapMaybe)
import GHC.Builtin.WiredIn.Types (promotedConsDataCon)
import GHC.Core (CoreExpr, Expr (Type, Var))
import GHC.Core.Class (Class)
import GHC.Core.DataCon (classDataCon)
import GHC.Core.Make (mkCoreApps, mkCoreConApps)
import GHC.Core.Type (Type, isNumLitTy, splitTyConApp_maybe)
import GHC.Plugins (Plugin (..), defaultPlugin, impurePlugin, purePlugin)
import GHC.Tc.Plugin (TcPluginM, lookupTHName, tcLookupClass, tcLookupId, tcPluginIO)
import GHC.Tc.Types (TcPlugin (..), TcPluginSolveResult (..))
import GHC.Tc.Types.Constraint (Ct (CDictCan), DictCt (..))
import GHC.Tc.Types.Evidence (EvTerm (EvExpr))
import GHC.Types.Id (Id)
import GHC.Types.Unique.FM (emptyUFM)
import Numeric.Natural (Natural)

-- | The exact class and the two Haskell proof constructors used by the solver.
data Definitions = Definitions Class Id Id

-- | Search the known prefix of a promoted list for a natural-number label.
plugin :: Plugin
plugin =
    defaultPlugin
        { tcPlugin = Just . solver
        , pluginRecompile = \options ->
            if null options then purePlugin options else impurePlugin options
        }

-- | Resolve names once, then answer requests with typed membership paths.
solver :: [String] -> TcPlugin
solver options =
    TcPlugin
        { tcPluginInit = definitions
        , tcPluginSolve = \defs _ _ wanteds -> do
            let solved = mapMaybe (solve defs) wanteds
            case (options, solved) of
                ([file], [(position, _)]) -> tcPluginIO (writeFile file (show position ++ "\n"))
                _ -> pure ()
            pure (TcPluginOk (map snd solved) [])
        , tcPluginRewrite = const emptyUFM
        , tcPluginPostTc = const (pure ())
        , tcPluginShutdown = const (pure ())
        }

-- | Use the original names from the witness library.
definitions :: TcPluginM Definitions
definitions = do
    Just cls <- lookupTHName ''Locate
    Just atHead <- lookupTHName 'here
    Just inTail <- lookupTHName 'there
    Definitions <$> tcLookupClass cls <*> tcLookupId atHead <*> tcLookupId inTail

-- | Build the dictionary from a path with the requested label and list type.
solve :: Definitions -> Ct -> Maybe (Natural, (EvTerm, Ct))
solve (Definitions cls atHead inTail) ct@(CDictCan dict)
    | di_cls dict == cls
    , [label, labels] <- di_tys dict
    , Just key <- isNumLitTy label = do
        (position, path) <- search key label labels
        let dictionary = mkCoreConApps (classDataCon cls) [Type label, Type labels, path]
        pure (position, (EvExpr dictionary, ct))
  where
    -- See Note [Evidence for finite membership].
    search :: Integer -> Type -> Type -> Maybe (Natural, CoreExpr)
    search key label labels
        | Just (con, [_, first, rest]) <- splitTyConApp_maybe labels
        , con == promotedConsDataCon
        , Just n <- isNumLitTy first =
            if n == key
                then Just (0, mkCoreApps (Var atHead) [Type label, Type rest])
                else do
                    (position, path) <- search key label rest
                    pure (position + 1, mkCoreApps (Var inTail) [Type label, Type first, Type rest, path])
        | otherwise = Nothing
solve _ _ = Nothing

{- Note [Evidence for finite membership]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At a matching literal, here constructs a path to the first cell. Each
preceding cell adds one there. An unknown suffix after the match need
not be inspected. An empty list, an unknown cell before a match, or an
unknown target leaves the wanted unsolved. The dictionary method returns
the constructed path. It contains no assumption about other calls to the
producer or about the termination of the producer.
-}
