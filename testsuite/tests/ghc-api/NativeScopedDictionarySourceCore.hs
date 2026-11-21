module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC (TcMPluginHandling(StartAndKeepRunningTcMPlugins), coreModule, depanal,
            desugarModule, getSessionDynFlags, guessTarget, mgModSummaries, parseModule,
            runGhc, setSessionDynFlags, setTargets, typecheckModule)
import GHC.Unit.Module.ModGuts (mg_binds)
import GHC.Core (CoreExpr, Expr(..), Alt(..), bindersOf, collectArgs, flattenBinds, nativeClassSelectorIndex)
import GHC.Core.Lint (lintScopedDictionaryExpr)
import GHC.Core.TyCo.Rep (Type(ClassAppTy, TermVarTy))
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (isNativeClassDictionaryId)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.DynFlags (gopt_unset, dopt_set)
import GHC.Driver.Flags (GeneralFlag(Opt_DoCoreLinting), DumpFlag(Opt_D_ppr_debug))
import GHC.Types.Name (getOccString)
import GHC.Types.Var (Id, varType)
import GHC.Utils.Outputable (ppr, renderWithContext, defaultSDocContext, sdocPprDebug)
import System.Environment (getArgs)

-- | Check the desugared source in memory, using the private Core lint entry.
main :: IO ()
main = do
    lib : input : name : count : callee <- getArgs
    runGhc (Just lib) $ do
        flags <- getSessionDynFlags
        _ <- setSessionDynFlags (dopt_set (gopt_unset flags Opt_DoCoreLinting) Opt_D_ppr_debug)
        target <- guessTarget input Nothing Nothing
        setTargets [target]
        [summary] <- mgModSummaries <$> depanal [] True
        parsed <- parseModule summary
        checked <- typecheckModule StartAndKeepRunningTcMPlugins parsed
        liftIO $ putStrLn "source typechecked"
        core <- desugarModule checked
        liftIO $ putStrLn "source desugared"
        let binds = mg_binds (coreModule core)
            cfg = initLintConfig flags (concatMap bindersOf binds)
        [(binder, rhs)] <- pure [(b, body) | (b, body) <- flattenBinds binds, getOccString b == name]
        liftIO $ do
            case lintScopedDictionaryExpr cfg rhs (varType binder) of
                Nothing -> putStrLn "source Core and its declared type agree"
                Just errors -> fail (renderWithContext (defaultSDocContext { sdocPprDebug = True }) (ppr errors))
            selected <- case callee of
                [] -> do
                    let indexed = [(inst, dict, varType arg) | ClassAppTy _ inst (TermVarTy dict)
                                    (TermVarTy arg) _ <- applications nativeClassSelectorIndex rhs]
                    unless (all (\(inst, _, domain) -> eqType inst domain) indexed) $
                        fail "selector operand and instance types differ"
                    pure [dict | (_, dict, _) <- indexed]
                [calleeName] -> pure (concat (applications (dictionaries calleeName) rhs))
                _ -> fail "pass one optional callee name"
            unless (length selected == read count) $
                fail "unexpected number of selected dictionaries"
            case selected of
                [lhs, rhsDict] | lhs == rhsDict -> fail "the two retained calls selected the same Given"
                _ -> pure ()
            print (length selected)

-- | Find evidence arguments of a call to the named function.
dictionaries :: String -> CoreExpr -> Maybe [Id]
dictionaries callee expr
  | (Var fun, args) <- collectArgs expr
  , getOccString fun == callee
  , let dicts = [dict | Var dict <- args, isNativeClassDictionaryId dict]
  , not (null dicts)
  = Just dicts
  | otherwise = Nothing

-- | Visit saturated application spines and their enclosing expressions.
applications :: (CoreExpr -> Maybe a) -> CoreExpr -> [a]
applications match = go
  where
    go expr | Just result <- match expr = [result]
    go (App f a) = go f ++ go a
    go (Lam _ body) = go body
    go (Let bind body) = concatMap (go . snd) (flattenBinds [bind]) ++ go body
    go (Case scrut _ _ alts) = go scrut ++ concatMap (\(Alt _ _ rhs) -> go rhs) alts
    go (Cast expr _) = go expr
    go (Tick _ expr) = go expr
    go _ = []
