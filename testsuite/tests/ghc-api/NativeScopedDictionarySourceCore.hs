module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC (TcMPluginHandling(StartAndKeepRunningTcMPlugins), coreModule, depanal,
            desugarModule, getSessionDynFlags, guessTarget, mgModSummaries, parseModule,
            runGhc, setSessionDynFlags, setTargets, typecheckModule)
import GHC.Unit.Module.ModGuts (mg_binds)
import GHC.Core (CoreExpr, Expr(..), Alt(..), bindersOf, flattenBinds, nativeClassSelectorIndex)
import GHC.Core.Lint (lintScopedDictionaryExpr)
import GHC.Core.TyCo.Rep (Type(ClassAppTy, TermVarTy))
import GHC.Core.TyCo.Compare (eqType)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.DynFlags (gopt_unset, dopt_set)
import GHC.Driver.Flags (GeneralFlag(Opt_DoCoreLinting), DumpFlag(Opt_D_ppr_debug))
import GHC.Types.Name (getOccString)
import GHC.Types.Var (varType)
import GHC.Utils.Outputable (ppr, renderWithContext, defaultSDocContext, sdocPprDebug)
import System.Environment (getArgs)

-- | Check the desugared source in memory, using the private Core lint entry.
main :: IO ()
main = do
    [lib, input, name, count] <- getArgs
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
            let selected = [(inst, dict, varType arg) | ClassAppTy _ inst (TermVarTy dict)
                            (TermVarTy arg) _ <- selectors rhs]
            unless (length selected == read count &&
                    all (\(inst, _, domain) -> eqType inst domain) selected) $
                fail "wrong number or domain of selected lexically scoped dictionaries"
            case selected of
                [(_, lhs, _), (_, rhsDict, _)] | lhs == rhsDict ->
                    fail "the two retained calls selected the same Given"
                _ -> pure ()
            print (length selected)

-- | Find direct class selector calls in a Core expression.
selectors :: CoreExpr -> [Type]
selectors expr
  | Just index <- nativeClassSelectorIndex expr = [index]
selectors (App f a) = selectors f ++ selectors a
selectors (Lam _ body) = selectors body
selectors (Let bind body) = concatMap (selectors . snd) (flattenBinds [bind]) ++ selectors body
selectors (Case scrut _ _ alts) = selectors scrut ++ concatMap (\(Alt _ _ rhs) -> selectors rhs) alts
selectors (Cast expr _) = selectors expr
selectors (Tick _ expr) = selectors expr
selectors _ = []
