module Main (main) where

import Control.Monad (forM, unless)
import GHC (
    CoreModule (..),
    compileToCoreModule,
    getLogger,
    getSessionDynFlags,
    noLoc,
    parseDynamicFlags,
    runGhc,
    setSessionDynFlags,
 )
import GHC.Builtin.WiredIn.Types (charTyCon)
import GHC.Core (bindersOfBinds, flattenBinds)
import GHC.Core.CharCase (certOwner, certResultTyCon, certRows, certify)
import GHC.Core.Lint (lintExpr)
import GHC.Core.Type (isCharLitTy)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.DynFlags (verbosity)
import GHC.Types.Id (idName)
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [libdir, source, worker, destination] -> generate libdir source worker destination
        _ -> die "usage: generate-upper ABS_LIBDIR SOURCE WORKER UPPER_SOURCE"

-- | Lint the selected binding and emit the equations derived from its Core.
generate :: FilePath -> FilePath -> String -> FilePath -> IO ()
generate libdir source worker destination = do
    (flags, core) <- runGhc (Just libdir) $ do
        flags <- getSessionDynFlags
        logger <- getLogger
        (flags', _, _) <- parseDynamicFlags logger flags [noLoc "-package", noLoc "ghc-internal"]
        setSessionDynFlags flags'{verbosity = 0}
        core <- compileToCoreModule source
        pure (flags', core)
    (owner, rhs) <- case [ (ident, expr)
                         | (ident, expr) <- flattenBinds (cm_binds core)
                         , getOccString ident == worker
                         , nameModule_maybe (idName ident) == Just (cm_module core)
                         ] of
        [binding] -> pure binding
        _ -> die "expected one resolved binding for the selected worker"
    case lintExpr (initLintConfig flags (bindersOfBinds (cm_binds core))) rhs of
        Nothing -> pure ()
        Just problems -> die (showSDocUnsafe (ppr problems))
    certificate <- either (die . show) pure (certify owner rhs)
    unless (certOwner certificate == idName owner) $
        die "certificate belongs to a different producer"
    -- The character certificate requires an identity default.
    unless (certResultTyCon certificate == charTyCon) $
        die "expected a character mapping certificate"
    rows <- forM (certRows certificate) $ \(input, result) ->
        case isCharLitTy result of
            Just output -> pure (input, output)
            Nothing -> die "expected a character literal in the certificate"
    writeFile destination (renderUpper rows)

{- | Render only an already certified decision. Put the default after literals
so the closed family's ordinary apartness check can keep it stuck on a skolem.
-}
renderUpper :: [(Char, Char)] -> String
renderUpper rows =
    unlines $
        [ "{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies #-}"
        , "module Upper (Upper) where"
        , ""
        , "type family Upper (c :: Char) :: Char where"
        ]
            ++ ["  Upper " ++ show input ++ " = " ++ show result | (input, result) <- rows]
            ++ ["  Upper c = c"]
