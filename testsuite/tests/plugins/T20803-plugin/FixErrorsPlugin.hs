{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}

module FixErrorsPlugin where

import GHC.Plugins
import GHC.Types.Error
import GHC.Hs
import GHC.Data.Bag
import GHC.Parser.Errors.Types

import System.IO
import Data.Type.Equality as Eq
import Data.Data
import Data.Maybe

-- Tests whether it's possible to remove a parse error and fix the erroneous AST
plugin :: Plugin
plugin = defaultPlugin {parsedResultAction = parsedAction}

-- Replace every hole (and other unbound vars) with the given expression
replaceHoles :: forall a . Data a => HsExpr GhcPs -> a -> a
replaceHoles new = gmapT \case
  (d :: d) -> replaceHoles new d `fromMaybe` tryHole
    where
      tryHole :: Maybe d
      tryHole = eqT @d @(HsExpr GhcPs) >>= \case
        Eq.Refl | HsUnboundVar _ _ <- d -> Just new
        _                               -> Nothing

parsedAction :: [CommandLineOption] -> ModSummary -> HsParsedModule
             -> (Messages PsWarning, Messages PsError)
             -> Hsc (HsParsedModule, (Messages PsWarning, Messages PsError))
parsedAction _ _ (HsParsedModule lmod srcFiles) (warns, errs) = do
  liftIO $ putStrLn "parsePlugin"
  liftIO $ putStrLn $ showPprUnsafe newModule
  -- TODO: Remove #20791
  liftIO $ hFlush stdout
  pure (HsParsedModule newModule srcFiles, (warns, otherErrs))

  where
    PsErrBangPatWithoutSpace (L _ holeExpr) = errMsgDiagnostic noSpaceBang
    (bagToList -> [noSpaceBang], mkMessages -> otherErrs) =
      partitionBag (isNoSpaceBang . errMsgDiagnostic) . getMessages $ errs

    isNoSpaceBang (PsErrBangPatWithoutSpace _) = True
    isNoSpaceBang _ = False

    newModule = replaceHoles holeExpr <$> lmod
