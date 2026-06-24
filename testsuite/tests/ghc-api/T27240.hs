
-- This test checks that bindings are preserved when configuring the occurrence
-- analyzer and the simple optimizer to not drop dead bindings with names
-- selected by a predicate.
--
-- This feature is important for the LiquidHaskell plugin, which relies on the
-- simple optimizer to make core programs easier to read, but needs to preserve
-- bindings that are relevant for verification.
--
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/27240 for the full discussion.
--

import           Control.Monad
import           Data.List (find)
import           Data.Time (getCurrentTime)
import GHC
import GHC.Core
import GHC.Core.SimpleOpt
import GHC.Data.StringBuffer
import GHC.Driver.Config
import GHC.Driver.DynFlags
import GHC.Driver.Env.Types
import GHC.Types.Name
import GHC.Unit.Module.ModGuts
import GHC.Unit.Types
import GHC.Utils.Error
import GHC.Utils.Outputable

import System.Environment (getArgs)


main :: IO ()
main =
  testLocalBindingsDesugaring

testLocalBindingsDesugaring :: IO ()
testLocalBindingsDesugaring = do
    let inputSource = unlines
          [ "module LocalDeadBindingsDesugaring where"
          , "f :: ()"
          , "f = ()"
          , "  where"
          , "    z = ()"
          ]

        isExpectedDesugaring p = case findExpr "f" p of
          Just (Let (NonRec b _) _)
            -> isIdNamed "z" b
          _ -> False

        isIdNamed name v = occNameString (occName v) == name

    coreProgram <-
       compileToCore
         (not . isIdNamed "z")
         "LocalDeadBindingsDesugaring"
         inputSource
    unless (isExpectedDesugaring coreProgram) $
      fail $ unlines $
        "Unexpected desugaring: No local binding for `z` found in the Core program."
        : map showPprQualified coreProgram

-- | Find the Core expression bound to the given name.
findExpr :: String -> CoreProgram -> Maybe CoreExpr
findExpr _ [] =
  Nothing
findExpr name (p:ps) = case p of
  NonRec b e
    | occNameString (occName b) == name
    -> Just e
  Rec binds
    | Just (_, e) <- find (\(b, _e) -> occNameString (occName b) == name) binds
    -> Just e
  _ -> findExpr name ps

showPprQualified :: Outputable a => a -> String
showPprQualified = showSDocQualified . ppr

showSDocQualified :: SDoc -> String
showSDocQualified = renderWithContext ctx
  where
    ctx = defaultSDocContext { sdocStyle = cmdlineParserStyle }



compileToCore :: (Id -> Bool) -> String -> String -> IO [CoreBind]
compileToCore canDrop modName inputSource = do
    [libdir] <- getArgs
    now <- getCurrentTime
    runGhc (Just libdir) $ do
      df1 <- getSessionDynFlags
      GHC.setSessionDynFlags $ df1 { GHC.backend = GHC.bytecodeBackend }
      let target = Target {
                   targetId           = TargetFile (modName ++ ".hs") Nothing
                 , targetUnitId       = homeUnitId_ df1
                 , targetAllowObjCode = False
                 , targetContents     = Just (stringToStringBuffer inputSource, now)
                 }
      setTargets [target]
      void $ GHC.depanal [] False

      dsMod <- getModSummary
                 (mkModule mainUnit (mkModuleName modName))
             >>= parseModule
             >>= typecheckModule NoTcMPlugins
             >>= desugarModule
      hsc_env <- getSession
      return $ mg_binds $ simpleOptimize canDrop hsc_env $ dm_core_module dsMod

-- Run the simple optimizer
simpleOptimize :: (Id -> Bool) -> GHC.HscEnv -> ModGuts -> ModGuts
simpleOptimize canDrop hsc_env guts@(ModGuts
                               { mg_module  = mgmod
                               , mg_binds   = binds
                               , mg_rules   = rules
                               }) =
    let dflags = hsc_dflags hsc_env
        simpl_opts = (initSimpleOpts dflags)
          { so_inline = canDrop
          , so_can_drop = canDrop
          }
        (binds2, rules2, _occ_anald_binds) =
          simpleOptPgm simpl_opts mgmod binds rules
      in guts
          { mg_binds = binds2
          , mg_rules = rules2
          }
