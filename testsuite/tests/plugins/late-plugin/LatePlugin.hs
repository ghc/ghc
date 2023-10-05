module LatePlugin where

import Data.Bool
import GHC.Core
import GHC.Core.TyCo.Compare
import GHC.Driver.Monad
import GHC.Plugins
import GHC.Types.Avail
import GHC.Types.Var
import GHC.Types.Id
import System.IO

-- | Both a core plugin and a late plugin. The Core plugin edits the binding in
-- the test file (testBinding) to be the integer "111111". The late plugin then
-- edits the binding to be the integer "222222". Then we make sure the "222222"
-- did not make it in the interface file and the "111111" did.
plugin :: Plugin
plugin =
    defaultPlugin
      { installCoreToDos = earlyP
      , latePlugin = lateP
      }

earlyP :: CorePlugin
earlyP _ todos = do
    return
      . (: todos)
      $ CoreDoPluginPass "earlyP"
      $ \mgs -> liftIO $ do
          binds' <- editCoreBinding True (moduleName (mg_module mgs)) (mg_binds mgs)
          return mgs { mg_binds = binds' }

lateP :: LatePlugin
lateP _ opts (cg_guts, cc_state) = do
  binds' <- editCoreBinding False (moduleName (cg_module cg_guts)) (cg_binds cg_guts)
  return (cg_guts { cg_binds = binds' }, cc_state)

editCoreBinding :: Bool -> ModuleName -> CoreProgram -> IO CoreProgram
editCoreBinding early modName pgm = do
    putStrLn $
      bool "late " "early " early ++ "plugin running on module " ++
      moduleNameString modName
    pure $ go pgm
  where
    go :: [CoreBind] -> [CoreBind]
    go (b@(NonRec v e) : bs)
      | occNameString (getOccName v) == "testBinding" && exprType e `eqType` intTy =
          NonRec v (mkUncheckedIntExpr $ bool 222222 111111 early) : bs
    go (b:bs) = b : go bs
    go [] = []
