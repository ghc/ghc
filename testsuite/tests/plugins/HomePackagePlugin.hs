module HomePackagePlugin where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin {
        installCoreToDos = install
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _options todos = do
    return $ CoreDoPluginPass "String replacement" corePass : todos
  where
    corePass guts = do
      binds' <- stringReplacementPass (mg_binds guts)
      pure guts { mg_binds = binds' }

stringReplacementPass :: CoreProgram -> CoreM CoreProgram
stringReplacementPass binds = return $ map replaceInCompUnit binds

replaceInCompUnit :: CoreCompUnit -> CoreCompUnit
replaceInCompUnit (CoreCompUnit binds rules)
  = CoreCompUnit (map replaceInBind binds) rules

replaceInBind :: CoreBind -> CoreBind
replaceInBind (NonRec b e) = NonRec b (replaceInExpr e)
replaceInBind (Rec bes) = Rec [(b, replaceInExpr e) | (b, e) <- bes]

replaceInExpr :: CoreExpr -> CoreExpr
replaceInExpr (Var x) = Var x
replaceInExpr (Lit (LitString _)) = mkStringLit "Hello From The Plugin" -- The payload
replaceInExpr (Lit l) = Lit l
replaceInExpr (Lam b e) = Lam b (replaceInExpr e)
replaceInExpr (App e1 e2) = App (replaceInExpr e1) (replaceInExpr e2)
replaceInExpr (Let bi e) = Let (replaceInBind bi) (replaceInExpr e)
replaceInExpr (Tick t e) = Tick t (replaceInExpr e)
replaceInExpr (Cast e co) = Cast (replaceInExpr e) co
replaceInExpr (Case e b ty alts) = Case (replaceInExpr e) b ty (map replaceInAlt alts)
replaceInExpr (Type ty) = Type ty

replaceInAlt :: CoreAlt -> CoreAlt
replaceInAlt (Alt ac bs e) = Alt ac bs (replaceInExpr e)
