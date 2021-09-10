{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Simple.ReplacePlugin(plugin) where

import GHC.Types.Unique.FM
import GHC.Plugins
import qualified GHC.Utils.Error
import GHC.Types.TyThing

import Debug.Trace
import Data.Bifunctor (second)
import Control.Monad
import qualified Language.Haskell.TH as TH
import Data.List (isSuffixOf)

woz :: Int -> Int
woz x = trace ("Got " ++ show x) x

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install,
    pluginRecompile  = purePlugin
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todos = do
    mb <- thNameToGhcName 'woz
    case mb of
      Nothing -> error "Failed to locate woz"
      Just m  -> do
        rep <- lookupId m
        return $ CoreDoPluginPass "Replace wiz with woz" (fixGuts rep) : todos

fixGuts :: Id -> ModGuts -> CoreM ModGuts
fixGuts rep guts = pure $ guts { mg_binds = fmap fix_bind (mg_binds guts) }
  where
    fix_bind (NonRec b e) = NonRec b (fix_expr e)
    fix_bind (Rec bes)    = Rec (fmap (second fix_expr) bes)

    fix_expr :: CoreExpr -> CoreExpr
    fix_expr = \case
      Var i         -> if "$wiz" `isSuffixOf` nameStableString (idName i)
                        then Var rep
                        else Var i
      Lit l         -> Lit l
      App e1 e2     -> App (fix_expr e1) (fix_expr e2)
      Lam b e       -> Lam b (fix_expr e)
      Case e b t as -> Case (fix_expr e) b t (map fix_alt as)
      Cast e c      -> Cast (fix_expr e) c
      Tick t e      -> Tick t (fix_expr e)
      Type t        -> Type t
      Coercion c    -> Coercion c

    fix_alt (Alt c bs e) = Alt c bs (fix_expr e)
