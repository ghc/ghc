{-# LANGUAGE TemplateHaskell #-}

module Simple.Plugin(plugin) where

import UniqFM
import GhcPlugins
import qualified ErrUtils

-- For annotation tests
import Simple.DataStructures

import Control.Monad
import Data.Monoid
import Data.Dynamic
import qualified Language.Haskell.TH as TH

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todos = do
    putMsgS $ "Simple Plugin Passes Queried"
    putMsgS $ "Got options: " ++ unwords options
    
    -- Create some actual passes to continue the test.
    return $ CoreDoPluginPass "Main pass" mainPass
             : todos

findNameBinds :: String -> [CoreBind] -> First Name
findNameBinds target = mconcat . map (findNameBind target)

findNameBind :: String -> CoreBind -> First Name
findNameBind target (NonRec b e) = findNameBndr target b
findNameBind target (Rec bes) = mconcat (map (findNameBndr target . fst) bes)

findNameBndr :: String -> CoreBndr -> First Name
findNameBndr target b 
  = if getOccString (varName b) == target
    then First (Just (varName b))
    else First Nothing


mainPass :: ModGuts -> CoreM ModGuts
mainPass guts = do
    putMsgS "Simple Plugin Pass Run"
    anns <- getAnnotations deserializeWithData guts
    bindsOnlyPass (mapM (changeBind anns Nothing)) guts

changeBind :: UniqFM [ReplaceWith] -> Maybe String -> CoreBind -> CoreM CoreBind
changeBind anns mb_replacement (NonRec b e) = changeBindPr anns mb_replacement b e >>= (return . uncurry NonRec)
changeBind anns mb_replacement (Rec bes) = liftM Rec $ mapM (uncurry (changeBindPr anns mb_replacement)) bes

changeBindPr :: UniqFM [ReplaceWith] -> Maybe String -> CoreBndr -> CoreExpr -> CoreM (CoreBndr, CoreExpr)
changeBindPr anns mb_replacement b e = do
    case lookupWithDefaultUFM anns [] b of
        [] -> do
                e' <- changeExpr anns mb_replacement e
                return (b, e')
        [ReplaceWith replace_string] -> do
                e' <- changeExpr anns (Just replace_string) e
                return (b, e')
        _ -> do dflags <- getDynFlags
                error ("Too many change_anns on one binder:" ++ showPpr dflags b)

changeExpr :: UniqFM [ReplaceWith] -> Maybe String -> CoreExpr -> CoreM CoreExpr
changeExpr anns mb_replacement e = let go = changeExpr anns mb_replacement in case e of
        Lit (MachStr _) -> case mb_replacement of
                Nothing -> return e
                Just replacement -> do
                        putMsgS "Performing Replacement"
                        return $ Lit (MachStr (fastStringToByteString (mkFastString replacement)))
        App e1 e2 -> liftM2 App (go e1) (go e2)
        Lam b e -> liftM (Lam b) (go e)
        Let bind e -> liftM2 Let (changeBind anns mb_replacement bind) (go e)
        Case e b ty alts -> liftM4 Case (go e) (return b) (return ty) (mapM (changeAlt anns mb_replacement) alts)
        Cast e coerce -> liftM2 Cast (go e) (return coerce)
        Tick t e -> liftM (Tick t) (go e)
        _ -> return e

changeAlt :: UniqFM [ReplaceWith] -> Maybe String -> CoreAlt -> CoreM CoreAlt
changeAlt anns mb_replacement (con, bs, e) = liftM (\e' -> (con, bs, e')) (changeExpr anns mb_replacement e)
