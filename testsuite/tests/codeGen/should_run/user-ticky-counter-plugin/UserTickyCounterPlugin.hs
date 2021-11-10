{-# LANGUAGE TupleSections #-}

module UserTickyCounterPlugin (plugin) where

import GHC.Plugins
import GHC.Types.Tickish
import Prelude hiding ((<>))

plugin :: Plugin
plugin =
    defaultPlugin { installCoreToDos = \_args todos -> return $ todos ++ [corePlugin] }

corePlugin :: CoreToDo
corePlugin = CoreDoPluginPass "add tickers" (bindsOnlyPass addTickers)

addTickers :: CoreProgram -> CoreM CoreProgram
addTickers binds = mapM addTicker binds

addTicker :: CoreBind -> CoreM CoreBind
addTicker (NonRec bndr rhs) = NonRec bndr <$> addTick bndr rhs
addTicker (Rec bs)          = Rec <$> mapM (\(bndr, rhs) -> (bndr,) <$> addTick bndr rhs) bs

addTick :: Id -> CoreExpr -> CoreM CoreExpr
addTick bndr rhs = do
    u <- getUniqueM
    dflags <- getDynFlags
    let tickish = TickyCounter $ showSDoc dflags $ text "hi" <> ppr bndr <> ppr u
    return $ Tick tickish rhs
