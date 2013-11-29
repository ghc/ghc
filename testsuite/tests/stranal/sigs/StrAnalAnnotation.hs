{-# LANGUAGE DeriveDataTypeable #-}


-- | This module is not used in GHC. Rather, it is a module that
-- can be used to annotate functions with expected result of the demand
-- analyzer, and it will print warnings if they do not match.
-- This is primarily used for the GHC testsuite, but you can use it in your own
-- test suites as well.
module StrAnalAnnotation (plugin, StrAnal(..)) where

import GhcPlugins
import Demand (StrictSig, pprIfaceStrictSig)

import Data.Data
import Control.Monad

-- | Use this to annotate your functions
data StrAnal= StrAnal String deriving (Data, Typeable)

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  return (todo ++ [CoreDoPluginPass "Strictness Analzier result test" pass])

pass :: ModGuts -> CoreM ModGuts
pass g = mapM_ (printAnn g) (allIds (mg_binds g)) >> return g

printAnn :: ModGuts -> Id -> CoreM ()
printAnn guts b = do
    anns <- annotationsOn guts b :: CoreM [StrAnal]
    flags <- getDynFlags
    mapM_ (report flags b) anns

report :: DynFlags -> Id -> StrAnal -> CoreM ()
report flags id (StrAnal ann)
  | sigStr == ann = return ()
  | otherwise = putMsg $
      hang (text "Mismatch in expected strictness signature:") 4 $
          vcat [ text "name:    " <+> ppr id
               , text "expected:" <+> text ann
               , text "found:   " <+> text sigStr
               ]
 where sig = idStrictness id
       sigStr = showSDoc flags (pprIfaceStrictSig (idStrictness id))

allIds :: CoreProgram -> [Id]
allIds = concatMap go
  where go (NonRec i _) = [i]
        go (Rec bs) = map fst bs

annotationsOn :: Data a => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  anns <- getAnnotations deserializeWithData guts
  return $ lookupWithDefaultUFM anns [] (varUnique bndr)
