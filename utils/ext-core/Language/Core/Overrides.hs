{-# OPTIONS -Wall #-}
{- 
   This module encapsulates some magic regarding overridden modules.

   In the interpreter, we use "overridden" versions of certain
   standard GHC library modules in order to avoid implementing
   more primitives than we need to implement to run simple programs.
   So, after typechecking but before interpretation, references to overridden
   modules are resolved to references to modules in our simplified
   version of the standard library.

   It's kind of ugly.
-}
module Language.Core.Overrides (override) where

import Language.Core.Core
import Language.Core.Encoding
import Language.Core.ParsecParser

import Data.Generics
import System.FilePath

override :: [Module] -> IO [Module]
override = mapM overrideOne
  where overrideOne :: Module -> IO Module
        overrideOne (Module mn _ _) | mn `elem` wiredInModules =
           findWiredInModule mn >>= (return . snd)
        overrideOne m = return m

-- This function encapsulates all the business with overriden modules.
-- The story is that if an "overridden" module exists for the given
-- module, then we parse it in and rewrite all occurrences of the "base-extcore"
-- package name inside it to "base". We have to do this b/c when compiling
-- the overridden modules, we gave the package name "base-extcore", because
-- GHC gets unhappy if we try to make it part of the "base" package.
-- Was that clear? (No.)
findWiredInModule :: AnMname -> IO (FilePath, Module)
findWiredInModule m@(M (pn, encHier, encLeafName)) =
   findModuleIO (Just munged) (wiredInFileName m)
     where hier = map zDecodeString encHier
           leafName = zDecodeString encLeafName
           munged = 
             M (pn, map (\ p -> if p == "GHC_ExtCore" then "GHC" else p) hier,
                 leafName)


wiredInModules :: [AnMname]
wiredInModules =
  map (\ m -> (mkBaseMname m)) ["Handle", "IO", "Unicode"]

wiredInFileName :: AnMname -> FilePath
wiredInFileName (M (_,_,leafName)) =
  "./lib/GHC_ExtCore/" </> leafName `addExtension` "hcr"


mungePackageName :: Module -> Module
-- for now: just substitute "base-extcore" for "base"
-- and "GHC" for "GHC_ExtCore" in every module name
mungePackageName m@(Module _ _ _) = everywhere (mkT mungeMname)
    (everywhere (mkT mungePname) 
       (everywhere (mkT mungeVarName) m))
  where mungePname (P s) | s == zEncodeString overriddenPname =
           (P "base")
        mungePname p = p
{- TODO: Commented out because this code should eventually
   be completely rewritten. No time to do it now.
        -- rewrite uses of fake primops
        mungeVarName (Var (Just mn', v))
          | mn' == mn && v `elem` (fst (unzip newPrimVars)) =
            Var (Just primMname, v)
-}
        mungeVarName :: Exp -> Exp
        mungeVarName e = e

mungeMname :: AnMname -> AnMname
mungeMname (M (pname, (hd:rest), leaf)) 
  | zDecodeString hd == "GHC_ExtCore" =
          (M (pname, ("GHC":rest), leaf))
mungeMname mn = mn

overriddenPname :: String
overriddenPname = "base-extcore"


findModuleIO :: Mname -> FilePath -> IO (FilePath, Module)
findModuleIO trueName fp = do
   res <- parseCore fp
   case res of
     Left _   -> error ("findModule: error parsing dependency " ++ fp)
     Right parsedMod -> do
              let resultMod@(Module _ _ _) = 
                      case trueName of
                        Just _ -> mungePackageName parsedMod
                        Nothing -> parsedMod
              return (fp, resultMod)

