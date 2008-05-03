{-# OPTIONS -Wall #-}
{- 
   Besides computing dependencies between External Core modules,
   this module encapsulates some magic regarding overridden modules.

   In the interpreter, we use "overridden" versions of certain
   standard GHC library modules in order to avoid implementing
   more primitives than we need to implement to run simple programs.
   So, during the dependency-finding process (which, because the
   dependency-finder maintains a module cache to make sure no 
   module is loaded/parsed more than once), references to overridden
   modules are resolved to references to modules in our simplified
   version of the standard library.

   It's kind of ugly.
-}
module Dependencies(getDependencies) where

import Core
import Encoding
import ParsecParser
import Prims

import Control.Monad.State
import Data.Generics
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO

type DepM a = StateT (FilePath, -- "main" module file path
                 -- maps module names onto dependencies
                 M.Map (Either AnMname FilePath) [AnMname],
                 -- module cache
                 M.Map (Either AnMname FilePath) (FilePath, Module)) IO a

-- Given a module, return all the modules it
-- depends on (directly or indirectly).
getDependencies :: [FilePath] -> IO [(FilePath, Module)]
getDependencies ms =
  evalStateT (do
    (mapM_ (\ f -> do
              liftIO $ putStrLn $ "==== Finding deps for " ++ show f ++ "====="
              -- Every module depends on itself anyway,
              -- so we ignore the FilePath deps.
              ds <- go getDeps lefts (map Left) (map Right ms)
              return (f, ds)) ms)
    (_,t,_) <- get
    let modNames = nub $ concat (snd (unzip (leftsPairs (M.toList t))))
    (liftM catMaybes) $ mapM findModuleP (map Left modNames))
   (last ms, M.empty, M.empty)

go :: (Show a, Show b, Eq b, MonadIO m) =>
  (a -> m [b]) -> ([a] -> [b]) -> ([b] -> [a]) -> [a] -> m [b]
go getMore p fixUp start = do
  next <- concatMapM getMore start
  let more = nub $ (p start) ++ next
  if (length start == length more)
    then return more
    else go getMore p fixUp (fixUp more)

varRef :: Exp -> [AnMname]
varRef (Var v) | Just m' <- getModule v = [m']
varRef (Dcon dc) | Just m' <- getModule dc = [m']
varRef _ = []

tyRef :: Ty -> [AnMname]
tyRef (Tcon tc) | Just m' <- getModule tc = [m']
tyRef  _ = []


getDeps :: Either AnMname FilePath -> DepM [AnMname]
getDeps mn = do
          (a,t,b) <- get
          case M.lookup mn t of
            Just ds -> return ds
            Nothing -> do
              maybeM <- findModule mn
              case maybeM of
                Nothing -> return []
                Just m@(Module mname _ _) -> do
                  let ds =   (everything union ([] `mkQ` varRef) m)
                            `union` (everything union ([] `mkQ` tyRef) m) in do
                  put (a, M.insert mn ds t, b)
                  -- in case we were given a filepath, register the
                  -- module name too
                  put (a, M.insert (Left mname) ds t, b)
                  return ds

findModule :: Either AnMname FilePath -> DepM (Maybe Module)
findModule x = do
 maybeRes <- findModuleP x
 case maybeRes of
   Just (_,m) -> return $ Just m
   _          -> return Nothing

findModuleP :: Either AnMname FilePath -> DepM (Maybe (FilePath, Module))
findModuleP (Left mn) | mn `elem` wiredInModules = 
  findWiredInModule mn >>= (return . Just)
findModuleP (Left mn) | mn == wrapperMainMname || mn == mainMname = do
  (f,_,_) <- get
  findModuleP (Right f)
findModuleP (Left mn) | mn == primMname = return Nothing
  -- Nothing means that this module is valid; it just doesn't have
  -- an implementation
findModuleP m = tryFindModule m

tryFindModule :: Either AnMname FilePath -> DepM (Maybe (FilePath, Module))
tryFindModule k = do
  (_,_,mCache) <- get
  liftM Just $ case M.lookup k mCache of
    Just p -> return p
    Nothing -> findModuleNotCached k

-- This function encapsulates all the business with overriden modules.
-- The story is that if an "overridden" module exists for the given
-- module, then we parse it in and rewrite all occurrences of the "base-extcore"
-- package name inside it to "base". We have to do this b/c when compiling
-- the overridden modules, we gave the package name "base-extcore", because
-- GHC gets unhappy if we try to make it part of the "base" package.
-- Was that clear? (No.)
findModuleNotCached :: Either AnMname FilePath -> DepM (FilePath, Module)
findModuleNotCached (Left m@(M (P pkgName, encHier, encLeafName))) = do
      let hier = map zDecodeString encHier
          leafName = zDecodeString encLeafName
          possibleFiles = (map (dirs hier leafName) searchPath)
                     ++ map (dirs (zDecodeString pkgName:hier) leafName) searchPath in do
      match <- liftIO $ findM doesFileExist possibleFiles
      case match of
         Just fp -> findModule' Nothing fp
         Nothing -> error ("findModule: failed to find dependency " ++ show m
                      ++ " tried " ++ show possibleFiles)
findModuleNotCached (Right fp) = findModule' Nothing fp

dirs :: [String] -> String -> FilePath -> FilePath
dirs modulePath leafName dir = dir </> 
                 (foldr (</>) (addExtension leafName "hcr") modulePath)

findWiredInModule :: AnMname -> DepM (FilePath, Module)
findWiredInModule m@(M (pn, encHier, encLeafName)) =
   findModule' (Just munged) (wiredInFileName m)
     where hier = map zDecodeString encHier
           leafName = zDecodeString encLeafName
           munged = 
             M (pn, map (\ p -> if p == "GHC_ExtCore" then "GHC" else p) hier,
                 leafName)

findModule' :: Mname -> FilePath -> DepM (FilePath, Module)
findModule' trueName fp = do
          res <- liftIO $ parseCore fp
          case res of
            Left _   -> error ("findModule: error parsing dependency " ++ fp)
            Right parsedMod -> do
                let resultMod@(Module mn _ _) = 
                      case trueName of
                        Just _ -> mungePackageName parsedMod
                        Nothing -> parsedMod
                cacheModule mn fp resultMod
                return (fp, resultMod)

cacheModule :: AnMname -> FilePath -> Module -> DepM ()
cacheModule mn fp m = modify (\ (a, b, cache) ->
                           (a, b, M.insert (Left mn) (fp, m)
                                    (M.insert (Right fp) (fp, m)
                                       cache)))

searchPath :: [FilePath]
searchPath = overriddenDir:["../../libraries/",
              "../../libraries/integer-gmp/"]

overriddenDir :: FilePath
overriddenDir = "./lib/"

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = liftM listToMaybe . filterM p

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = (liftM concat) . (mapM f)

lefts :: [Either a b] -> [a]
lefts = foldr lefts' []
  where lefts' (Left a) xs = a:xs
        lefts' _        xs = xs

leftsPairs :: [(Either a b, c)] -> [(a, c)]
leftsPairs = foldr leftsPairs' []
  where leftsPairs' ((Left x), y) xs = (x, y):xs
        leftsPairs' _             xs = xs

mungePackageName :: Module -> Module
-- for now: just substitute "base-extcore" for "base"
-- and "GHC" for "GHC_ExtCore" in every module name
mungePackageName m@(Module mn _ _) = everywhere (mkT mungeMname)
    (everywhere (mkT mungePname) 
       (everywhere (mkT mungeVarName) m))
  where mungePname (P s) | s == zEncodeString overriddenPname =
           (P "base")
        mungePname p = p
        -- rewrite uses of fake primops
        mungeVarName (Var (Just mn', v))
          | mn' == mn && v `elem` (fst (unzip newPrimVars)) =
            Var (Just primMname, v)
        mungeVarName e = e

mungeMname :: AnMname -> AnMname
mungeMname (M (pname, (hd:rest), leaf)) 
  | zDecodeString hd == "GHC_ExtCore" =
          (M (pname, ("GHC":rest), leaf))
mungeMname mn = mn

overriddenPname :: String
overriddenPname = "base-extcore"

wiredInModules :: [AnMname]
wiredInModules =
  map (\ m -> (mkBaseMname m)) ["Handle", "IO", "Unicode"]

wiredInFileName :: AnMname -> FilePath
wiredInFileName (M (_,_,leafName)) =
  "./lib/GHC_ExtCore/" </> leafName `addExtension` "hcr"
