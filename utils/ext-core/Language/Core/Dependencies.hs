module Language.Core.Dependencies(getDependencies) where

import Language.Core.Core
import Language.Core.Encoding
import Language.Core.ParsecParser

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
    (_,t,c) <- get
    let modNames = nub $ concat (snd (unzip (leftsPairs (M.toList t))))
                       
    res1 <- (liftM catMaybes) $ mapM findModuleP (map Left modNames)
    return $ res1 `unionByFirst`
               (snd (unzip (M.toList c))))
   (last ms, M.empty, M.empty)
      where unionByFirst = unionBy (\ (f,_) (g,_) -> f == g)

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
          (_,t,_) <- get
          case M.lookup mn t of
            Just ds -> return ds
            Nothing -> do
              maybeM <- findModule mn
              case maybeM of
                Nothing -> return []
                Just m@(Module mname _ _) -> do
                  let ds = (everything union ([] `mkQ` varRef) m)
                            `union` (everything union ([] `mkQ` tyRef) m) in do
                  liftIO $ putStrLn (show mn ++ " : " ++ show ds)
                  (a1,t1,b1) <- get
                  -- in case we were given a filepath, register the
                  -- module name too
                  put (a1, M.insert mn ds (M.insert (Left mname) ds t1), b1)
                  return ds

findModule :: Either AnMname FilePath -> DepM (Maybe Module)
findModule x = do
 maybeRes <- findModuleP x
 case maybeRes of
   Just (_,m) -> return $ Just m
   _          -> return Nothing

findModuleP :: Either AnMname FilePath -> DepM (Maybe (FilePath, Module))
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

findModuleNotCached :: Either AnMname FilePath -> DepM (FilePath, Module)
findModuleNotCached (Left m@(M (P pkgName, encHier, encLeafName))) = do
      let hier = map zDecodeString encHier
          leafName = zDecodeString encLeafName
          possibleFiles = (map (dirs hier leafName) searchPath)
                     ++ map (dirs (zDecodeString pkgName:hier) leafName) searchPath in do
      match <- liftIO $ findM doesFileExist possibleFiles
      case match of
         Just fp -> findModule' fp
         Nothing -> error ("findModule: failed to find dependency " ++ show m
                      ++ " tried " ++ show possibleFiles)
findModuleNotCached (Right fp) = findModule' fp

dirs :: [String] -> String -> FilePath -> FilePath
dirs modulePath leafName dir = dir </> 
                 (foldr (</>) (addExtension leafName "hcr") modulePath)

findModule' :: FilePath -> DepM (FilePath, Module)
findModule' fp = do
          res <- liftIO $ parseCore fp
          case res of
            Left _   -> error ("findModule: error parsing dependency " ++ fp)
            Right parsedMod@(Module mn _ _) -> do
                cacheModule mn fp parsedMod
                return (fp, parsedMod)

cacheModule :: AnMname -> FilePath -> Module -> DepM ()
cacheModule mn fp m = modify (\ (a, b, cache) ->
                           (a, b, M.insert (Left mn) (fp, m)
                                    (M.insert (Right fp) (fp, m)
                                    cache)))

searchPath :: [FilePath]
searchPath = overriddenDir:["../../libraries/",
             -- kludgy: we wouldn't need these if we parsed the
             -- package.conf file, but for now, we are too lazy
              "../../libraries/integer-gmp/",
              "../../libraries/array/"]

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

{-
rightsPairs :: [(Either a b, c)] -> [(b, c)]
rightsPairs = foldr rightsPairs' []
  where rightsPairs' ((Right x), y) xs = (x, y):xs
        rightsPairs' _             xs = xs
-}