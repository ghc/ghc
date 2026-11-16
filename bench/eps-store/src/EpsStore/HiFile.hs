-- | A 'DeclStore' that stores nothing: the interface files on disk are the
-- store. Getting a declaration back re-reads the interface file of its module.
-- The declarations of the last few interface files read are kept, as lookups
-- tend to come in bursts for the same module.
module EpsStore.HiFile
  ( newHiFileStore
  ) where

import EpsStore.DeclStore

import GHC.Driver.Env ( HscEnv (..), hsc_dflags )
import GHC.Iface.Load ( readIfaceCopying )
import GHC.Iface.Syntax ( IfaceDecl (..) )
import GHC.Types.Name ( nameOccName )
import GHC.Types.Name.Occurrence ( OccName, OccEnv, mkOccEnv, lookupOccEnv )
import GHC.Unit.Module ( Module, ml_hi_file )
import GHC.Unit.Module.ModIface ( mi_decls )
import GHC.Utils.Outputable ( ppr, showSDocUnsafe )
import GHC.Data.Maybe ( MaybeErr (..) )

import Control.Concurrent.MVar
import Data.IORef
import qualified Data.IntMap.Strict as IM

data HiDecl = HiDecl !Module !FilePath !OccName

-- | Number of interface files whose declarations we keep.
cacheSize :: Int
cacheSize = 16

-- | The 'HscEnv' provides the hooks, flags and name cache used to read the
-- interface files; it should be the session's.
newHiFileStore :: HscEnv -> IO DeclStore
newHiFileStore hsc_env = do
  next  <- newIORef (0 :: Int)
  index <- newIORef (IM.empty :: IM.IntMap HiDecl)
  -- Most recently used first.
  cache <- newMVar ([] :: [(FilePath, OccEnv IfaceDecl)])
  let put mod loc decls = do
        let path = ml_hi_file loc
            n = length decls
        first <- atomicModifyIORef' next (\i -> (i + n, i))
        let keys = [first .. first + n - 1]
            entries = [ (k, HiDecl mod path (nameOccName (ifName d)))
                      | (k, d) <- zip keys decls ]
        mapM_ (\(_, HiDecl _ _ occ) -> occ `seq` pure ()) entries
        atomicModifyIORef' index (\m -> (IM.union m (IM.fromList entries), ()))
        pure keys

      get key = do
        HiDecl mod path occ <- (IM.! key) <$> readIORef index
        decls <- modifyMVar cache $ \c -> case lookup path c of
          Just ds -> pure ((path, ds) : filter ((/= path) . fst) c, ds)
          Nothing -> do
            ds <- read_decls mod path
            pure (take cacheSize ((path, ds) : c), ds)
        case lookupOccEnv decls occ of
          Just d  -> pure d
          Nothing -> fail ("EpsStore.HiFile: " ++ path ++ " does not declare "
                           ++ showSDocUnsafe (ppr occ))

      read_decls mod path = do
        -- Copy the lazily read parts out of the file buffer, so that a
        -- declaration (e.g. its unfolding) does not retain the whole file.
        r <- readIfaceCopying True (hsc_hooks hsc_env) (hsc_logger hsc_env)
               (hsc_dflags hsc_env) (hsc_NC hsc_env) mod path
        case r of
          Succeeded iface -> pure $! mkOccEnv
            [ (nameOccName (ifName d), d) | (_, d) <- mi_decls iface ]
          Failed _ -> fail ("EpsStore.HiFile: can't re-read " ++ path)

  pure DeclStore
    { ds_name  = "hi"
    , ds_put   = put
    , ds_get   = get
    , ds_close = pure ()
    }
