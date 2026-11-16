{-# LANGUAGE BangPatterns, LambdaCase #-}

-- | A low-memory External Package State store.
--
-- It keeps the declarations of loaded modules in a 'DeclStore' (outside the
-- Haskell heap) instead of keeping a lazily typechecked 'TyThing' for every
-- name of every loaded module. When GHC looks up a name, the store fetches the
-- declaration, typechecks it again ('epm_rehydrate') and caches the result
-- only as long as someone else references it (weak pointers).
--
-- Everything else (the slim 'ExternalIface's, instance/rule/... pools) is
-- delegated to GHC's in-memory store, see
-- Note [Abstract External Package State] in GHC.Unit.External.Store.
module EpsStore.LowMem
  ( newLowMemEps
  , LowMemStats (..)
  ) where

import EpsStore.DeclStore

import GHC.Core.ConLike ( ConLike (..) )
import GHC.Iface.Syntax ( IfaceDecl (..), ifaceDeclImplicitBndrs )
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.TyThing ( TyThing (..) )
import GHC.Unit.External.InMemory ( newInMemoryEps )
import GHC.Unit.External.Iface ( setExternalIfaceHashFn )
import GHC.Unit.External.Store
import GHC.Unit.Module ( Module, ModuleEnv, emptyModuleEnv, extendModuleEnv
                       , lookupModuleEnv, elemModuleEnv )
import GHC.Iface.Recomp.Binary ( computeFingerprint )
import GHC.Utils.Fingerprint ( Fingerprint )
import GHC.Utils.Panic ( panic )

import Control.Concurrent.MVar
import Control.Exception ( evaluate )
import Control.Monad
import Data.Array
import Data.IORef
import Data.Maybe ( isJust )
import System.IO.Unsafe ( unsafePerformIO )
import System.Mem.Weak

-- | What we keep in memory for a loaded module.
data ModEntry = ModEntry
  { me_occs      :: !(OccEnv Int)
      -- ^ Every 'OccName' the module declares, including implicit things
      -- (data constructors, selectors, ...), to the index of its declaration.
  , me_decls     :: !(Array Int DeclInfo)
  , me_rehydrate :: !(IfaceDecl -> IO [(Name, TyThing)])
      -- ^ Strict: a selector thunk would retain the 'EpsModule'.
  }

data DeclInfo = DeclInfo
  { di_key  :: !DeclKey
  , di_occ  :: !OccName      -- ^ of the main binder of the declaration
  , di_hash :: !Fingerprint
  }

data CacheEntry
  = Pending TyThing
    -- ^ Rehydrated but not yet forced: we can't make a weak pointer to it
    -- yet. Becomes 'Self' when it is forced, see 'promote'.
  | Self !(Weak TyThing)
    -- ^ Forced; kept as long as its inner object ('TyCon', 'Id', ...) is
    -- referenced from elsewhere.
  | Via !(Weak TyThing)
    -- ^ An implicit thing (or parent) of a thing that was forced: the
    -- (possibly unforced) 'TyThing' is kept as long as that thing is alive.

data LowMemStats = LowMemStats
  { lms_modules       :: !Int
  , lms_decls         :: !Int   -- ^ declarations put into the store
  , lms_lookups       :: !Int
  , lms_cache_hits    :: !Int
  , lms_rehydrations  :: !Int   -- ^ declarations fetched and typechecked again
  }

data LowMem = LowMem
  { lm_inner  :: !ExternalPackageState
  , lm_store  :: !DeclStore
  , lm_lock   :: !(MVar ())
  , lm_mods   :: !(IORef (ModuleEnv ModEntry))
  , lm_cache  :: !(IORef (NameEnv CacheEntry))
  , lm_stats  :: !(IORef LowMemStats)
  }

-- | A fresh low-memory EPS store, keeping declarations in the given
-- 'DeclStore'. Also returns an action reading its statistics.
newLowMemEps :: DeclStore -> IO (ExternalPackageState, IO LowMemStats)
newLowMemEps store = do
  inner <- newInMemoryEps
  lm <- LowMem inner store
          <$> newMVar ()
          <*> newIORef emptyModuleEnv
          <*> newIORef emptyNameEnv
          <*> newIORef (LowMemStats 0 0 0 0 0)
  let eps = inner
        { eps_lookupTyThing  = lookupTyThing lm
        , eps_moduleTyThings = moduleTyThings lm
        , eps_addModule      = addModule lm
        , eps_getStats       = do
            s <- eps_getStats inner
            n <- lms_decls <$> readIORef (lm_stats lm)
            pure s { n_decls_in = n_decls_in s + n }
        }
  pure (eps, readIORef (lm_stats lm))

bump :: LowMem -> (LowMemStats -> LowMemStats) -> IO ()
bump lm f = atomicModifyIORef' (lm_stats lm) (\s -> (f s, ()))

----------------------------------------------------------------------------
-- Adding a module

addModule :: LowMem -> EpsModule -> IO ()
addModule lm epm = withMVar (lm_lock lm) $ \_ -> do
  let mod = epm_module epm
  mods <- readIORef (lm_mods lm)
  failed <- isJust <$> eps_lookupIface (lm_inner lm) mod
  unless (mod `elemModuleEnv` mods || failed) $ do
    let decls = epm_iface_decls epm
    keys <- ds_put (lm_store lm) mod (epm_location epm) (map snd decls)
    -- Force everything we keep, so that nothing retains the 'IfaceDecl's.
    infos <- forM (zip keys decls) $ \(k, (fp, d)) ->
               evaluate (DeclInfo k (occName (ifName d)) fp)
    let occs = mkOccEnv
                 [ i `seq` (occ, i)
                 | (i, (_, d)) <- zip [0 :: Int ..] decls
                 , occ <- occName (ifName d) : ifaceDeclImplicitBndrs d ]
    -- Deeply: the unevaluated map would retain the declarations.
    _ <- evaluate (nonDetFoldOccEnv (+) 0 occs)
    let !arr = listArray (0, length infos - 1) infos
        entry = ModEntry
          { me_occs      = occs
          , me_decls     = arr
          , me_rehydrate = epm_rehydrate epm
          }
        -- Replaces the hash cache of the 'ExternalIface', which retains
        -- the declarations. Must agree with 'mkIfaceHashCache': implicit
        -- binders have their own hash, see 'ifaceDeclFingerprints'.
        hash_fn occ = do
          i <- lookupOccEnv occs occ
          let di = arr ! i
          pure $ if occ == di_occ di
                   then (occ, di_hash di)
                   else (occ, computeFingerprint (panic "LowMem.hash_fn")
                                                 (di_hash di, occ))
        !ext_iface = setExternalIfaceHashFn hash_fn (epm_ext_iface epm)
    -- First our index, so that the names of the module can be looked up as
    -- soon as the inner store knows the module.
    atomicModifyIORef' (lm_mods lm) (\m -> (extendModuleEnv m mod entry, ()))
    bump lm (\s -> s { lms_modules = lms_modules s + 1
                     , lms_decls   = lms_decls s + length infos })
    eps_addModule (lm_inner lm) epm
      { epm_ext_iface   = ext_iface
      , epm_decls       = []
      , epm_iface_decls = []
      }

----------------------------------------------------------------------------
-- Lookups

lookupTyThing :: LowMem -> Name -> IO (Maybe TyThing)
lookupTyThing lm name
  | Just mod <- nameModule_maybe name = do
      mods <- readIORef (lm_mods lm)
      case lookupModuleEnv mods mod of
        -- Not a module of ours, but maybe hi-boot declarations
        -- (eps_addBootDecls) in the inner store.
        Nothing -> eps_lookupTyThing (lm_inner lm) name
        Just me -> case lookupOccEnv (me_occs me) (nameOccName name) of
          Nothing -> pure Nothing
          Just i  -> do
            bump lm (\s -> s { lms_lookups = lms_lookups s + 1 })
            lookupCache lm name >>= \case
              Just thing -> do
                bump lm (\s -> s { lms_cache_hits = lms_cache_hits s + 1 })
                pure (Just thing)
              Nothing -> Just <$> rehydrate lm me i name
  | otherwise = pure Nothing

lookupCache :: LowMem -> Name -> IO (Maybe TyThing)
lookupCache lm name = do
  cache <- readIORef (lm_cache lm)
  maybe (pure Nothing) derefEntry (lookupNameEnv cache name)

derefEntry :: CacheEntry -> IO (Maybe TyThing)
derefEntry (Pending t) = pure (Just t)
derefEntry (Self w)    = deRefWeak w
derefEntry (Via w)     = deRefWeak w

-- | Fetch the declaration, typecheck it again and cache the declared thing
-- and its implicit things. Returns the thing called @name@.
--
-- All of them are cached as 'Pending' first. When one of them is forced, it
-- becomes 'Self' and the others become 'Via' it (see 'promote'): the others
-- typically reference it (or vice versa), so a strong reference from the
-- cache would keep it alive forever.
rehydrate :: LowMem -> ModEntry -> Int -> Name -> IO TyThing
rehydrate lm me i name = do
  bump lm (\s -> s { lms_rehydrations = lms_rehydrations s + 1 })
  decl   <- ds_get (lm_store lm) (di_key (me_decls me ! i))
  things <- me_rehydrate me decl
  let ref = lm_cache lm
      -- A lazy knot: each wrapper knows the wrappers of its siblings.
      wrapped = [ (n, promote ref n t [ s | s@(n', _) <- wrapped, n' /= n ])
                | (n, t) <- things ]
  results <- forM wrapped $ \(n, t') -> do
    -- Another thread may have been faster: then use its copy.
    existing <- lookupCache lm n
    case existing of
      Just old -> pure (n, old)
      Nothing  -> do
        atomicModifyIORef' ref (\cache -> (extendNameEnv cache n (Pending t'), ()))
        pure (n, t')
  case lookup name results of
    Just t  -> pure t
    Nothing -> error ("EpsStore.LowMem: rehydrated declaration does not define "
                      ++ occNameString (nameOccName name))

-- | When the 'TyThing' is forced, replace its 'Pending' cache entry by a weak
-- pointer, so that the cache does not keep it alive, and make the entries of
-- its siblings weak pointers keyed on it.
--
-- Must not force the siblings: they may refer back to this thing, which is
-- under evaluation.
promote :: IORef (NameEnv CacheEntry) -> Name -> TyThing -> [(Name, TyThing)] -> TyThing
promote ref n t siblings = unsafePerformIO $ do
  v <- evaluate t
  key <- weakKey v
  w <- mkWeakOn key v
  sibling_ws <- forM siblings $ \(n', t') -> (,) n' <$> mkWeakOn key t'
  sibling_entries <- forM sibling_ws $ \(n', w') -> do
    -- Keep entries of siblings which are alive by themselves.
    cache <- readIORef ref
    keep <- case lookupNameEnv cache n' of
      Just e@(Self _) -> isJust <$> derefEntry e
      _               -> pure False
    pure (if keep then Nothing else Just (n', Via w'))
  atomicModifyIORef' ref $ \cache ->
    ( extendNameEnvList (extendNameEnv cache n (Self w))
                        [ e | Just e <- sibling_entries ]
    , () )
  pure v
{-# NOINLINE promote #-}

-- | What weak pointers to a 'TyThing' are keyed on: the object inside it. The
-- 'TyThing' box itself is usually dropped right after the lookup, while the
-- 'TyCon', 'Id', ... is referenced from types and Core.
data WeakKey = forall k. WeakKey !k
  -- Strict: the field of the 'TyThing' may still be a thunk, but the weak
  -- pointer must be keyed on the evaluated object that everybody references.

weakKey :: TyThing -> IO WeakKey
weakKey v = case v of
  AnId x                    -> key x
  ATyCon x                  -> key x
  ACoAxiom x                -> key x
  AConLike (RealDataCon x)  -> key x
  AConLike (PatSynCon x)    -> key x
  where
    key :: k -> IO WeakKey
    key x = evaluate (WeakKey x)

mkWeakOn :: WeakKey -> v -> IO (Weak v)
mkWeakOn (WeakKey k) v = mkWeak k v Nothing

-- | All the things of a module (for error messages and the GHC API, so it
-- does not matter that this is slow): rehydrates every declaration, bypassing
-- the cache.
moduleTyThings :: LowMem -> Module -> IO [TyThing]
moduleTyThings lm mod = do
  mods <- readIORef (lm_mods lm)
  case lookupModuleEnv mods mod of
    Nothing -> eps_moduleTyThings (lm_inner lm) mod
    Just me -> fmap concat $ forM (elems (me_decls me)) $ \di -> do
      decl <- ds_get (lm_store lm) (di_key di)
      map snd <$> me_rehydrate me decl
