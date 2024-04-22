-- (c) The University of Glasgow 2002-2006

{-# LANGUAGE RankNTypes #-}

module GHC.Iface.Env (
        newGlobalBinder, newInteractiveBinder,
        externaliseName,
        lookupIfaceTop,
        lookupOrig, lookupNameCache, lookupOrigNameCache,
        newIfaceName, newIfaceNames,
        extendIfaceIdEnv, extendIfaceTyVarEnv,
        tcIfaceLclId, tcIfaceTyVar, lookupIfaceVar,
        lookupIfaceTyVar, extendIfaceEnvs,
        setNameModule,

        ifaceExportNames,

        trace_if, trace_hi_diffs,

        -- Name-cache stuff
        allocateGlobalBinder,
   ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.DynFlags

import GHC.Tc.Utils.Monad
import GHC.Core.Type
import GHC.Iface.Type
import GHC.Runtime.Context

import GHC.Unit.Module
import GHC.Unit.Module.ModIface

import GHC.Data.FastString.Env

import GHC.Types.Var
import GHC.Types.Name
import GHC.Types.Avail
import GHC.Types.Name.Cache
import GHC.Types.Unique.Supply
import GHC.Types.SrcLoc

import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Utils.Logger

import Data.List     ( partition )
import Control.Monad

{-
*********************************************************
*                                                      *
        Allocating new Names in the Name Cache
*                                                      *
*********************************************************

See Also: Note [The Name Cache] in GHC.Types.Name.Cache
-}

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
-- Used for source code and interface files, to make the
-- Name for a thing, given its Module and OccName
-- See Note [The Name Cache] in GHC.Types.Name.Cache
--
-- The cache may already have a binding for this thing,
-- because we may have seen an occurrence before, but now is the
-- moment when we know its Module and SrcLoc in their full glory

newGlobalBinder mod occ loc
  = do { hsc_env <- getTopEnv
       ; name <- liftIO $ allocateGlobalBinder (hsc_NC hsc_env) mod occ loc
       ; traceIf (text "newGlobalBinder" <+>
                  (vcat [ ppr mod <+> ppr occ <+> ppr loc, ppr name]))
       ; return name }

newInteractiveBinder :: HscEnv -> OccName -> SrcSpan -> IO Name
-- Works in the IO monad, and gets the Module
-- from the interactive context
newInteractiveBinder hsc_env occ loc = do
  let mod = icInteractiveModule (hsc_IC hsc_env)
  allocateGlobalBinder (hsc_NC hsc_env) mod occ loc

allocateGlobalBinder
  :: NameCache
  -> Module -> OccName -> SrcSpan
  -> IO Name
-- See Note [The Name Cache] in GHC.Types.Name.Cache
allocateGlobalBinder nc mod occ loc
  = updateNameCache nc mod occ $ \cache0 -> do
      case lookupOrigNameCache cache0 mod occ of
        -- A hit in the cache!  We are at the binding site of the name.
        -- This is the moment when we know the SrcLoc
        -- of the Name, so we set this field in the Name we return.
        --
        -- Then (bogus) multiple bindings of the same Name
        -- get different SrcLocs can be reported as such.
        --
        -- Possible other reason: it might be in the cache because we
        --      encountered an occurrence before the binding site for an
        --      implicitly-imported Name.  Perhaps the current SrcLoc is
        --      better... but not really: it'll still just say 'imported'
        --
        -- IMPORTANT: Don't mess with wired-in names.
        --            Their wired-in-ness is in their NameSort
        --            and their Module is correct.

        Just name | isWiredInName name
                  -> pure (cache0, name)
                  | otherwise
                  -> pure (new_cache, name')
                  where
                    uniq      = nameUnique name
                    name'     = mkExternalName uniq mod occ loc
                                -- name' is like name, but with the right SrcSpan
                    new_cache = extendOrigNameCache cache0 mod occ name'

        -- Miss in the cache!
        -- Build a completely new Name, and put it in the cache
        _ -> do
              uniq <- takeUniqFromNameCache nc
              let name      = mkExternalName uniq mod occ loc
              let new_cache = extendOrigNameCache cache0 mod occ name
              pure (new_cache, name)

ifaceExportNames :: [IfaceExport] -> TcRnIf gbl lcl [AvailInfo]
ifaceExportNames exports = return exports

{-
************************************************************************
*                                                                      *
                Name cache access
*                                                                      *
************************************************************************
-}

-- | Look up the 'Name' for a given 'Module' and 'OccName'.
-- Consider alternatively using 'lookupIfaceTop' if you're in the 'IfL' monad
-- and 'Module' is simply that of the 'ModIface' you are typechecking.
lookupOrig :: Module -> OccName -> TcRnIf a b Name
lookupOrig mod occ = do
  hsc_env <- getTopEnv
  traceIf (text "lookup_orig" <+> ppr mod <+> ppr occ)
  liftIO $ lookupNameCache (hsc_NC hsc_env) mod occ

lookupNameCache :: NameCache -> Module -> OccName -> IO Name
-- Lookup up the (Module,OccName) in the NameCache
-- If you find it, return it; if not, allocate a fresh original name and extend
-- the NameCache.
-- Reason: this may the first occurrence of (say) Foo.bar we have encountered.
-- If we need to explore its value we will load Foo.hi; but meanwhile all we
-- need is a Name for it.
lookupNameCache nc mod occ = updateNameCache nc mod occ $ \cache0 ->
  case lookupOrigNameCache cache0 mod occ of
    Just name -> pure (cache0, name)
    Nothing   -> do
      uniq <- takeUniqFromNameCache nc
      let name      = mkExternalName uniq mod occ noSrcSpan
      let new_cache = extendOrigNameCache cache0 mod occ name
      pure (new_cache, name)

externaliseName :: Module -> Name -> TcRnIf m n Name
-- Take an Internal Name and make it an External one,
-- with the same unique
externaliseName mod name
  = do { let occ = nameOccName name
             loc = nameSrcSpan name
             uniq = nameUnique name
       ; occ `seq` return ()  -- c.f. seq in newGlobalBinder
       ; hsc_env <- getTopEnv
       ; liftIO $ updateNameCache (hsc_NC hsc_env) mod occ $ \cache -> do
         let name'  = mkExternalName uniq mod occ loc
             cache' = extendOrigNameCache cache mod occ name'
         pure (cache', name') }

-- | Set the 'Module' of a 'Name'.
setNameModule :: Maybe Module -> Name -> TcRnIf m n Name
setNameModule Nothing n = return n
setNameModule (Just m) n =
    newGlobalBinder m (nameOccName n) (nameSrcSpan n)

{-
************************************************************************
*                                                                      *
                Type variables and local Ids
*                                                                      *
************************************************************************
-}

tcIfaceLclId :: IfLclName -> IfL Id
tcIfaceLclId occ
  = do  { lcl <- getLclEnv
        ; case lookupFsEnv (if_id_env lcl) (ifLclNameFS occ) of
            Just ty_var -> return ty_var
            Nothing     -> failIfM $
              vcat
                [ text "Iface id out of scope: " <+> ppr occ
                , text "env:" <+> ppr (if_id_env lcl) ]
        }

extendIfaceIdEnv :: [Id] -> IfL a -> IfL a
extendIfaceIdEnv ids
  = updLclEnv $ \env ->
    let { id_env' = extendFsEnvList (if_id_env env) pairs
        ; pairs   = [(occNameFS (getOccName id), id) | id <- ids] }
    in env { if_id_env = id_env' }


tcIfaceTyVar :: IfLclName -> IfL TyVar
tcIfaceTyVar occ
  = do  { lcl <- getLclEnv
        ; case lookupFsEnv (if_tv_env lcl) (ifLclNameFS occ) of
            Just ty_var -> return ty_var
            Nothing     -> failIfM (text "Iface type variable out of scope: " <+> ppr occ)
        }

lookupIfaceTyVar :: IfaceTvBndr -> IfL (Maybe TyVar)
lookupIfaceTyVar (occ, _)
  = do  { lcl <- getLclEnv
        ; return (lookupFsEnv (if_tv_env lcl) (ifLclNameFS occ)) }

lookupIfaceVar :: IfaceBndr -> IfL (Maybe TyCoVar)
lookupIfaceVar (IfaceIdBndr (_, occ, _))
  = do  { lcl <- getLclEnv
        ; return (lookupFsEnv (if_id_env lcl) (ifLclNameFS occ)) }
lookupIfaceVar (IfaceTvBndr (occ, _))
  = do  { lcl <- getLclEnv
        ; return (lookupFsEnv (if_tv_env lcl) (ifLclNameFS occ)) }

extendIfaceTyVarEnv :: [TyVar] -> IfL a -> IfL a
extendIfaceTyVarEnv tyvars
  = updLclEnv $ \env ->
    let { tv_env' = extendFsEnvList (if_tv_env env) pairs
        ; pairs   = [(occNameFS (getOccName tv), tv) | tv <- tyvars] }
    in env { if_tv_env = tv_env' }

extendIfaceEnvs :: [TyCoVar] -> IfL a -> IfL a
extendIfaceEnvs tcvs thing_inside
  = extendIfaceTyVarEnv tvs $
    extendIfaceIdEnv    cvs $
    thing_inside
  where
    (tvs, cvs) = partition isTyVar tcvs

{-
************************************************************************
*                                                                      *
                Getting from RdrNames to Names
*                                                                      *
************************************************************************
-}

-- | Look up a top-level name from the current Iface module
lookupIfaceTop :: OccName -> IfL Name
lookupIfaceTop occ
  = do  { env <- getLclEnv; lookupOrig (if_mod env) occ }

newIfaceName :: OccName -> IfL Name
newIfaceName occ
  = do  { uniq <- newUnique
        ; return $! mkInternalName uniq occ noSrcSpan }

newIfaceNames :: [OccName] -> IfL [Name]
newIfaceNames occs
  = do  { uniqs <- getUniquesM
        ; return [ mkInternalName uniq occ noSrcSpan
                 | (occ,uniq) <- occs `zip` uniqs] }

trace_if :: Logger -> SDoc -> IO ()
{-# INLINE trace_if #-} -- see Note [INLINE conditional tracing utilities]
trace_if logger doc = when (logHasDumpFlag logger Opt_D_dump_if_trace) $ putMsg logger doc

trace_hi_diffs :: Logger -> SDoc -> IO ()
{-# INLINE trace_hi_diffs #-} -- see Note [INLINE conditional tracing utilities]
trace_hi_diffs logger doc = when (logHasDumpFlag logger Opt_D_dump_hi_diffs) $ putMsg logger doc
