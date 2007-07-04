module Vectorise( vectorise )
where

#include "HsVersions.h"

import DynFlags
import HscTypes

import CoreLint       ( showPass, endPass )
import TyCon
import Var
import VarEnv
import NameEnv

import DsMonad

import PrelNames

import Outputable

vectorise :: HscEnv -> ModGuts -> IO ModGuts
vectorise hsc_env guts
  | not (Opt_Vectorise `dopt` dflags) = return guts
  | otherwise
  = do
      showPass dflags "Vectorisation"
      eps <- hscEPS hsc_env
      let info = hptVectInfo hsc_env `plusVectInfo` eps_vect_info eps
      Just guts' <- initDs hsc_env (mg_module guts)
                                   (mg_rdr_env guts)
                                   (mg_types guts)
                                   (vectoriseModule info guts)
      endPass dflags "Vectorisation" Opt_D_dump_vect (mg_binds guts')
      return guts'
  where
    dflags = hsc_dflags hsc_env

-- ----------------------------------------------------------------------------
-- Vectorisation monad

data Builtins = Builtins {
                  parrayTyCon      :: TyCon
                , paTyCon          :: TyCon
                , closureTyCon     :: TyCon
                , mkClosureVar     :: Var
                , applyClosureVar  :: Var
                , mkClosurePVar    :: Var
                , applyClosurePVar :: Var
                , closurePAVar     :: Var
                , lengthPAVar      :: Var
                , replicatePAVar   :: Var
                }

initBuiltins :: DsM Builtins
initBuiltins
  = do
      parrayTyCon  <- dsLookupTyCon parrayTyConName
      paTyCon      <- dsLookupTyCon paTyConName
      closureTyCon <- dsLookupTyCon closureTyConName

      mkClosureVar     <- dsLookupGlobalId mkClosureName
      applyClosureVar  <- dsLookupGlobalId applyClosureName
      mkClosurePVar    <- dsLookupGlobalId mkClosurePName
      applyClosurePVar <- dsLookupGlobalId applyClosurePName
      closurePAVar     <- dsLookupGlobalId closurePAName
      lengthPAVar      <- dsLookupGlobalId lengthPAName
      replicatePAVar   <- dsLookupGlobalId replicatePAName

      return $ Builtins {
                 parrayTyCon      = parrayTyCon
               , paTyCon          = paTyCon
               , closureTyCon     = closureTyCon
               , mkClosureVar     = mkClosureVar
               , applyClosureVar  = applyClosureVar
               , mkClosurePVar    = mkClosurePVar
               , applyClosurePVar = applyClosurePVar
               , closurePAVar     = closurePAVar
               , lengthPAVar      = lengthPAVar
               , replicatePAVar   = replicatePAVar
               }

data VEnv = VEnv {
              -- Mapping from variables to their vectorised versions
              --
              vect_vars :: VarEnv Var

              -- Exported variables which have a vectorised version
              --
            , vect_exported_vars :: VarEnv (Var, Var)

              -- Mapping from TyCons to their vectorised versions.
              -- TyCons which do not have to be vectorised are mapped to
              -- themselves.
            , vect_tycons :: NameEnv TyCon
            }

initVEnv :: VectInfo -> DsM VEnv
initVEnv info
  = return $ VEnv {
               vect_vars          = mapVarEnv  snd $ vectInfoCCVar   info
             , vect_exported_vars = emptyVarEnv
             , vect_tycons        = mapNameEnv snd $ vectInfoCCTyCon info
             }

-- FIXME
updVectInfo :: VEnv -> ModGuts -> ModGuts
updVectInfo env guts = guts { mg_vect_info = info' }
  where
    info' = info {
              vectInfoCCVar   = vect_exported_vars env
            , vectInfoCCTyCon = tc_env
            }

    info  = mg_vect_info guts
    tyenv = mg_types guts

    tc_env = mkNameEnv [(tc_name, (tc,tc')) | tc <- typeEnvTyCons tyenv
                                            , let tc_name = tyConName tc
                                            , Just tc' <- [lookupNameEnv (vect_tycons env) tc_name]]

newtype VM a = VM { runVM :: Builtins -> VEnv -> DsM (VEnv, a) }

instance Monad VM where
  return x   = VM $ \bi env -> return (env, x)
  VM p >>= f = VM $ \bi env -> do
                                 (env', x) <- p bi env
                                 runVM (f x) bi env'

builtin :: (Builtins -> a) -> VM a
builtin f = VM $ \bi env -> return (env, f bi)

readEnv :: (VEnv -> a) -> VM a
readEnv f = VM $ \bi env -> return (env, f env)

setEnv :: VEnv -> VM ()
setEnv env = VM $ \_ _ -> return (env, ())

updEnv :: (VEnv -> VEnv) -> VM ()
updEnv f = VM $ \_ env -> return (f env, ())


lookupTyCon :: TyCon -> VM (Maybe TyCon)
lookupTyCon tc = readEnv $ \env -> lookupNameEnv (vect_tycons env) (tyConName tc)

-- ----------------------------------------------------------------------------
-- Bindings

vectoriseModule :: VectInfo -> ModGuts -> DsM ModGuts
vectoriseModule info guts
  = do
      builtins <- initBuiltins
      env <- initVEnv info
      (env', guts') <- runVM (vectModule guts) builtins env
      return $ updVectInfo env' guts'

vectModule :: ModGuts -> VM ModGuts
vectModule guts = return guts

