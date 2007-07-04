module Vectorise( vectorise )
where

#include "HsVersions.h"

import DynFlags
import HscTypes

import CoreLint       ( showPass, endPass )
import TyCon
import Var
import VarEnv

import DsMonad

import PrelNames

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
            }

initVEnv :: VectInfo -> DsM VEnv
initVEnv info
  = return $ VEnv {
               vect_vars = mapVarEnv snd $ vectInfoCCVar info
             }

-- FIXME
updVectInfo :: VEnv -> VectInfo -> VectInfo
updVectInfo env info = info

newtype VM a = VM { runVM :: Builtins -> VEnv -> DsM (VEnv, a) }

instance Monad VM where
  return x   = VM $ \bi env -> return (env, x)
  VM p >>= f = VM $ \bi env -> do
                                 (env', x) <- p bi env
                                 runVM (f x) bi env'

vectoriseModule :: VectInfo -> ModGuts -> DsM ModGuts
vectoriseModule info guts
  = do
      builtins <- initBuiltins
      env <- initVEnv info
      (env', guts') <- runVM (vectModule guts) builtins env
      return $ guts' { mg_vect_info = updVectInfo env' info }

vectModule :: ModGuts -> VM ModGuts
vectModule guts = return guts

