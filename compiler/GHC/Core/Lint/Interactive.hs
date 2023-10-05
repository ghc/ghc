{-# LANGUAGE ScopedTypeVariables #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


A ``lint'' pass to check for Core correctness.
See Note [Core Lint guarantee].
-}

module GHC.Core.Lint.Interactive (
    interactiveInScope,
 ) where

import GHC.Prelude

import GHC.Runtime.Context

import GHC.Core.Coercion
import GHC.Core.TyCo.FVs
import GHC.Core.InstEnv      ( instanceDFunId, instEnvElts )

import GHC.Types.Id
import GHC.Types.TypeEnv


interactiveInScope :: InteractiveContext -> [Var]
-- In GHCi we may lint expressions, or bindings arising from 'deriving'
-- clauses, that mention variables bound in the interactive context.
-- These are Local things (see Note [Interactively-bound Ids in GHCi] in GHC.Runtime.Context).
-- So we have to tell Lint about them, lest it reports them as out of scope.
--
-- We do this by find local-named things that may appear free in interactive
-- context.  This function is pretty revolting and quite possibly not quite right.
-- When we are not in GHCi, the interactive context (hsc_IC hsc_env) is empty
-- so this is a (cheap) no-op.
--
-- See #8215 for an example
interactiveInScope ictxt
  = tyvars ++ ids
  where
    -- C.f. GHC.Tc.Module.setInteractiveContext, Desugar.deSugarExpr
    (cls_insts, _fam_insts) = ic_instances ictxt
    te1    = mkTypeEnvWithImplicits (ic_tythings ictxt)
    te     = extendTypeEnvWithIds te1 (map instanceDFunId $ instEnvElts cls_insts)
    ids    = typeEnvIds te
    tyvars = tyCoVarsOfTypesList $ map idType ids
              -- Why the type variables?  How can the top level envt have free tyvars?
              -- I think it's because of the GHCi debugger, which can bind variables
              --   f :: [t] -> [t]
              -- where t is a RuntimeUnk (see TcType)
