{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Core.Opt.DmdAnal
   ( DmdAnalOpts(..)
   , dmdAnalProgramIO
   , dmdAnalProgramPure
   )
where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Utils.Logger as Logger
import GHC.Utils.Outputable

import GHC.Types.Demand
import GHC.Types.Id.Info

import GHC.Core
import GHC.Core.Seq
import GHC.Core.FamInstEnv
import GHC.Core.Utils
import GHC.Core.Opt.DmdAnal.Framework
import GHC.Core.Opt.DmdAnal.Denot

dmdAnalProgramIO :: Logger -> DynFlags -> FamInstEnvs -> [CoreRule] -> CoreProgram -> IO CoreProgram
dmdAnalProgramIO logger dflags fam_envs rules binds = do
  let !opts = DmdAnalOpts
               { dmd_strict_dicts      = gopt Opt_DictsStrict dflags
               , dmd_unbox_width       = dmdUnboxWidth dflags
               , dmd_iter_check_prefs  = dmdAnalIterCheckPrefixes dflags
               }
      binds_plus_dmds = dmdAnalProgramPure opts fam_envs rules binds
  Logger.putDumpFileMaybe logger Opt_D_dump_dmd_signatures "Demand signatures" FormatText $
    dumpIdInfoOfProgram (ppr . zapDmdEnvSig . dmdSigInfo) binds_plus_dmds
  return binds_plus_dmds

-- | Outputs a new copy of the Core program in which binders have been annotated
-- with usage and strictness information.
dmdAnalProgramPure :: DmdAnalOpts -> FamInstEnvs -> [CoreRule] -> CoreProgram -> CoreProgram
dmdAnalProgramPure opts fam_envs rules binds = {-# SCC "dmdAnalProgram" #-}
  case runBuilder (denotProgram opts fam_envs rules binds) of
    -- See Note [Stamp out space leaks in demand analysis] in GHC.Core.Opt.DmdAnal
    framework -> forceProgram $ annotateProgram (evalFramework framework) binds
  where
    forceProgram prog = seqBinds prog `seq` prog

{- Note [Stamp out space leaks in demand analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The demand analysis pass outputs a new copy of the Core program in
which binders have been annotated with demand and strictness
information. It's tiresome to ensure that this information is fully
evaluated everywhere that we produce it, so we just run a single
seqBinds over the output before returning it, to ensure that there are
no references holding on to the input Core program.

This makes a ~30% reduction in peak memory usage when compiling
DynFlags (cf #9675 and #13426).

This is particularly important when we are doing late demand analysis,
since we don't do a seqBinds at any point thereafter. Hence code
generation would hold on to an extra copy of the Core program, via
unforced thunks in demand or strictness information; and it is the
most memory-intensive part of the compilation process, so this added
seqBinds makes a big difference in peak memory usage.

Note [Final Demand Analyser run]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some of the information that the demand analyser determines is not always
preserved by the simplifier.  For example, the simplifier will happily rewrite
  \y [Demand=MU] let x = y in x + x
to
  \y [Demand=MU] y + y
which is quite a lie: Now y occurs more than just once.

The once-used information is (currently) only used by the code
generator, though.  So:

 * We zap the used-once info in the worker-wrapper;
   see Note [Zapping Used Once info in WorkWrap] in
   GHC.Core.Opt.WorkWrap.
   If it's not reliable, it's better not to have it at all.

 * Just before TidyCore, we add a pass of the demand analyser,
      but WITHOUT subsequent worker/wrapper and simplifier,
   right before TidyCore.  See SimplCore.getCoreToDo.

   This way, correct information finds its way into the module interface
   (strictness signatures!) and the code generator (single-entry thunks!)

Note that, in contrast, the single-call information (CM(..)) /can/ be
relied upon, as the simplifier tends to be very careful about not
duplicating actual function calls.

Also see #11731.
-}
