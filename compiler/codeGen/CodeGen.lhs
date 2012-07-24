%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

The Code Generator

This module says how things get going at the top level.

@codeGen@ is the interface to the outside world. The \tr{cgTop*}
functions drive the mangling of top-level bindings.

\begin{code}

module CodeGen ( codeGen ) where

#include "HsVersions.h"

-- Required so that CgExpr is reached via at least one non-SOURCE
-- import. Before, that wasn't the case, and CM therefore didn't
-- bother to compile it.
import CgExpr ( {-NOTHING!-} ) -- DO NOT DELETE THIS IMPORT
import CgProf
import CgMonad
import CgBindery
import CgClosure
import CgCon
import CgUtils
import CgHpc

import CLabel
import OldCmm
import OldPprCmm ()

import StgSyn
import PrelNames
import DynFlags
import StaticFlags

import HscTypes
import CostCentre
import Id
import Name
import TyCon
import Module
import ErrUtils
import Panic
import Outputable
import Util

import OrdList
import Stream (Stream, liftIO)
import qualified Stream

import Data.IORef

codeGen :: DynFlags
        -> Module                     -- Module we are compiling
        -> [TyCon]                    -- Type constructors
        -> CollectedCCs               -- (Local/global) cost-centres needing declaring/registering.
        -> [(StgBinding,[(Id,[Id])])] -- Bindings to convert, with SRTs
        -> HpcInfo                    -- Profiling info
        -> Stream IO CmmGroup ()
              -- N.B. returning '[Cmm]' and not 'Cmm' here makes it
              -- possible for object splitting to split up the
              -- pieces later.

codeGen dflags this_mod data_tycons cost_centre_info stg_binds hpc_info

   = do { liftIO $ showPass dflags "CodeGen"

        ; cgref <- liftIO $ newIORef =<< initC
        ; let cg :: FCode CmmGroup -> Stream IO CmmGroup ()
              cg fcode = do
                cmm <- liftIO $ do
                         st <- readIORef cgref
                         let (a,st') = runC dflags this_mod st fcode

                         dumpIfSet_dyn dflags Opt_D_dump_cmm "Cmm" $ ppr a

                         -- NB. stub-out cgs_tops and cgs_stmts.  This fixes
                         -- a big space leak.  DO NOT REMOVE!
                         writeIORef cgref $! st'{ cgs_tops = nilOL,
                                                  cgs_stmts = nilOL }
                         return a
                Stream.yield cmm

        ; cg (getCmm $ mkModuleInit dflags cost_centre_info this_mod hpc_info)

        ; mapM_ (cg . getCmm . cgTopBinding dflags) stg_binds

        ; mapM_ (cg . cgTyCon) data_tycons
        }

mkModuleInit
        :: DynFlags
        -> CollectedCCs         -- cost centre info
        -> Module
        -> HpcInfo
        -> Code

mkModuleInit dflags cost_centre_info this_mod hpc_info
  = do  { -- Allocate the static boolean that records if this
        ; whenC (opt_Hpc) $
              hpcTable this_mod hpc_info

        ; whenC (dopt Opt_SccProfilingOn dflags) $ do
            initCostCentres cost_centre_info

            -- For backwards compatibility: user code may refer to this
            -- label for calling hs_add_root().
        ; emitDecl (CmmData Data (Statics (mkPlainModuleInitLabel this_mod) []))

        ; whenC (this_mod == mainModIs dflags) $
             emitSimpleProc (mkPlainModuleInitLabel rOOT_MAIN) $ return ()
    }
\end{code}



Cost-centre profiling: Besides the usual stuff, we must produce
declarations for the cost-centres defined in this module;

(The local cost-centres involved in this are passed into the
code-generator.)

\begin{code}
initCostCentres :: CollectedCCs -> Code
-- Emit the declarations, and return code to register them
initCostCentres (local_CCs, ___extern_CCs, singleton_CCSs)
  = do dflags <- getDynFlags
       if not (dopt Opt_SccProfilingOn dflags)
           then nopC
           else do mapM_ emitCostCentreDecl      local_CCs
                   mapM_ emitCostCentreStackDecl singleton_CCSs
\end{code}

%************************************************************************
%*                                                                      *
\subsection[codegen-top-bindings]{Converting top-level STG bindings}
%*                                                                      *
%************************************************************************

@cgTopBinding@ is only used for top-level bindings, since they need
to be allocated statically (not in the heap) and need to be labelled.
No unboxed bindings can happen at top level.

In the code below, the static bindings are accumulated in the
@MkCgState@, and transferred into the ``statics'' slot by @forkStatics@.
This is so that we can write the top level processing in a compositional
style, with the increasing static environment being plumbed as a state
variable.

\begin{code}
cgTopBinding :: DynFlags -> (StgBinding,[(Id,[Id])]) -> Code
cgTopBinding dflags (StgNonRec id rhs, srts)
  = do  { id' <- maybeExternaliseId dflags id
        ; mapM_ (mkSRT [id']) srts
        ; (id,info) <- cgTopRhs id' rhs
        ; addBindC id info      -- Add the *un-externalised* Id to the envt,
                                -- so we find it when we look up occurrences
        }

cgTopBinding dflags (StgRec pairs, srts)
  = do  { let (bndrs, rhss) = unzip pairs
        ; bndrs' <- mapFCs (maybeExternaliseId dflags) bndrs
        ; let pairs' = zip bndrs' rhss
        ; mapM_ (mkSRT bndrs')  srts
        ; _new_binds <- fixC (\ new_binds -> do
                { addBindsC new_binds
                ; mapFCs ( \ (b,e) -> cgTopRhs b e ) pairs' })
        ; nopC }

mkSRT :: [Id] -> (Id,[Id]) -> Code
mkSRT _ (_,[])  = nopC
mkSRT these (id,ids)
  = do  { ids <- mapFCs remap ids
        ; id  <- remap id
        ; emitRODataLits "CodeGen.mkSRT" (mkSRTLabel (idName id) (idCafInfo id))
               (map (\id -> CmmLabel $ mkClosureLabel (idName id) (idCafInfo id)) ids)
        }
  where
        -- Sigh, better map all the ids against the environment in
        -- case they've been externalised (see maybeExternaliseId below).
    remap id = case filter (==id) these of
                (id':_) -> returnFC id'
                [] -> do { info <- getCgIdInfo id; return (cgIdInfoId info) }

-- Urgh!  I tried moving the forkStatics call from the rhss of cgTopRhs
-- to enclose the listFCs in cgTopBinding, but that tickled the
-- statics "error" call in initC.  I DON'T UNDERSTAND WHY!

cgTopRhs :: Id -> StgRhs -> FCode (Id, CgIdInfo)
        -- The Id is passed along for setting up a binding...
        -- It's already been externalised if necessary

cgTopRhs bndr (StgRhsCon _cc con args)
  = forkStatics (cgTopRhsCon bndr con args)

cgTopRhs bndr (StgRhsClosure cc bi fvs upd_flag srt args body)
  = ASSERT(null fvs)    -- There should be no free variables
    setSRTLabel (mkSRTLabel (idName bndr) (idCafInfo bndr)) $
    setSRT srt $
    forkStatics (cgTopRhsClosure bndr cc bi upd_flag args body)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Stuff to support splitting}
%*                                                                      *
%************************************************************************

If we're splitting the object, we need to externalise all the top-level names
(and then make sure we only use the externalised one in any C label we use
which refers to this name).

\begin{code}
maybeExternaliseId :: DynFlags -> Id -> FCode Id
maybeExternaliseId dflags id
  | dopt Opt_SplitObjs dflags,  -- Externalise the name for -split-objs
    isInternalName name = do { mod <- getModuleName
                             ; returnFC (setIdName id (externalise mod)) }
  | otherwise           = returnFC id
  where
    externalise mod = mkExternalName uniq mod new_occ loc
    name    = idName id
    uniq    = nameUnique name
    new_occ = mkLocalOcc uniq (nameOccName name)
    loc     = nameSrcSpan name
        -- We want to conjure up a name that can't clash with any
        -- existing name.  So we generate
        --      Mod_$L243foo
        -- where 243 is the unique.
\end{code}
