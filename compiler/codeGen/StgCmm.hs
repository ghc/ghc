-----------------------------------------------------------------------------
--
-- Stg to C-- code generation
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmm ( codeGen ) where

#define FAST_STRING_NOT_NEEDED
#include "HsVersions.h"

import StgCmmProf (initCostCentres, ldvEnter)
import StgCmmMonad
import StgCmmEnv
import StgCmmBind
import StgCmmCon
import StgCmmLayout
import StgCmmUtils
import StgCmmClosure
import StgCmmHpc
import StgCmmTicky

import Cmm
import CLabel

import StgSyn
import DynFlags

import HscTypes
import CostCentre
import Id
import IdInfo
import Type
import DataCon
import Name
import TyCon
import Module
import ErrUtils
import Outputable
import Stream
import BasicTypes

import OrdList
import MkGraph

import Data.IORef
import Control.Monad (when,void)
import Util

codeGen :: DynFlags
        -> Module
        -> [TyCon]
        -> CollectedCCs                -- (Local/global) cost-centres needing declaring/registering.
        -> [StgBinding]                -- Bindings to convert
        -> HpcInfo
        -> Stream IO CmmGroup ()       -- Output as a stream, so codegen can
                                        -- be interleaved with output

codeGen dflags this_mod data_tycons
        cost_centre_info stg_binds hpc_info
  = do  { liftIO $ showPass dflags "New CodeGen"

              -- cg: run the code generator, and yield the resulting CmmGroup
              -- Using an IORef to store the state is a bit crude, but otherwise
              -- we would need to add a state monad layer.
        ; cgref <- liftIO $ newIORef =<< initC
        ; let cg :: FCode () -> Stream IO CmmGroup ()
              cg fcode = do
                cmm <- liftIO $ do
                         st <- readIORef cgref
                         let (a,st') = runC dflags this_mod st (getCmm fcode)

                         -- NB. stub-out cgs_tops and cgs_stmts.  This fixes
                         -- a big space leak.  DO NOT REMOVE!
                         writeIORef cgref $! st'{ cgs_tops = nilOL,
                                                  cgs_stmts = mkNop }
                         return a
                yield cmm

               -- Note [codegen-split-init] the cmm_init block must come
               -- FIRST.  This is because when -split-objs is on we need to
               -- combine this block with its initialisation routines; see
               -- Note [pipeline-split-init].
        ; cg (mkModuleInit cost_centre_info this_mod hpc_info)

        ; mapM_ (cg . cgTopBinding dflags) stg_binds

                -- Put datatype_stuff after code_stuff, because the
                -- datatype closure table (for enumeration types) to
                -- (say) PrelBase_True_closure, which is defined in
                -- code_stuff
        ; let do_tycon tycon = do
                -- Generate a table of static closures for an
                -- enumeration type Note that the closure pointers are
                -- tagged.
                 when (isEnumerationTyCon tycon) $ cg (cgEnumerationTyCon tycon)
                 mapM_ (cg . cgDataCon) (tyConDataCons tycon)

        ; mapM_ do_tycon data_tycons
        }

---------------------------------------------------------------
--      Top-level bindings
---------------------------------------------------------------

{- 'cgTopBinding' is only used for top-level bindings, since they need
to be allocated statically (not in the heap) and need to be labelled.
No unboxed bindings can happen at top level.

In the code below, the static bindings are accumulated in the
@MkCgState@, and transferred into the ``statics'' slot by @forkStatics@.
This is so that we can write the top level processing in a compositional
style, with the increasing static environment being plumbed as a state
variable. -}

cgTopBinding :: DynFlags -> StgBinding -> FCode ()
cgTopBinding dflags (StgNonRec id rhs)
  = do  { id' <- maybeExternaliseId dflags id
        ; let (info, fcode) = cgTopRhs dflags NonRecursive id' rhs
        ; fcode
        ; addBindC info -- Add the *un-externalised* Id to the envt,
                        -- so we find it when we look up occurrences
        }

cgTopBinding dflags (StgRec pairs)
  = do  { let (bndrs, rhss) = unzip pairs
        ; bndrs' <- Prelude.mapM (maybeExternaliseId dflags) bndrs
        ; let pairs' = zip bndrs' rhss
              r = unzipWith (cgTopRhs dflags Recursive) pairs'
              (infos, fcodes) = unzip r
        ; addBindsC infos
        ; sequence_ fcodes
        }


cgTopRhs :: DynFlags -> RecFlag -> Id -> StgRhs -> (CgIdInfo, FCode ())
        -- The Id is passed along for setting up a binding...
        -- It's already been externalised if necessary

cgTopRhs dflags _rec bndr (StgRhsCon _cc con args)
  = cgTopRhsCon dflags bndr con args

cgTopRhs dflags rec bndr (StgRhsClosure cc bi fvs upd_flag _srt args body)
  = ASSERT(null fvs)    -- There should be no free variables
    cgTopRhsClosure dflags rec bndr cc bi upd_flag args body


---------------------------------------------------------------
--      Module initialisation code
---------------------------------------------------------------

{- The module initialisation code looks like this, roughly:

        FN(__stginit_Foo) {
          JMP_(__stginit_Foo_1_p)
        }

        FN(__stginit_Foo_1_p) {
        ...
        }

   We have one version of the init code with a module version and the
   'way' attached to it.  The version number helps to catch cases
   where modules are not compiled in dependency order before being
   linked: if a module has been compiled since any modules which depend on
   it, then the latter modules will refer to a different version in their
   init blocks and a link error will ensue.

   The 'way' suffix helps to catch cases where modules compiled in different
   ways are linked together (eg. profiled and non-profiled).

   We provide a plain, unadorned, version of the module init code
   which just jumps to the version with the label and way attached.  The
   reason for this is that when using foreign exports, the caller of
   startupHaskell() must supply the name of the init function for the "top"
   module in the program, and we don't want to require that this name
   has the version and way info appended to it.

We initialise the module tree by keeping a work-stack,
        * pointed to by Sp
        * that grows downward
        * Sp points to the last occupied slot
-}

mkModuleInit
        :: CollectedCCs         -- cost centre info
        -> Module
        -> HpcInfo
        -> FCode ()

mkModuleInit cost_centre_info this_mod hpc_info
  = do  { initHpc this_mod hpc_info
        ; initCostCentres cost_centre_info
            -- For backwards compatibility: user code may refer to this
            -- label for calling hs_add_root().
        ; emitDecl (CmmData Data (Statics (mkPlainModuleInitLabel this_mod) []))
        }


---------------------------------------------------------------
--      Generating static stuff for algebraic data types
---------------------------------------------------------------


cgEnumerationTyCon :: TyCon -> FCode ()
cgEnumerationTyCon tycon
  = do dflags <- getDynFlags
       emitRODataLits (mkLocalClosureTableLabel (tyConName tycon) NoCafRefs)
             [ CmmLabelOff (mkLocalClosureLabel (dataConName con) NoCafRefs)
                           (tagForCon dflags con)
             | con <- tyConDataCons tycon]


cgDataCon :: DataCon -> FCode ()
-- Generate the entry code, info tables, and (for niladic constructor)
-- the static closure, for a constructor.
cgDataCon data_con
  = do  { dflags <- getDynFlags
        ; let
            (tot_wds, --  #ptr_wds + #nonptr_wds
             ptr_wds, --  #ptr_wds
             arg_things) = mkVirtConstrOffsets dflags arg_reps

            nonptr_wds   = tot_wds - ptr_wds

            sta_info_tbl = mkDataConInfoTable dflags data_con True  ptr_wds nonptr_wds
            dyn_info_tbl = mkDataConInfoTable dflags data_con False ptr_wds nonptr_wds

            emit_info info_tbl ticky_code
                = emitClosureAndInfoTable info_tbl NativeDirectCall []
                             $ mk_code ticky_code

            mk_code ticky_code
              =         -- NB: We don't set CC when entering data (WDP 94/06)
                do { _ <- ticky_code
                   ; ldvEnter (CmmReg nodeReg)
                   ; tickyReturnOldCon (length arg_things)
                   ; void $ emitReturn [cmmOffsetB dflags (CmmReg nodeReg)
                                            (tagForCon dflags data_con)]
                   }
                        -- The case continuation code expects a tagged pointer

            arg_reps :: [(PrimRep, UnaryType)]
            arg_reps = [(typePrimRep rep_ty, rep_ty) | ty <- dataConRepArgTys data_con, rep_ty <- flattenRepType (repType ty)]

            -- Dynamic closure code for non-nullary constructors only
        ; when (not (isNullaryRepDataCon data_con))
                (emit_info dyn_info_tbl tickyEnterDynCon)

                -- Dynamic-Closure first, to reduce forward references
        ; emit_info sta_info_tbl tickyEnterStaticCon }


---------------------------------------------------------------
--      Stuff to support splitting
---------------------------------------------------------------

maybeExternaliseId :: DynFlags -> Id -> FCode Id
maybeExternaliseId dflags id
  | gopt Opt_SplitObjs dflags,  -- See Note [Externalise when splitting]
                                -- in StgCmmMonad
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
