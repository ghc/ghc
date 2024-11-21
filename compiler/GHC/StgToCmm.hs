
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
--
-- Stg to C-- code generation
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm ( codeGen ) where

import GHC.Prelude as Prelude

import GHC.StgToCmm.Prof (initCostCentres, ldvEnter)
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Env
import GHC.StgToCmm.Bind
import GHC.StgToCmm.DataCon
import GHC.StgToCmm.Layout
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Closure
import GHC.StgToCmm.Config
import GHC.StgToCmm.Ticky
import GHC.StgToCmm.Types (ModuleLFInfos)

import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.Cmm.Graph

import GHC.Stg.Syntax

import GHC.Types.CostCentre
import GHC.Types.IPE
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.RepType
import GHC.Types.Basic
import GHC.Types.Var.Set ( isEmptyDVarSet )
import GHC.Types.Unique.FM
import GHC.Types.Name.Env

import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.Multiplicity


import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Logger

import GHC.Utils.TmpFs

import GHC.Data.Stream
import GHC.Data.OrdList
import GHC.Types.Unique.Map

import Control.Monad (when,void, forM_)
import GHC.Utils.Misc
import System.IO.Unsafe
import qualified Data.ByteString as BS
import Data.IORef
import GHC.Utils.Panic

codeGen :: Logger
        -> TmpFs
        -> StgToCmmConfig
        -> InfoTableProvMap
        -> [TyCon]
        -> CollectedCCs                -- (Local/global) cost-centres needing declaring/registering.
        -> [CgStgTopBinding]           -- Bindings to convert
        -> Stream IO CmmGroup ModuleLFInfos       -- Output as a stream, so codegen can
                                       -- be interleaved with output
codeGen logger tmpfs cfg (InfoTableProvMap (UniqMap denv) _ _) data_tycons
        cost_centre_info stg_binds
  = do  {     -- cg: run the code generator, and yield the resulting CmmGroup
              -- Using an IORef to store the state is a bit crude, but otherwise
              -- we would need to add a state monad layer which regresses
              -- allocations by 0.5-2%.
        ; cgref <- liftIO $ initC >>= \s -> newIORef s
        ; let cg :: FCode a -> Stream IO CmmGroup a
              cg fcode = do
                (a, cmm) <- liftIO . withTimingSilent logger (text "STG -> Cmm") (`seq` ()) $ do
                         st <- readIORef cgref
                         let fstate = initFCodeState $ stgToCmmPlatform cfg
                         let (a,st') = runC cfg fstate st (getCmm fcode)

                         -- NB. stub-out cgs_tops and cgs_stmts.  This fixes
                         -- a big space leak.  DO NOT REMOVE!
                         -- This is observed by the #3294 test
                         writeIORef cgref $! (st'{ cgs_tops = nilOL, cgs_stmts = mkNop })
                         return a
                yield cmm
                return a

        ; cg (mkModuleInit cost_centre_info)

        ; mapM_ (cg . cgTopBinding logger tmpfs cfg) stg_binds
                -- Put datatype_stuff after code_stuff, because the
                -- datatype closure table (for enumeration types) to
                -- (say) PrelBase_True_closure, which is defined in
                -- code_stuff
        ; let do_tycon tycon = do
                -- Generate a table of static closures for an
                -- enumeration type Note that the closure pointers are
                -- tagged.
                 when (isEnumerationTyCon tycon) $ cg (cgEnumerationTyCon tycon)
                 -- Emit normal info_tables, for data constructors defined in this module.
                 mapM_ (cg . cgDataCon DefinitionSite) (tyConDataCons tycon)

        ; mapM_ do_tycon data_tycons

        -- Emit special info tables for everything used in this module
        -- This will only do something if  `-fdistinct-info-tables` is turned on.
        ; mapM_ (\(dc, ns) -> forM_ ns $ \(k, _ss) -> cg (cgDataCon (UsageSite (stgToCmmThisModule cfg) k) dc)) (nonDetEltsUFM denv)
        ; final_state <- liftIO (readIORef cgref)
        ; let cg_id_infos = cgs_binds final_state

          -- See Note [Conveying CAF-info and LFInfo between modules] in
          -- GHC.StgToCmm.Types
        ; let extractInfo info = (name, lf)
                where
                  !name = idName (cg_id info)
                  !lf = cg_lf info

              !generatedInfo
                | stgToCmmOmitIfPragmas cfg
                = emptyNameEnv
                | otherwise
                = mkNameEnv (Prelude.map extractInfo (nonDetEltsUFM cg_id_infos))

        ; return generatedInfo
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

cgTopBinding :: Logger -> TmpFs -> StgToCmmConfig -> CgStgTopBinding -> FCode ()
cgTopBinding logger tmpfs cfg = \case
    StgTopLifted (StgNonRec id rhs) -> do
        let (info, fcode) = cgTopRhs cfg NonRecursive id rhs
        fcode
        addBindC info

    StgTopLifted (StgRec pairs) -> do
        let (bndrs, rhss) = unzip pairs
        let pairs' = zip bndrs rhss
            r = unzipWith (cgTopRhs cfg Recursive) pairs'
            (infos, fcodes) = unzip r
        addBindsC infos
        sequence_ fcodes

    StgTopStringLit id str -> do
        let label = mkBytesLabel (idName id)
        -- emit either a CmmString literal or dump the string in a file and emit a
        -- CmmFileEmbed literal.  If binary blobs aren't supported,
        -- the threshold in `cfg` will be 0.
        -- See Note [Embedding large binary blobs] in GHC.CmmToAsm.Ppr
        let asString = case stgToCmmBinBlobThresh cfg of
              Just bin_blob_threshold -> fromIntegral (BS.length str) <= bin_blob_threshold
              Nothing                -> True

            (lit,decl) = if asString
              then mkByteStringCLit label str
              else unsafePerformIO $ do
                     bFile <- newTempName logger tmpfs (stgToCmmTmpDir cfg) TFL_CurrentModule ".dat"
                     BS.writeFile bFile str
                     return $ mkFileEmbedLit label bFile (BS.length str)
        emitDecl decl
        addBindC (litIdInfo (stgToCmmPlatform cfg) id mkLFStringLit lit)


cgTopRhs :: StgToCmmConfig -> RecFlag -> Id -> CgStgRhs -> (CgIdInfo, FCode ())
        -- The Id is passed along for setting up a binding...

cgTopRhs cfg _rec bndr (StgRhsCon _cc con mn _ts args _typ)
  = cgTopRhsCon cfg bndr con mn (assertNonVoidStgArgs args)
      -- con args are always non-void,
      -- see Note [Post-unarisation invariants] in GHC.Stg.Unarise

cgTopRhs cfg rec bndr (StgRhsClosure fvs cc upd_flag args body _typ)
  = assertPpr (isEmptyDVarSet fvs) (text "fvs:" <> ppr fvs) $   -- There should be no free variables
    cgTopRhsClosure (stgToCmmPlatform cfg) rec bndr cc upd_flag args body


---------------------------------------------------------------
--      Module initialisation code
---------------------------------------------------------------

mkModuleInit
        :: CollectedCCs         -- cost centre info
        -> FCode ()

mkModuleInit cost_centre_info
  = do  { initCostCentres cost_centre_info
        }


---------------------------------------------------------------
--      Generating static stuff for algebraic data types
---------------------------------------------------------------


cgEnumerationTyCon :: TyCon -> FCode ()
cgEnumerationTyCon tycon
  = do platform <- getPlatform
       emitRODataLits (mkClosureTableLabel (tyConName tycon) NoCafRefs)
             [ CmmLabelOff (mkClosureLabel (dataConName con) NoCafRefs)
                           (tagForCon platform con)
             | con <- tyConDataCons tycon]


cgDataCon :: ConInfoTableLocation -> DataCon -> FCode ()
-- Generate the entry code, info tables, and (for niladic constructor)
-- the static closure, for a constructor.
cgDataCon mn data_con
  = do  { massert (not (isUnboxedTupleDataCon data_con || isUnboxedSumDataCon data_con))
        ; profile <- getProfile
        ; platform <- getPlatform
        ; let
            (tot_wds, --  #ptr_wds + #nonptr_wds
             ptr_wds) --  #ptr_wds
              = mkVirtConstrSizes profile arg_reps

            nonptr_wds   = tot_wds - ptr_wds

            dyn_info_tbl =
              mkDataConInfoTable profile data_con mn False ptr_wds nonptr_wds

            -- We're generating info tables, so we don't know and care about
            -- what the actual arguments are. Using () here as the place holder.
            arg_reps :: [PrimRep]
            arg_reps = [ rep_ty
                       | ty <- dataConRepArgTys data_con
                       , rep_ty <- typePrimRep (scaledThing ty)
                       ]

        ; emitClosureAndInfoTable platform dyn_info_tbl NativeDirectCall [] $
            -- NB: the closure pointer is assumed *untagged* on
            -- entry to a constructor.  If the pointer is tagged,
            -- then we should not be entering it.  This assumption
            -- is used in ldvEnter and when tagging the pointer to
            -- return it.
            -- NB 2: We don't set CC when entering data (WDP 94/06)
            do { tickyEnterDynCon
               ; let node = CmmReg $ nodeReg platform
               ; ldvEnter node
               ; tickyReturnOldCon (length arg_reps)
               ; void $ emitReturn [cmmOffsetB platform node (tagForCon platform data_con)]
               }
                    -- The case continuation code expects a tagged pointer
        }
