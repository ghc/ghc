
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Generate infotables for interpreter-made bytecodes
module GHC.ByteCode.InfoTable ( mkITbls ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Profile

import GHC.Runtime.Interpreter
import GHCi.RemoteTypes
import qualified GHC.Exts.Heap as Heap

import GHC.Types.Name       ( Name, getName )
import GHC.Types.RepType

import GHC.Core.DataCon     ( DataCon, dataConRepArgTys, dataConIdentity )
import GHC.Core.TyCon       ( TyCon, tyConFamilySize, isDataTyCon, tyConDataCons )
import GHC.Core.Multiplicity     ( scaledThing )

import GHC.StgToCmm.Layout  ( mkVirtConstrSizes )
import GHC.StgToCmm.Closure ( tagForCon )

import GHC.Utils.Misc
import GHC.Utils.Panic

{-
  Manufacturing of info tables for DataCons
-}

-- Make info tables for the data decls in this module
mkITbls :: Profile -> [TyCon] -> [(Name, Message (RemotePtr Heap.StgInfoTable))]
mkITbls profile tcs = concatMap mkITbl (filter isDataTyCon tcs)
 where
  mkITbl :: TyCon -> [(Name, Message (RemotePtr Heap.StgInfoTable))]
  mkITbl tc
    | dcs `lengthIs` n -- paranoia; this is an assertion.
    = make_constr_itbls profile dcs
       where
          dcs = tyConDataCons tc
          n   = tyConFamilySize tc
  mkITbl _ = panic "mkITbl"

-- Assumes constructors are numbered from zero, not one
make_constr_itbls :: Profile -> [DataCon] -> [(Name, Message (RemotePtr Heap.StgInfoTable))]
make_constr_itbls profile cons =
  -- TODO: the profile should be bundled with the interpreter: the rts ways are
  -- fixed for an interpreter
  map (uncurry mk_itbl) (zip cons [0..])
  where
    mk_itbl :: DataCon -> Int -> (Name, Message (RemotePtr Heap.StgInfoTable))
    mk_itbl dcon conNo =
      ( getName dcon,
        MkConInfoTable
          tables_next_to_code
          ptrs'
          nptrs_really
          conNo
          (tagForCon platform dcon)
          descr
      )
      where
         rep_args = [ prim_rep
                    | arg <- dataConRepArgTys dcon
                    , prim_rep <- typePrimRep (scaledThing arg) ]

         (tot_wds, ptr_wds) =
             mkVirtConstrSizes profile rep_args

         ptrs'  = ptr_wds
         nptrs' = tot_wds - ptr_wds
         nptrs_really
            | ptrs' + nptrs' >= pc_MIN_PAYLOAD_SIZE constants = nptrs'
            | otherwise = pc_MIN_PAYLOAD_SIZE constants - ptrs'

         descr = dataConIdentity dcon

         platform = profilePlatform profile
         constants = platformConstants platform
         tables_next_to_code = platformTablesNextToCode platform
