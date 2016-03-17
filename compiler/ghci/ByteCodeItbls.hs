{-# LANGUAGE CPP, MagicHash, ScopedTypeVariables #-}
{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | ByteCodeItbls: Generate infotables for interpreter-made bytecodes
module ByteCodeItbls ( mkITbls ) where

#include "HsVersions.h"

import ByteCodeTypes
import GHCi
import DynFlags
import HscTypes
import Name             ( Name, getName )
import NameEnv
import DataCon          ( DataCon, dataConRepArgTys, dataConIdentity )
import TyCon            ( TyCon, tyConFamilySize, isDataTyCon, tyConDataCons )
import Type             ( flattenRepType, repType, typePrimRep )
import StgCmmLayout     ( mkVirtHeapOffsets )
import Util
import Panic

{-
  Manufacturing of info tables for DataCons
-}

-- Make info tables for the data decls in this module
mkITbls :: HscEnv -> [TyCon] -> IO ItblEnv
mkITbls hsc_env tcs =
  foldr plusNameEnv emptyNameEnv <$>
    mapM (mkITbl hsc_env) (filter isDataTyCon tcs)
 where
  mkITbl :: HscEnv -> TyCon -> IO ItblEnv
  mkITbl hsc_env tc
    | dcs `lengthIs` n -- paranoia; this is an assertion.
    = make_constr_itbls hsc_env dcs
       where
          dcs = tyConDataCons tc
          n   = tyConFamilySize tc
  mkITbl _ _ = panic "mkITbl"

mkItblEnv :: [(Name,ItblPtr)] -> ItblEnv
mkItblEnv pairs = mkNameEnv [(n, (n,p)) | (n,p) <- pairs]

-- Assumes constructors are numbered from zero, not one
make_constr_itbls :: HscEnv -> [DataCon] -> IO ItblEnv
make_constr_itbls hsc_env cons =
  mkItblEnv <$> mapM (uncurry mk_itbl) (zip cons [0..])
 where
  dflags = hsc_dflags hsc_env

  mk_itbl :: DataCon -> Int -> IO (Name,ItblPtr)
  mk_itbl dcon conNo = do
     let rep_args = [ (typePrimRep rep_arg,rep_arg)
                    | arg <- dataConRepArgTys dcon
                    , rep_arg <- flattenRepType (repType arg) ]

         (tot_wds, ptr_wds, _, []) =
             mkVirtHeapOffsets dflags False{-not a THUNK-} rep_args 0

         ptrs'  = ptr_wds
         nptrs' = tot_wds - ptr_wds
         nptrs_really
            | ptrs' + nptrs' >= mIN_PAYLOAD_SIZE dflags = nptrs'
            | otherwise = mIN_PAYLOAD_SIZE dflags - ptrs'

         descr = dataConIdentity dcon

     r <- iservCmd hsc_env (MkConInfoTable  ptrs' nptrs_really conNo descr)
     return (getName dcon, ItblPtr r)
