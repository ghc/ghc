{-# LANGUAGE CPP, MagicHash, ScopedTypeVariables #-}
{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Generate infotables for interpreter-made bytecodes
module GHC.ByteCode.InfoTable ( mkITbls ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.ByteCode.Types
import GHC.Runtime.Interpreter
import GHC.Driver.Session
import GHC.Driver.Types
import GHC.Types.Name       ( Name, getName )
import GHC.Types.Name.Env
import GHC.Core.DataCon     ( DataCon, dataConRepArgTys, dataConIdentity )
import GHC.Core.TyCon       ( TyCon, tyConFamilySize, isDataTyCon, tyConDataCons )
import GHC.Types.RepType
import GHC.StgToCmm.Layout  ( mkVirtConstrSizes )
import GHC.StgToCmm.Closure ( tagForCon, NonVoid (..) )
import GHC.Utils.Misc
import GHC.Utils.Panic

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
     let rep_args = [ NonVoid prim_rep
                    | arg <- dataConRepArgTys dcon
                    , prim_rep <- typePrimRep arg ]

         (tot_wds, ptr_wds) =
             mkVirtConstrSizes dflags rep_args

         ptrs'  = ptr_wds
         nptrs' = tot_wds - ptr_wds
         nptrs_really
            | ptrs' + nptrs' >= mIN_PAYLOAD_SIZE dflags = nptrs'
            | otherwise = mIN_PAYLOAD_SIZE dflags - ptrs'

         descr = dataConIdentity dcon

         tables_next_to_code = tablesNextToCode dflags

     r <- iservCmd hsc_env (MkConInfoTable tables_next_to_code ptrs' nptrs_really
                              conNo (tagForCon dflags dcon) descr)
     return (getName dcon, ItblPtr r)
