
-- | Build instance tycons for the PData and PDatas type families.
--
--   TODO: the PData and PDatas cases are very similar.
--   We should be able to factor out the common parts.
module Vectorise.Generic.PData
  ( buildPDataTyCon
  , buildPDatasTyCon ) 
where

import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Generic.Description
import Vectorise.Utils

import BasicTypes
import BuildTyCl
import DataCon
import TyCon
import Type
import Name
import Util
import MonadUtils
import Control.Monad


-- buildPDataTyCon ------------------------------------------------------------
-- | Build the PData instance tycon for a given type constructor.
buildPDataTyCon :: TyCon -> TyCon -> SumRepr -> VM TyCon
buildPDataTyCon orig_tc vect_tc repr 
 = fixV $ \repr_tc ->
 do name' <- mkLocalisedName mkPDataTyConOcc orig_name
    rhs   <- buildPDataTyConRhs orig_name vect_tc repr_tc repr
    pdata <- builtin pdataTyCon

    liftDs $ buildAlgTyCon name'
                           tyvars
                           []          -- no stupid theta
                           rhs
                           rec_flag    -- FIXME: is this ok?
                           False       -- not GADT syntax
                           NoParentTyCon
                           (Just $ mk_fam_inst pdata vect_tc)
 where
    orig_name = tyConName orig_tc
    tyvars    = tyConTyVars vect_tc
    rec_flag  = boolToRecFlag (isRecursiveTyCon vect_tc)


buildPDataTyConRhs :: Name -> TyCon -> TyCon -> SumRepr -> VM AlgTyConRhs
buildPDataTyConRhs orig_name vect_tc repr_tc repr
 = do data_con <- buildPDataDataCon orig_name vect_tc repr_tc repr
      return $ DataTyCon { data_cons = [data_con], is_enum = False }


buildPDataDataCon :: Name -> TyCon -> TyCon -> SumRepr -> VM DataCon
buildPDataDataCon orig_name vect_tc repr_tc repr
 = do let tvs   = tyConTyVars vect_tc
      dc_name   <- mkLocalisedName mkPDataDataConOcc orig_name
      comp_tys  <- mkSumTys repr_sel_ty mkPDataType repr

      liftDs $ buildDataCon dc_name
                            False                  -- not infix
                            (map (const HsNoBang) comp_tys)
                            []                     -- no field labels
                            tvs
                            []                     -- no existentials
                            []                     -- no eq spec
                            []                     -- no context
                            comp_tys
                            (mkFamilyTyConApp repr_tc (mkTyVarTys tvs))
                            repr_tc


-- buildPDatasTyCon -----------------------------------------------------------
-- | Build the PDatas instance tycon for a given type constructor.
buildPDatasTyCon :: TyCon -> TyCon -> SumRepr -> VM TyCon
buildPDatasTyCon orig_tc vect_tc repr 
 = fixV $ \repr_tc ->
 do name'       <- mkLocalisedName mkPDatasTyConOcc orig_name
    rhs         <- buildPDatasTyConRhs orig_name vect_tc repr_tc repr
    pdatas      <- builtin pdatasTyCon

    liftDs $ buildAlgTyCon name'
                           tyvars
                           []          -- no stupid theta
                           rhs
                           rec_flag    -- FIXME: is this ok?
                           False       -- not GADT syntax
                           NoParentTyCon
                           (Just $ mk_fam_inst pdatas vect_tc)
 where
    orig_name = tyConName   orig_tc
    tyvars    = tyConTyVars vect_tc
    rec_flag  = boolToRecFlag (isRecursiveTyCon vect_tc)


buildPDatasTyConRhs :: Name -> TyCon -> TyCon -> SumRepr -> VM AlgTyConRhs
buildPDatasTyConRhs orig_name vect_tc repr_tc repr
 = do data_con <- buildPDatasDataCon orig_name vect_tc repr_tc repr
      return $ DataTyCon { data_cons = [data_con], is_enum = False }


buildPDatasDataCon :: Name -> TyCon -> TyCon -> SumRepr -> VM DataCon
buildPDatasDataCon orig_name vect_tc repr_tc repr
 = do let tvs   = tyConTyVars vect_tc
      dc_name        <- mkLocalisedName mkPDatasDataConOcc orig_name

      comp_tys  <- mkSumTys repr_sels_ty mkPDatasType repr

      liftDs $ buildDataCon dc_name
                            False                  -- not infix
                            (map (const HsNoBang) comp_tys)
                            []                     -- no field labels
                            tvs
                            []                     -- no existentials
                            []                     -- no eq spec
                            []                     -- no context
                            comp_tys
                            (mkFamilyTyConApp repr_tc (mkTyVarTys tvs))
                            repr_tc


-- Utils ----------------------------------------------------------------------
-- | Flatten a SumRepr into a list of data constructor types.
mkSumTys 
        :: (SumRepr -> Type)
        -> (Type -> VM Type)
        -> SumRepr
        -> VM [Type]

mkSumTys repr_selX_ty mkTc repr
 = sum_tys repr
 where
    sum_tys EmptySum      = return []
    sum_tys (UnarySum r)  = con_tys r
    sum_tys d@(Sum { repr_cons   = cons })
      = liftM (repr_selX_ty d :) (concatMapM con_tys cons)

    con_tys (ConRepr _ r)  = prod_tys r

    prod_tys EmptyProd     = return []
    prod_tys (UnaryProd r) = liftM singleton (comp_ty r)
    prod_tys (Prod { repr_comps = comps }) = mapM comp_ty comps

    comp_ty r = mkTc (compOrigType r)


mk_fam_inst :: TyCon -> TyCon -> (TyCon, [Type])
mk_fam_inst fam_tc arg_tc
  = (fam_tc, [mkTyConApp arg_tc . mkTyVarTys $ tyConTyVars arg_tc])
