
module Vectorise.Type.PData
	(buildPDataTyCon)
where
import VectUtils
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Type.Repr

import BasicTypes
import BuildTyCl
import DataCon
import TyCon
import Type
import OccName
import Name
import Util
import MonadUtils
import Control.Monad



buildPDataTyCon :: TyCon -> TyCon -> SumRepr -> VM TyCon
buildPDataTyCon orig_tc vect_tc repr = fixV $ \repr_tc ->
  do
    name' <- cloneName mkPDataTyConOcc orig_name
    rhs   <- buildPDataTyConRhs orig_name vect_tc repr_tc repr
    pdata <- builtin pdataTyCon

    liftDs $ buildAlgTyCon name'
                           tyvars
                           []          -- no stupid theta
                           rhs
                           rec_flag    -- FIXME: is this ok?
                           False       -- FIXME: no generics
                           False       -- not GADT syntax
                           (Just $ mk_fam_inst pdata vect_tc)
  where
    orig_name = tyConName orig_tc
    tyvars = tyConTyVars vect_tc
    rec_flag = boolToRecFlag (isRecursiveTyCon vect_tc)


buildPDataTyConRhs :: Name -> TyCon -> TyCon -> SumRepr -> VM AlgTyConRhs
buildPDataTyConRhs orig_name vect_tc repr_tc repr
  = do
      data_con <- buildPDataDataCon orig_name vect_tc repr_tc repr
      return $ DataTyCon { data_cons = [data_con], is_enum = False }

buildPDataDataCon :: Name -> TyCon -> TyCon -> SumRepr -> VM DataCon
buildPDataDataCon orig_name vect_tc repr_tc repr
  = do
      dc_name  <- cloneName mkPDataDataConOcc orig_name
      comp_tys <- sum_tys repr

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
  where
    tvs   = tyConTyVars vect_tc

    sum_tys EmptySum = return []
    sum_tys (UnarySum r) = con_tys r
    sum_tys (Sum { repr_sel_ty = sel_ty
                 , repr_cons   = cons })
      = liftM (sel_ty :) (concatMapM con_tys cons)

    con_tys (ConRepr _ r) = prod_tys r

    prod_tys EmptyProd = return []
    prod_tys (UnaryProd r) = liftM singleton (comp_ty r)
    prod_tys (Prod { repr_comps = comps }) = mapM comp_ty comps

    comp_ty r = mkPDataType (compOrigType r)


mk_fam_inst :: TyCon -> TyCon -> (TyCon, [Type])
mk_fam_inst fam_tc arg_tc
  = (fam_tc, [mkTyConApp arg_tc . mkTyVarTys $ tyConTyVars arg_tc])
