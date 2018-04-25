
-- | Build instance tycons for the PData and PDatas type families.
--
--   TODO: the PData and PDatas cases are very similar.
--   We should be able to factor out the common parts.
module Vectorise.Generic.PData
  ( buildPDataTyCon
  , buildPDatasTyCon )
where

import GhcPrelude

import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Generic.Description
import Vectorise.Utils
import Vectorise.Env( GlobalEnv( global_fam_inst_env ) )

import BasicTypes ( SourceText(..) )
import BuildTyCl
import DataCon
import TyCon
import Type
import FamInst
import FamInstEnv
import TcMType
import Name
import Util
import MonadUtils
import Control.Monad


-- buildPDataTyCon ------------------------------------------------------------
-- | Build the PData instance tycon for a given type constructor.
buildPDataTyCon :: TyCon -> TyCon -> SumRepr -> VM FamInst
buildPDataTyCon orig_tc vect_tc repr
 = fixV $ \fam_inst ->
   do let repr_tc = dataFamInstRepTyCon fam_inst
      name' <- mkLocalisedName mkPDataTyConOcc orig_name
      rhs   <- buildPDataTyConRhs orig_name vect_tc repr_tc repr
      pdata <- builtin pdataTyCon
      buildDataFamInst name' pdata vect_tc rhs
 where
    orig_name = tyConName orig_tc

buildDataFamInst :: Name -> TyCon -> TyCon -> AlgTyConRhs -> VM FamInst
buildDataFamInst name' fam_tc vect_tc rhs
 = do { axiom_name <- mkDerivedName mkInstTyCoOcc name'

      ; (_, tyvars') <- liftDs $ freshenTyVarBndrs tyvars
      ; let ax       = mkSingleCoAxiom Representational axiom_name tyvars' [] fam_tc pat_tys rep_ty
            tys'     = mkTyVarTys tyvars'
            rep_ty   = mkTyConApp rep_tc tys'
            pat_tys  = [mkTyConApp vect_tc tys']
            rep_tc   = mkAlgTyCon name'
                           (mkTyConBindersPreferAnon tyvars' liftedTypeKind)
                           liftedTypeKind
                           (map (const Nominal) tyvars')
                           Nothing
                           []          -- no stupid theta
                           rhs
                           (DataFamInstTyCon ax fam_tc pat_tys)
                           False       -- not GADT syntax
      ; liftDs $ newFamInst (DataFamilyInst rep_tc) ax }
 where
    tyvars    = tyConTyVars vect_tc

buildPDataTyConRhs :: Name -> TyCon -> TyCon -> SumRepr -> VM AlgTyConRhs
buildPDataTyConRhs orig_name vect_tc repr_tc repr
 = do data_con <- buildPDataDataCon orig_name vect_tc repr_tc repr
      return $ DataTyCon { data_cons = [data_con], is_enum = False }


buildPDataDataCon :: Name -> TyCon -> TyCon -> SumRepr -> VM DataCon
buildPDataDataCon orig_name vect_tc repr_tc repr
 = do let tvs   = tyConTyVars vect_tc
      dc_name   <- mkLocalisedName mkPDataDataConOcc orig_name
      comp_tys  <- mkSumTys repr_sel_ty mkPDataType repr
      fam_envs  <- readGEnv global_fam_inst_env
      rep_nm    <- liftDs $ newTyConRepName dc_name
      let univ_tvbs = mkTyVarBinders Specified tvs
      liftDs $ buildDataCon fam_envs dc_name
                            False                  -- not infix
                            rep_nm
                            (map (const no_bang) comp_tys)
                            (Just $ map (const HsLazy) comp_tys)
                            []                     -- no field labels
                            tvs
                            []                     -- no existentials
                            univ_tvbs
                            []                     -- no eq spec
                            []                     -- no context
                            comp_tys
                            (mkFamilyTyConApp repr_tc (mkTyVarTys tvs))
                            repr_tc
  where
    no_bang = HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict


-- buildPDatasTyCon -----------------------------------------------------------
-- | Build the PDatas instance tycon for a given type constructor.
buildPDatasTyCon :: TyCon -> TyCon -> SumRepr -> VM FamInst
buildPDatasTyCon orig_tc vect_tc repr
 = fixV $ \fam_inst ->
   do let repr_tc = dataFamInstRepTyCon fam_inst
      name'       <- mkLocalisedName mkPDatasTyConOcc orig_name
      rhs         <- buildPDatasTyConRhs orig_name vect_tc repr_tc repr
      pdatas     <- builtin pdatasTyCon
      buildDataFamInst name' pdatas vect_tc rhs
 where
    orig_name = tyConName orig_tc

buildPDatasTyConRhs :: Name -> TyCon -> TyCon -> SumRepr -> VM AlgTyConRhs
buildPDatasTyConRhs orig_name vect_tc repr_tc repr
 = do data_con <- buildPDatasDataCon orig_name vect_tc repr_tc repr
      return $ DataTyCon { data_cons = [data_con], is_enum = False }


buildPDatasDataCon :: Name -> TyCon -> TyCon -> SumRepr -> VM DataCon
buildPDatasDataCon orig_name vect_tc repr_tc repr
 = do let tvs   = tyConTyVars vect_tc
      dc_name        <- mkLocalisedName mkPDatasDataConOcc orig_name

      comp_tys  <- mkSumTys repr_sels_ty mkPDatasType repr
      fam_envs <- readGEnv global_fam_inst_env
      rep_nm   <- liftDs $ newTyConRepName dc_name
      let univ_tvbs = mkTyVarBinders Specified tvs
      liftDs $ buildDataCon fam_envs dc_name
                            False                  -- not infix
                            rep_nm
                            (map (const no_bang) comp_tys)
                            (Just $ map (const HsLazy) comp_tys)
                            []                     -- no field labels
                            tvs
                            []                     -- no existentials
                            univ_tvbs
                            []                     -- no eq spec
                            []                     -- no context
                            comp_tys
                            (mkFamilyTyConApp repr_tc (mkTyVarTys tvs))
                            repr_tc
  where
     no_bang = HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict


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

{-
mk_fam_inst :: TyCon -> TyCon -> (TyCon, [Type])
mk_fam_inst fam_tc arg_tc
  = (fam_tc, [mkTyConApp arg_tc . mkTyVarTys $ tyConTyVars arg_tc])
-}
