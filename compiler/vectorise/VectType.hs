module VectType ( vectTyCon, vectType, vectTypeEnv,
                   PAInstance, buildPADict )
where

#include "HsVersions.h"

import VectMonad
import VectUtils
import VectCore

import HscTypes          ( TypeEnv, extendTypeEnvList, typeEnvTyCons )
import CoreSyn
import CoreUtils
import BuildTyCl
import DataCon
import TyCon
import Type
import TypeRep
import Coercion
import FamInstEnv        ( FamInst, mkLocalFamInst )
import InstEnv           ( Instance, mkLocalInstance, instanceDFunId )
import OccName
import MkId
import BasicTypes        ( StrictnessMark(..), OverlapFlag(..), boolToRecFlag )
import Var               ( Var )
import Id                ( mkWildId )
import Name              ( Name, getOccName )
import NameEnv
import TysWiredIn        ( unitTy, unitTyCon, intTy, intDataCon, unitDataConId )
import TysPrim           ( intPrimTy )

import Unique
import UniqFM
import UniqSet
import Digraph           ( SCC(..), stronglyConnComp )

import Outputable

import Control.Monad  ( liftM, liftM2, zipWithM, zipWithM_, mapAndUnzipM )
import Data.List      ( inits, tails, zipWith4, zipWith5 )

-- ----------------------------------------------------------------------------
-- Types

vectTyCon :: TyCon -> VM TyCon
vectTyCon tc
  | isFunTyCon tc        = builtin closureTyCon
  | isBoxedTupleTyCon tc = return tc
  | isUnLiftedTyCon tc   = return tc
  | otherwise = do
                  r <- lookupTyCon tc
                  case r of
                    Just tc' -> return tc'

                    -- FIXME: just for now
                    Nothing  -> pprTrace "ccTyCon:" (ppr tc) $ return tc

vectType :: Type -> VM Type
vectType ty | Just ty' <- coreView ty = vectType ty'
vectType (TyVarTy tv) = return $ TyVarTy tv
vectType (AppTy ty1 ty2) = liftM2 AppTy (vectType ty1) (vectType ty2)
vectType (TyConApp tc tys) = liftM2 TyConApp (vectTyCon tc) (mapM vectType tys)
vectType (FunTy ty1 ty2)   = liftM2 TyConApp (builtin closureTyCon)
                                             (mapM vectType [ty1,ty2])
vectType ty@(ForAllTy _ _)
  = do
      mdicts   <- mapM paDictArgType tyvars
      mono_ty' <- vectType mono_ty
      return $ tyvars `mkForAllTys` ([dict | Just dict <- mdicts] `mkFunTys` mono_ty')
  where
    (tyvars, mono_ty) = splitForAllTys ty

vectType ty = pprPanic "vectType:" (ppr ty)

-- ----------------------------------------------------------------------------
-- Type definitions

type TyConGroup = ([TyCon], UniqSet TyCon)

data PAInstance = PAInstance {
                    painstDFun      :: Var
                  , painstOrigTyCon :: TyCon
                  , painstVectTyCon :: TyCon
                  , painstArrTyCon  :: TyCon
                  }

vectTypeEnv :: TypeEnv -> VM (TypeEnv, [FamInst], [(Var, CoreExpr)])
vectTypeEnv env
  = do
      cs <- readGEnv $ mk_map . global_tycons
      let (conv_tcs, keep_tcs) = classifyTyCons cs groups
          keep_dcs             = concatMap tyConDataCons keep_tcs
      zipWithM_ defTyCon   keep_tcs keep_tcs
      zipWithM_ defDataCon keep_dcs keep_dcs
      new_tcs <- vectTyConDecls conv_tcs

      let orig_tcs = keep_tcs ++ conv_tcs
          vect_tcs  = keep_tcs ++ new_tcs

      repr_tcs <- zipWithM buildPReprTyCon   orig_tcs vect_tcs
      parr_tcs <- zipWithM buildPArrayTyCon orig_tcs vect_tcs
      dfuns    <- mapM mkPADFun vect_tcs
      defTyConPAs (zip vect_tcs dfuns)
      binds    <- sequence (zipWith5 buildTyConBindings orig_tcs
                                                        vect_tcs
                                                        repr_tcs
                                                        parr_tcs
                                                        dfuns)

      let all_new_tcs = new_tcs ++ repr_tcs ++ parr_tcs

      let new_env = extendTypeEnvList env
                       (map ATyCon all_new_tcs
                        ++ [ADataCon dc | tc <- all_new_tcs
                                        , dc <- tyConDataCons tc])

      return (new_env, map mkLocalFamInst (repr_tcs ++ parr_tcs), concat binds)
  where
    tycons = typeEnvTyCons env
    groups = tyConGroups tycons

    mk_map env = listToUFM_Directly [(u, getUnique n /= u) | (u,n) <- nameEnvUniqueElts env]

    keep_tc tc = let dcs = tyConDataCons tc
                 in
                 defTyCon tc tc >> zipWithM_ defDataCon dcs dcs


vectTyConDecls :: [TyCon] -> VM [TyCon]
vectTyConDecls tcs = fixV $ \tcs' ->
  do
    mapM_ (uncurry defTyCon) (lazy_zip tcs tcs')
    mapM vectTyConDecl tcs
  where
    lazy_zip [] _ = []
    lazy_zip (x:xs) ~(y:ys) = (x,y) : lazy_zip xs ys

vectTyConDecl :: TyCon -> VM TyCon
vectTyConDecl tc
  = do
      name' <- cloneName mkVectTyConOcc name
      rhs'  <- vectAlgTyConRhs (algTyConRhs tc)

      liftDs $ buildAlgTyCon name'
                             tyvars
                             []           -- no stupid theta
                             rhs'
                             rec_flag     -- FIXME: is this ok?
                             False        -- FIXME: no generics
                             False        -- not GADT syntax
                             Nothing      -- not a family instance
  where
    name   = tyConName tc
    tyvars = tyConTyVars tc
    rec_flag = boolToRecFlag (isRecursiveTyCon tc)

vectAlgTyConRhs :: AlgTyConRhs -> VM AlgTyConRhs
vectAlgTyConRhs (DataTyCon { data_cons = data_cons
                           , is_enum   = is_enum
                           })
  = do
      data_cons' <- mapM vectDataCon data_cons
      zipWithM_ defDataCon data_cons data_cons'
      return $ DataTyCon { data_cons = data_cons'
                         , is_enum   = is_enum
                         }

vectDataCon :: DataCon -> VM DataCon
vectDataCon dc
  | not . null $ dataConExTyVars dc = pprPanic "vectDataCon: existentials" (ppr dc)
  | not . null $ dataConEqSpec   dc = pprPanic "vectDataCon: eq spec" (ppr dc)
  | otherwise
  = do
      name'    <- cloneName mkVectDataConOcc name
      tycon'   <- vectTyCon tycon
      arg_tys  <- mapM vectType rep_arg_tys

      liftDs $ buildDataCon name'
                            False           -- not infix
                            (map (const NotMarkedStrict) arg_tys)
                            []              -- no labelled fields
                            univ_tvs
                            []              -- no existential tvs for now
                            []              -- no eq spec for now
                            []              -- no context
                            arg_tys
                            tycon'
  where
    name        = dataConName dc
    univ_tvs    = dataConUnivTyVars dc
    rep_arg_tys = dataConRepArgTys dc
    tycon       = dataConTyCon dc

mk_fam_inst :: TyCon -> TyCon -> (TyCon, [Type])
mk_fam_inst fam_tc arg_tc
  = (fam_tc, [mkTyConApp arg_tc . mkTyVarTys $ tyConTyVars arg_tc])

buildPReprTyCon :: TyCon -> TyCon -> VM TyCon
buildPReprTyCon orig_tc vect_tc
  = do
      name     <- cloneName mkPReprTyConOcc (tyConName orig_tc)
      rhs_ty   <- buildPReprType vect_tc
      prepr_tc <- builtin preprTyCon
      liftDs $ buildSynTyCon name
                             tyvars
                             (SynonymTyCon rhs_ty)
                             (Just $ mk_fam_inst prepr_tc vect_tc)
  where
    tyvars = tyConTyVars vect_tc


data Repr = ProdRepr {
              prod_components   :: [Type]
            , prod_tycon        :: TyCon
            , prod_data_con     :: DataCon
            , prod_arr_tycon    :: TyCon
            , prod_arr_data_con :: DataCon
            }

          | SumRepr {
              sum_components    :: [Repr]
            , sum_tycon         :: TyCon
            , sum_arr_tycon     :: TyCon
            , sum_arr_data_con  :: DataCon
            }

          | IdRepr Type

          | VoidRepr {
              void_tycon        :: TyCon
            , void_bottom       :: CoreExpr
            }

mkVoid :: VM Repr
mkVoid = do
           tycon <- builtin voidTyCon
           var   <- builtin voidVar
           return $ VoidRepr {
                      void_tycon  = tycon
                    , void_bottom = Var var
                    }

mkProduct :: [Type] -> VM Repr
mkProduct tys
  = do
      tycon <- builtin (prodTyCon arity)
      let [data_con] = tyConDataCons tycon

      (arr_tycon, _) <- parrayReprTyCon $ mkTyConApp tycon tys
      let [arr_data_con] = tyConDataCons arr_tycon

      return $ ProdRepr {
                 prod_components   = tys
               , prod_tycon        = tycon
               , prod_data_con     = data_con
               , prod_arr_tycon    = arr_tycon
               , prod_arr_data_con = arr_data_con
               }
  where
    arity = length tys

mkSubProduct :: [Type] -> VM Repr
mkSubProduct []   = mkVoid
mkSubProduct [ty] = return $ IdRepr ty
mkSubProduct tys  = mkProduct tys

mkSum :: [Repr] -> VM Repr
mkSum [repr] = return repr
mkSum reprs
  = do
      tycon <- builtin (sumTyCon arity)
      (arr_tycon, _) <- parrayReprTyCon
                      . mkTyConApp tycon
                      $ map reprType reprs

      let [arr_data_con] = tyConDataCons arr_tycon

      return $ SumRepr {
                 sum_components   = reprs
               , sum_tycon        = tycon
               , sum_arr_tycon    = arr_tycon
               , sum_arr_data_con = arr_data_con
               }
  where
    arity = length reprs

reprType :: Repr -> Type
reprType (ProdRepr { prod_tycon = tycon, prod_components = tys })
  = mkTyConApp tycon tys
reprType (SumRepr { sum_tycon = tycon, sum_components = reprs })
  = mkTyConApp tycon (map reprType reprs)
reprType (IdRepr ty) = ty
reprType (VoidRepr { void_tycon = tycon }) = mkTyConApp tycon []

arrReprType :: Repr -> VM Type
arrReprType = mkPArrayType . reprType

arrShapeTys :: Repr -> VM [Type]
arrShapeTys (SumRepr  {})
  = do
      int_arr <- builtin parrayIntPrimTyCon
      return [intPrimTy, mkTyConApp int_arr [], mkTyConApp int_arr []]
arrShapeTys (ProdRepr {}) = return [intPrimTy]
arrShapeTys (IdRepr _)    = return []
arrShapeTys (VoidRepr {}) = return [intPrimTy]

arrShapeVars :: Repr -> VM [Var]
arrShapeVars repr = mapM (newLocalVar FSLIT("sh")) =<< arrShapeTys repr

replicateShape :: Repr -> CoreExpr -> CoreExpr -> VM [CoreExpr]
replicateShape (ProdRepr {}) len _ = return [len]
replicateShape (SumRepr {})  len tag
  = do
      rep <- builtin replicatePAIntPrimVar
      up  <- builtin upToPAIntPrimVar
      return [len, Var rep `mkApps` [len, tag], Var up `App` len]
replicateShape (IdRepr _) _ _ = return []
replicateShape (VoidRepr {}) len _ = return [len]

arrReprElemTys :: Repr -> VM [[Type]]
arrReprElemTys (SumRepr { sum_components = prods })
  = mapM arrProdElemTys prods
arrReprElemTys prod@(ProdRepr {})
  = do
      tys <- arrProdElemTys prod
      return [tys]
arrReprElemTys (IdRepr ty) = return [[ty]]
arrReprElemTys (VoidRepr { void_tycon = tycon })
  = return [[mkTyConApp tycon []]]

arrProdElemTys (ProdRepr { prod_components = [] })
  = do
      void <- builtin voidTyCon
      return [mkTyConApp void []]
arrProdElemTys (ProdRepr { prod_components = tys })
  = return tys
arrProdElemTys (IdRepr ty) = return [ty]
arrProdElemTys (VoidRepr { void_tycon = tycon })
  = return [mkTyConApp tycon []]

arrReprTys :: Repr -> VM [[Type]]
arrReprTys repr = mapM (mapM mkPArrayType) =<< arrReprElemTys repr

arrReprVars :: Repr -> VM [[Var]]
arrReprVars repr
  = mapM (mapM (newLocalVar FSLIT("rs"))) =<< arrReprTys repr

mkRepr :: TyCon -> VM Repr
mkRepr vect_tc
  | [tys] <- rep_tys = mkProduct tys
  | otherwise        = mkSum =<< mapM mkSubProduct rep_tys
  where
    rep_tys = map dataConRepArgTys $ tyConDataCons vect_tc

buildPReprType :: TyCon -> VM Type
buildPReprType = liftM reprType . mkRepr

buildToPRepr :: Repr -> TyCon -> TyCon -> TyCon -> VM CoreExpr
buildToPRepr repr vect_tc prepr_tc _
  = do
      arg    <- newLocalVar FSLIT("x") arg_ty
      result <- to_repr repr (Var arg)

      return . Lam arg
             . wrapFamInstBody prepr_tc var_tys
             $ result
  where
    var_tys = mkTyVarTys $ tyConTyVars vect_tc
    arg_ty  = mkTyConApp vect_tc var_tys
    res_ty  = reprType repr

    cons    = tyConDataCons vect_tc
    [con]   = cons

    to_repr (SumRepr { sum_components = prods
                     , sum_tycon      = tycon })
            expr
      = do
          (vars, bodies) <- mapAndUnzipM prod_alt prods
          return . Case expr (mkWildId (exprType expr)) res_ty
                 $ zipWith4 mk_alt cons vars (tyConDataCons tycon) bodies
      where
        mk_alt con vars sum_con body
          = (DataAlt con, vars, mkConApp sum_con (ty_args ++ [body]))

        ty_args = map (Type . reprType) prods

    to_repr prod expr
      = do
          (vars, body) <- prod_alt prod
          return $ Case expr (mkWildId (exprType expr)) res_ty
                   [(DataAlt con, vars, body)]

    prod_alt (ProdRepr { prod_components = tys
                       , prod_data_con   = data_con })
      = do
          vars <- mapM (newLocalVar FSLIT("r")) tys
          return (vars, mkConApp data_con (map Type tys ++ map Var vars))

    prod_alt (IdRepr ty)
      = do
          var <- newLocalVar FSLIT("y") ty
          return ([var], Var var)

    prod_alt (VoidRepr { void_bottom = bottom })
      = return ([], bottom)


buildFromPRepr :: Repr -> TyCon -> TyCon -> TyCon -> VM CoreExpr
buildFromPRepr repr vect_tc prepr_tc _
  = do
      arg_ty <- mkPReprType res_ty
      arg    <- newLocalVar FSLIT("x") arg_ty

      liftM (Lam arg)
           . from_repr repr
           $ unwrapFamInstScrut prepr_tc var_tys (Var arg)
  where
    var_tys = mkTyVarTys $ tyConTyVars vect_tc
    res_ty  = mkTyConApp vect_tc var_tys

    cons    = map (`mkConApp` map Type var_tys) (tyConDataCons vect_tc)
    [con]   = cons

    from_repr repr@(SumRepr { sum_components = prods
                            , sum_tycon      = tycon })
              expr
      = do
          vars   <- mapM (newLocalVar FSLIT("x")) (map reprType prods)
          bodies <- sequence . zipWith3 from_prod prods cons
                             $ map Var vars
          return . Case expr (mkWildId (reprType repr)) res_ty
                 $ zipWith3 sum_alt (tyConDataCons tycon) vars bodies
      where
        sum_alt data_con var body = (DataAlt data_con, [var], body)

    from_repr repr expr = from_prod repr con expr

    from_prod prod@(ProdRepr { prod_components = tys
                             , prod_data_con   = data_con })
              con
              expr
      = do
          vars <- mapM (newLocalVar FSLIT("y")) tys
          return $ Case expr (mkWildId (reprType prod)) res_ty
                   [(DataAlt data_con, vars, con `mkVarApps` vars)]

    from_prod (IdRepr _) con expr
       = return $ con `App` expr

    from_prod (VoidRepr {}) con expr
       = return con

buildToArrPRepr :: Repr -> TyCon -> TyCon -> TyCon -> VM CoreExpr
buildToArrPRepr repr vect_tc prepr_tc arr_tc
  = do
      arg_ty     <- mkPArrayType el_ty
      arg        <- newLocalVar FSLIT("xs") arg_ty

      res_ty     <- mkPArrayType (reprType repr)

      shape_vars <- arrShapeVars repr
      repr_vars  <- arrReprVars  repr

      parray_co  <- mkBuiltinCo parrayTyCon

      let Just repr_co = tyConFamilyCoercion_maybe prepr_tc
          co           = mkAppCoercion parray_co
                       . mkSymCoercion
                       $ mkTyConApp repr_co var_tys

          scrut   = unwrapFamInstScrut arr_tc var_tys (Var arg)

      result <- to_repr shape_vars repr_vars repr

      return . Lam arg
             . mkCoerce co
             $ Case scrut (mkWildId (mkTyConApp arr_tc var_tys)) res_ty
               [(DataAlt arr_dc, shape_vars ++ concat repr_vars, result)]
  where
    var_tys = mkTyVarTys $ tyConTyVars vect_tc
    el_ty   = mkTyConApp vect_tc var_tys

    [arr_dc] = tyConDataCons arr_tc

    to_repr shape_vars@(len_var : _)
            repr_vars
            (SumRepr { sum_components   = prods
                     , sum_arr_tycon    = tycon
                     , sum_arr_data_con = data_con })
      = do
          exprs <- zipWithM to_prod repr_vars prods

          return . wrapFamInstBody tycon tys
                 . mkConApp data_con
                 $ map Type tys ++ map Var shape_vars ++ exprs
      where
        tys = map reprType prods

    to_repr [len_var]
            [repr_vars]
            (ProdRepr { prod_components   = tys
                      , prod_arr_tycon    = tycon
                      , prod_arr_data_con = data_con })
       = return . wrapFamInstBody tycon tys
                . mkConApp data_con
                $ map Type tys ++ map Var (len_var : repr_vars)

    to_prod repr_vars@(r : _)
            (ProdRepr { prod_components   = tys
                      , prod_arr_tycon    = tycon
                      , prod_arr_data_con = data_con })
      = do
          len <- lengthPA (Var r)
          return . wrapFamInstBody tycon tys
                 . mkConApp data_con
                 $ map Type tys ++ len : map Var repr_vars

    to_prod [var] (IdRepr ty)   = return (Var var)
    to_prod [var] (VoidRepr {}) = return (Var var)


buildFromArrPRepr :: Repr -> TyCon -> TyCon -> TyCon -> VM CoreExpr
buildFromArrPRepr repr vect_tc prepr_tc arr_tc
  = do
      arg_ty     <- mkPArrayType =<< mkPReprType el_ty
      arg        <- newLocalVar FSLIT("xs") arg_ty

      res_ty     <- mkPArrayType el_ty

      shape_vars <- arrShapeVars repr
      repr_vars  <- arrReprVars  repr

      parray_co  <- mkBuiltinCo parrayTyCon

      let Just repr_co = tyConFamilyCoercion_maybe prepr_tc
          co           = mkAppCoercion parray_co
                       $ mkTyConApp repr_co var_tys

          scrut  = mkCoerce co (Var arg)

          result = wrapFamInstBody arr_tc var_tys
                 . mkConApp arr_dc
                 $ map Type var_tys ++ map Var (shape_vars ++ concat repr_vars)

      liftM (Lam arg)
            (from_repr repr scrut shape_vars repr_vars res_ty result)
  where
    var_tys = mkTyVarTys $ tyConTyVars vect_tc
    el_ty   = mkTyConApp vect_tc var_tys

    [arr_dc] = tyConDataCons arr_tc

    from_repr (SumRepr { sum_components   = prods
                       , sum_arr_tycon    = tycon
                       , sum_arr_data_con = data_con })
              expr
              shape_vars
              repr_vars
              res_ty
              body
      = do
          vars <- mapM (newLocalVar FSLIT("xs")) =<< mapM arrReprType prods
          result <- go prods repr_vars vars body

          let scrut = unwrapFamInstScrut tycon ty_args expr
          return . Case scrut (mkWildId scrut_ty) res_ty
                 $ [(DataAlt data_con, shape_vars ++ vars, result)]
      where
        ty_args  = map reprType prods
        scrut_ty = mkTyConApp tycon ty_args

        go [] [] [] body = return body
        go (prod : prods) (repr_vars : rss) (var : vars) body
          = do
              shape_vars <- mapM (newLocalVar FSLIT("s")) =<< arrShapeTys prod

              from_prod prod (Var var) shape_vars repr_vars res_ty
                =<< go prods rss vars body

    from_repr repr expr shape_vars [repr_vars] res_ty body
      = from_prod repr expr shape_vars repr_vars res_ty body

    from_prod prod@(ProdRepr { prod_components = tys
                             , prod_arr_tycon  = tycon
                             , prod_arr_data_con = data_con })
              expr
              shape_vars
              repr_vars
              res_ty
              body
      = do
          let scrut    = unwrapFamInstScrut tycon tys expr
              scrut_ty = mkTyConApp tycon tys
          ty <- arrReprType prod

          return $ Case scrut (mkWildId scrut_ty) res_ty
                   [(DataAlt data_con, shape_vars ++ repr_vars, body)]

    from_prod (IdRepr ty)
              expr
              shape_vars
              [repr_var]
              res_ty
              body
      = return $ Let (NonRec repr_var expr) body

    from_prod (VoidRepr {})
              expr
              shape_vars
              [repr_var]
              res_ty
              body
      = return $ Let (NonRec repr_var expr) body

buildPRDictRepr :: Repr -> VM CoreExpr
buildPRDictRepr (VoidRepr { void_tycon = tycon })
  = prDFunOfTyCon tycon
buildPRDictRepr (IdRepr ty) = mkPR ty
buildPRDictRepr (ProdRepr {
                   prod_components = tys
                 , prod_tycon      = tycon
                 })
  = do
      prs  <- mapM mkPR tys
      dfun <- prDFunOfTyCon tycon
      return $ dfun `mkTyApps` tys `mkApps` prs

buildPRDictRepr (SumRepr {
                   sum_components = prods
                 , sum_tycon      = tycon })
  = do
      prs  <- mapM buildPRDictRepr prods
      dfun <- prDFunOfTyCon tycon
      return $ dfun `mkTyApps` map reprType prods `mkApps` prs

buildPRDict :: Repr -> TyCon -> TyCon -> TyCon -> VM CoreExpr
buildPRDict repr vect_tc prepr_tc _
  = do
      dict  <- buildPRDictRepr repr

      pr_co <- mkBuiltinCo prTyCon
      let co = mkAppCoercion pr_co
             . mkSymCoercion
             $ mkTyConApp arg_co var_tys

      return $ mkCoerce co dict
  where
    var_tys = mkTyVarTys $ tyConTyVars vect_tc

    Just arg_co = tyConFamilyCoercion_maybe prepr_tc

buildPArrayTyCon :: TyCon -> TyCon -> VM TyCon
buildPArrayTyCon orig_tc vect_tc = fixV $ \repr_tc ->
  do
    name'  <- cloneName mkPArrayTyConOcc orig_name
    rhs    <- buildPArrayTyConRhs orig_name vect_tc repr_tc
    parray <- builtin parrayTyCon

    liftDs $ buildAlgTyCon name'
                           tyvars
                           []          -- no stupid theta
                           rhs
                           rec_flag    -- FIXME: is this ok?
                           False       -- FIXME: no generics
                           False       -- not GADT syntax
                           (Just $ mk_fam_inst parray vect_tc)
  where
    orig_name = tyConName orig_tc
    tyvars = tyConTyVars vect_tc
    rec_flag = boolToRecFlag (isRecursiveTyCon vect_tc)


buildPArrayTyConRhs :: Name -> TyCon -> TyCon -> VM AlgTyConRhs
buildPArrayTyConRhs orig_name vect_tc repr_tc
  = do
      data_con <- buildPArrayDataCon orig_name vect_tc repr_tc
      return $ DataTyCon { data_cons = [data_con], is_enum = False }

buildPArrayDataCon :: Name -> TyCon -> TyCon -> VM DataCon
buildPArrayDataCon orig_name vect_tc repr_tc
  = do
      dc_name  <- cloneName mkPArrayDataConOcc orig_name
      repr     <- mkRepr vect_tc

      shape_tys <- arrShapeTys repr
      repr_tys  <- arrReprTys  repr

      let tys = shape_tys ++ concat repr_tys

      liftDs $ buildDataCon dc_name
                            False                  -- not infix
                            (map (const NotMarkedStrict) tys)
                            []                     -- no field labels
                            (tyConTyVars vect_tc)
                            []                     -- no existentials
                            []                     -- no eq spec
                            []                     -- no context
                            tys
                            repr_tc

mkPADFun :: TyCon -> VM Var
mkPADFun vect_tc
  = newExportedVar (mkPADFunOcc $ getOccName vect_tc) =<< paDFunType vect_tc

buildTyConBindings :: TyCon -> TyCon -> TyCon -> TyCon -> Var
                   -> VM [(Var, CoreExpr)]
buildTyConBindings orig_tc vect_tc prepr_tc arr_tc dfun
  = do
      repr  <- mkRepr vect_tc
      vectDataConWorkers repr orig_tc vect_tc arr_tc
      dict <- buildPADict repr vect_tc prepr_tc arr_tc dfun
      binds <- takeHoisted
      return $ (dfun, dict) : binds
  where
    orig_dcs = tyConDataCons orig_tc
    vect_dcs = tyConDataCons vect_tc
    [arr_dc] = tyConDataCons arr_tc

    repr_tys = map dataConRepArgTys vect_dcs

vectDataConWorkers :: Repr -> TyCon -> TyCon -> TyCon
                   -> VM ()
vectDataConWorkers repr orig_tc vect_tc arr_tc
  = do
      arr_tys <- arrReprElemTys repr
      bs <- sequence
          . zipWith3 def_worker  (tyConDataCons orig_tc) rep_tys
          $ zipWith4 mk_data_con (tyConDataCons vect_tc)
                                 rep_tys
                                 (inits arr_tys)
                                 (tail $ tails arr_tys)
      mapM_ (uncurry hoistBinding) bs
  where
    tyvars   = tyConTyVars vect_tc
    var_tys  = mkTyVarTys tyvars
    ty_args  = map Type var_tys

    res_ty   = mkTyConApp vect_tc var_tys

    rep_tys  = map dataConRepArgTys $ tyConDataCons vect_tc

    [arr_dc] = tyConDataCons arr_tc

    mk_data_con con tys pre post
      = liftM2 (,) (vect_data_con con)
                   (lift_data_con tys (concat pre)
                                      (concat post)
                                      (mkDataConTag con))

    vect_data_con con = return $ mkConApp con ty_args
    lift_data_con tys pre_tys post_tys tag
      = do
          len  <- builtin liftingContext
          args <- mapM (newLocalVar FSLIT("xs"))
                  =<< mapM mkPArrayType tys

          shape <- replicateShape repr (Var len) tag
          repr  <- mk_arr_repr (Var len) (map Var args)

          pre   <- mapM emptyPA pre_tys
          post  <- mapM emptyPA post_tys

          return . mkLams (len : args)
                 . wrapFamInstBody arr_tc var_tys
                 . mkConApp arr_dc
                 $ ty_args ++ shape ++ pre ++ repr ++ post

    mk_arr_repr len []
      = do
          units <- replicatePA len (Var unitDataConId)
          return [units]

    mk_arr_repr len arrs = return arrs

    def_worker data_con arg_tys mk_body
      = do
          body <- closedV
                . inBind orig_worker
                . polyAbstract tyvars $ \abstract ->
                  liftM (abstract . vectorised)
                $ buildClosures tyvars [] arg_tys res_ty mk_body

          vect_worker <- cloneId mkVectOcc orig_worker (exprType body)
          defGlobalVar orig_worker vect_worker
          return (vect_worker, body)
      where
        orig_worker = dataConWorkId data_con

buildPADict :: Repr -> TyCon -> TyCon -> TyCon -> Var -> VM CoreExpr
buildPADict repr vect_tc prepr_tc arr_tc dfun
  = polyAbstract tvs $ \abstract ->
    do
      meth_binds <- mapM (mk_method repr) paMethods
      let meth_exprs = map (Var . fst) meth_binds

      pa_dc <- builtin paDataCon
      let dict = mkConApp pa_dc (Type (mkTyConApp vect_tc arg_tys) : meth_exprs)
          body = Let (Rec meth_binds) dict
      return . mkInlineMe $ abstract body
  where
    tvs = tyConTyVars arr_tc
    arg_tys = mkTyVarTys tvs

    mk_method repr (name, build)
      = localV
      $ do
          body <- build repr vect_tc prepr_tc arr_tc
          var  <- newLocalVar name (exprType body)
          return (var, mkInlineMe body)

paMethods = [(FSLIT("toPRepr"),      buildToPRepr),
             (FSLIT("fromPRepr"),    buildFromPRepr),
             (FSLIT("toArrPRepr"),   buildToArrPRepr),
             (FSLIT("fromArrPRepr"), buildFromArrPRepr),
             (FSLIT("dictPRepr"),    buildPRDict)]

-- | Split the given tycons into two sets depending on whether they have to be
-- converted (first list) or not (second list). The first argument contains
-- information about the conversion status of external tycons:
--
--   * tycons which have converted versions are mapped to True
--   * tycons which are not changed by vectorisation are mapped to False
--   * tycons which can't be converted are not elements of the map
--
classifyTyCons :: UniqFM Bool -> [TyConGroup] -> ([TyCon], [TyCon])
classifyTyCons = classify [] []
  where
    classify conv keep cs [] = (conv, keep)
    classify conv keep cs ((tcs, ds) : rs)
      | can_convert && must_convert
        = classify (tcs ++ conv) keep (cs `addListToUFM` [(tc,True) | tc <- tcs]) rs
      | can_convert
        = classify conv (tcs ++ keep) (cs `addListToUFM` [(tc,False) | tc <- tcs]) rs
      | otherwise
        = classify conv keep cs rs
      where
        refs = ds `delListFromUniqSet` tcs

        can_convert  = isNullUFM (refs `minusUFM` cs) && all convertable tcs
        must_convert = foldUFM (||) False (intersectUFM_C const cs refs)

        convertable tc = isDataTyCon tc && all isVanillaDataCon (tyConDataCons tc)

-- | Compute mutually recursive groups of tycons in topological order
--
tyConGroups :: [TyCon] -> [TyConGroup]
tyConGroups tcs = map mk_grp (stronglyConnComp edges)
  where
    edges = [((tc, ds), tc, uniqSetToList ds) | tc <- tcs
                                , let ds = tyConsOfTyCon tc]

    mk_grp (AcyclicSCC (tc, ds)) = ([tc], ds)
    mk_grp (CyclicSCC els)       = (tcs, unionManyUniqSets dss)
      where
        (tcs, dss) = unzip els

tyConsOfTyCon :: TyCon -> UniqSet TyCon
tyConsOfTyCon
  = tyConsOfTypes . concatMap dataConRepArgTys . tyConDataCons

tyConsOfType :: Type -> UniqSet TyCon
tyConsOfType ty
  | Just ty' <- coreView ty    = tyConsOfType ty'
tyConsOfType (TyVarTy v)       = emptyUniqSet
tyConsOfType (TyConApp tc tys) = extend (tyConsOfTypes tys)
  where
    extend | isUnLiftedTyCon tc
           || isTupleTyCon   tc = id

           | otherwise          = (`addOneToUniqSet` tc)

tyConsOfType (AppTy a b)       = tyConsOfType a `unionUniqSets` tyConsOfType b
tyConsOfType (FunTy a b)       = (tyConsOfType a `unionUniqSets` tyConsOfType b)
                                 `addOneToUniqSet` funTyCon
tyConsOfType (ForAllTy _ ty)   = tyConsOfType ty
tyConsOfType other             = pprPanic "ClosureConv.tyConsOfType" $ ppr other

tyConsOfTypes :: [Type] -> UniqSet TyCon
tyConsOfTypes = unionManyUniqSets . map tyConsOfType

