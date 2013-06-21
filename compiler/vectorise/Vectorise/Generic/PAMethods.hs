
-- | Generate methods for the PA class.
--
--   TODO: there is a large amount of redundancy here between the 
--   a, PData a, and PDatas a forms. See if we can factor some of this out.
--
module Vectorise.Generic.PAMethods
  ( buildPReprTyCon
  , buildPAScAndMethods 
  ) where

import Vectorise.Utils
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Generic.Description
import CoreSyn
import CoreUtils
import FamInstEnv
import MkCore            ( mkWildCase )
import TyCon
import CoAxiom
import Type
import OccName
import Coercion
import MkId
import FamInst

import DynFlags
import FastString
import MonadUtils
import Control.Monad
import Outputable


buildPReprTyCon :: TyCon -> TyCon -> SumRepr -> VM FamInst
buildPReprTyCon orig_tc vect_tc repr
 = do name      <- mkLocalisedName mkPReprTyConOcc (tyConName orig_tc)
      rhs_ty    <- sumReprType repr
      prepr_tc  <- builtin preprTyCon
      let axiom = mkSingleCoAxiom name tyvars prepr_tc instTys rhs_ty
      liftDs $ newFamInst SynFamilyInst axiom
  where
    tyvars = tyConTyVars vect_tc
    instTys = [mkTyConApp vect_tc . mkTyVarTys $ tyConTyVars vect_tc]

-- buildPAScAndMethods --------------------------------------------------------

-- | This says how to build the PR superclass and methods of PA
--   Recall the definition of the PA class:
--
--   @
--    class class PR (PRepr a) => PA a where
--      toPRepr       :: a                -> PRepr a
--      fromPRepr     :: PRepr a          -> a
--
--      toArrPRepr    :: PData a          -> PData (PRepr a)
--      fromArrPRepr  :: PData (PRepr a)  -> PData a
--
--      toArrPReprs   :: PDatas a         -> PDatas (PRepr a)
--      fromArrPReprs :: PDatas (PRepr a) -> PDatas a
--   @
--
type PAInstanceBuilder
        =  TyCon        -- ^ Vectorised TyCon 
        -> CoAxiom Unbranched
                        -- ^ Coercion to the representation TyCon
        -> TyCon        -- ^ 'PData'  TyCon
        -> TyCon        -- ^ 'PDatas' TyCon
        -> SumRepr      -- ^ Description of generic representation.
        -> VM CoreExpr  -- ^ Instance function.


buildPAScAndMethods :: VM [(String, PAInstanceBuilder)]
buildPAScAndMethods
 = return [ ("toPRepr",       buildToPRepr)
          , ("fromPRepr",     buildFromPRepr)
          , ("toArrPRepr",    buildToArrPRepr)
          , ("fromArrPRepr",  buildFromArrPRepr)
          , ("toArrPReprs",   buildToArrPReprs)
          , ("fromArrPReprs", buildFromArrPReprs)]


-- buildToPRepr ---------------------------------------------------------------
-- | Build the 'toRepr' method of the PA class.
buildToPRepr :: PAInstanceBuilder
buildToPRepr vect_tc repr_ax _ _ repr
 = do let arg_ty = mkTyConApp vect_tc ty_args

      -- Get the representation type of the argument.
      res_ty <- mkPReprType arg_ty

      -- Var to bind the argument
      arg    <- newLocalVar (fsLit "x") arg_ty

      -- Build the expression to convert the argument to the generic representation.
      result <- to_sum (Var arg) arg_ty res_ty repr

      return $ Lam arg result
  where
    ty_args        = mkTyVarTys (tyConTyVars vect_tc)

    wrap_repr_inst = wrapTypeUnbranchedFamInstBody repr_ax ty_args

    -- CoreExp to convert the given argument to the generic representation.
    -- We start by doing a case branch on the possible data constructors.
    to_sum :: CoreExpr -> Type -> Type -> SumRepr -> VM CoreExpr
    to_sum _ _ _ EmptySum
     = do void <- builtin voidVar
          return $ wrap_repr_inst $ Var void

    to_sum arg arg_ty res_ty (UnarySum r)
     = do (pat, vars, body) <- con_alt r
          return $ mkWildCase arg arg_ty res_ty
                   [(pat, vars, wrap_repr_inst body)]

    to_sum arg arg_ty res_ty (Sum { repr_sum_tc  = sum_tc
                                  , repr_con_tys = tys
                                  , repr_cons    =  cons })
     = do alts <- mapM con_alt cons
          let alts' = [(pat, vars, wrap_repr_inst
                                   $ mkConApp sum_con (map Type tys ++ [body]))
                        | ((pat, vars, body), sum_con)
                            <- zip alts (tyConDataCons sum_tc)]
          return $ mkWildCase arg arg_ty res_ty alts'

    con_alt (ConRepr con r)
     = do (vars, body) <- to_prod r
          return (DataAlt con, vars, body)

    -- CoreExp to convert data constructor fields to the generic representation.
    to_prod :: ProdRepr -> VM ([Var], CoreExpr)
    to_prod EmptyProd
     = do void <- builtin voidVar
          return ([], Var void)

    to_prod (UnaryProd comp)
     = do var  <- newLocalVar (fsLit "x") (compOrigType comp)
          body <- to_comp (Var var) comp
          return ([var], body)

    to_prod (Prod { repr_tup_tc   = tup_tc
                  , repr_comp_tys = tys
                  , repr_comps    = comps })
     = do vars  <- newLocalVars (fsLit "x") (map compOrigType comps)
          exprs <- zipWithM to_comp (map Var vars) comps
          let [tup_con] = tyConDataCons tup_tc
          return (vars, mkConApp tup_con (map Type tys ++ exprs))

    -- CoreExp to convert a data constructor component to the generic representation.
    to_comp :: CoreExpr -> CompRepr -> VM CoreExpr
    to_comp expr (Keep _ _) = return expr
    to_comp expr (Wrap ty)  = wrapNewTypeBodyOfWrap expr ty


-- buildFromPRepr -------------------------------------------------------------

-- |Build the 'fromPRepr' method of the PA class.
--
buildFromPRepr :: PAInstanceBuilder
buildFromPRepr vect_tc repr_ax _ _ repr
  = do
      arg_ty <- mkPReprType res_ty
      arg <- newLocalVar (fsLit "x") arg_ty

      result <- from_sum (unwrapTypeUnbranchedFamInstScrut repr_ax ty_args (Var arg))
                         repr
      return $ Lam arg result
  where
    ty_args = mkTyVarTys (tyConTyVars vect_tc)
    res_ty  = mkTyConApp vect_tc ty_args

    from_sum _ EmptySum
     = do dummy <- builtin fromVoidVar
          return $ Var dummy `App` Type res_ty

    from_sum expr (UnarySum r) = from_con expr r
    from_sum expr (Sum { repr_sum_tc  = sum_tc
                       , repr_con_tys = tys
                       , repr_cons    = cons })
     = do vars  <- newLocalVars (fsLit "x") tys
          es    <- zipWithM from_con (map Var vars) cons
          return $ mkWildCase expr (exprType expr) res_ty
                   [(DataAlt con, [var], e)
                      | (con, var, e) <- zip3 (tyConDataCons sum_tc) vars es]

    from_con expr (ConRepr con r)
      = from_prod expr (mkConApp con $ map Type ty_args) r

    from_prod _ con EmptyProd = return con
    from_prod expr con (UnaryProd r)
     = do e <- from_comp expr r
          return $ con `App` e
     
    from_prod expr con (Prod { repr_tup_tc   = tup_tc
                             , repr_comp_tys = tys
                             , repr_comps    = comps
                             })
     = do vars <- newLocalVars (fsLit "y") tys
          es   <- zipWithM from_comp (map Var vars) comps
          let [tup_con] = tyConDataCons tup_tc
          return $ mkWildCase expr (exprType expr) res_ty
                   [(DataAlt tup_con, vars, con `mkApps` es)]

    from_comp expr (Keep _ _) = return expr
    from_comp expr (Wrap ty)  = unwrapNewTypeBodyOfWrap expr ty


-- buildToArrRepr -------------------------------------------------------------

-- |Build the 'toArrRepr' method of the PA class.
--
buildToArrPRepr :: PAInstanceBuilder
buildToArrPRepr vect_tc repr_co pdata_tc _ r
 = do arg_ty <- mkPDataType el_ty
      res_ty <- mkPDataType =<< mkPReprType el_ty
      arg    <- newLocalVar (fsLit "xs") arg_ty

      pdata_co <- mkBuiltinCo pdataTyCon
      let co           = mkAppCo pdata_co
                       . mkSymCo
                       $ mkUnbranchedAxInstCo repr_co ty_args

          scrut   = unwrapFamInstScrut pdata_tc ty_args (Var arg)

      (vars, result) <- to_sum r

      return . Lam arg
             $ mkWildCase scrut (mkTyConApp pdata_tc ty_args) res_ty
               [(DataAlt pdata_dc, vars, mkCast result co)]
  where
    ty_args    = mkTyVarTys $ tyConTyVars vect_tc
    el_ty      = mkTyConApp vect_tc ty_args
    [pdata_dc] = tyConDataCons pdata_tc

    to_sum ss
     = case ss of
        EmptySum    -> builtin pvoidVar >>= \pvoid -> return ([], Var pvoid) 
        UnarySum r  -> to_con r
        Sum{}
         -> do  let psum_tc     =  repr_psum_tc ss
                let [psum_con]  =  tyConDataCons psum_tc
                (vars, exprs)   <- mapAndUnzipM to_con (repr_cons ss)
                sel             <- newLocalVar (fsLit "sel") (repr_sel_ty ss)
                return ( sel : concat vars
                       , wrapFamInstBody psum_tc (repr_con_tys ss)
                         $ mkConApp psum_con 
                         $ map Type (repr_con_tys ss) ++ (Var sel : exprs))

    to_prod ss
     = case ss of
        EmptyProd    -> builtin pvoidVar >>= \pvoid -> return ([], Var pvoid)
        UnaryProd r
         -> do  pty  <- mkPDataType (compOrigType r)
                var  <- newLocalVar (fsLit "x") pty
                expr <- to_comp (Var var) r
                return ([var], expr)
        Prod{}
         -> do  let [ptup_con]  = tyConDataCons (repr_ptup_tc ss)
                ptys   <- mapM (mkPDataType . compOrigType) (repr_comps ss)
                vars   <- newLocalVars (fsLit "x") ptys
                exprs  <- zipWithM to_comp (map Var vars) (repr_comps ss)
                return ( vars
                       , wrapFamInstBody (repr_ptup_tc ss) (repr_comp_tys ss)
                         $ mkConApp ptup_con
                         $ map Type (repr_comp_tys ss) ++ exprs)

    to_con (ConRepr _ r)    = to_prod r

    to_comp expr (Keep _ _) = return expr
    to_comp expr (Wrap ty)  = wrapNewTypeBodyOfPDataWrap expr ty


-- buildFromArrPRepr ----------------------------------------------------------

-- |Build the 'fromArrPRepr' method for the PA class.
--
buildFromArrPRepr :: PAInstanceBuilder
buildFromArrPRepr vect_tc repr_co pdata_tc _ r
 = do arg_ty <- mkPDataType =<< mkPReprType el_ty
      res_ty <- mkPDataType el_ty
      arg    <- newLocalVar (fsLit "xs") arg_ty

      pdata_co <- mkBuiltinCo pdataTyCon
      let co           = mkAppCo pdata_co
                       $ mkUnbranchedAxInstCo repr_co var_tys

      let scrut        = mkCast (Var arg) co

      let mk_result args
            = wrapFamInstBody pdata_tc var_tys
            $ mkConApp pdata_con
            $ map Type var_tys ++ args

      (expr, _) <- fixV $ \ ~(_, args) ->
                     from_sum res_ty (mk_result args) scrut r

      return $ Lam arg expr
 where
    var_tys     = mkTyVarTys $ tyConTyVars vect_tc
    el_ty       = mkTyConApp vect_tc var_tys
    [pdata_con] = tyConDataCons pdata_tc

    from_sum res_ty res expr ss
     = case ss of
        EmptySum    -> return (res, [])
        UnarySum r  -> from_con res_ty res expr r
        Sum {}
         -> do  let psum_tc    =  repr_psum_tc ss
                let [psum_con] =  tyConDataCons psum_tc
                sel            <- newLocalVar (fsLit "sel") (repr_sel_ty ss)
                ptys           <- mapM mkPDataType (repr_con_tys ss)
                vars           <- newLocalVars (fsLit "xs") ptys
                (res', args)   <- fold from_con res_ty res (map Var vars) (repr_cons ss)
                let scrut      =  unwrapFamInstScrut psum_tc (repr_con_tys ss) expr
                let body       =  mkWildCase scrut (exprType scrut) res_ty
                                    [(DataAlt psum_con, sel : vars, res')]
                return (body, Var sel : args)

    from_prod res_ty res expr ss
     = case ss of
        EmptyProd   -> return (res, [])
        UnaryProd r -> from_comp res_ty res expr r
        Prod {}
         -> do  let ptup_tc    =  repr_ptup_tc ss
                let [ptup_con] =  tyConDataCons ptup_tc
                ptys           <- mapM mkPDataType (repr_comp_tys ss)
                vars           <- newLocalVars (fsLit "ys") ptys
                (res', args)   <- fold from_comp res_ty res (map Var vars) (repr_comps ss)
                let scrut      =  unwrapFamInstScrut ptup_tc (repr_comp_tys ss) expr
                let body       =  mkWildCase scrut (exprType scrut) res_ty
                                    [(DataAlt ptup_con, vars, res')]
                return (body, args)      

    from_con res_ty res expr (ConRepr _ r) = from_prod res_ty res expr r

    from_comp _ res expr (Keep _ _) = return (res, [expr])
    from_comp _ res expr (Wrap ty)  = do { expr' <- unwrapNewTypeBodyOfPDataWrap expr ty
                                         ; return (res, [expr'])
                                         }

    fold f res_ty res exprs rs
      = foldrM f' (res, []) (zip exprs rs)
      where
        f' (expr, r) (res, args) 
         = do (res', args') <- f res_ty res expr r
              return (res', args' ++ args)


-- buildToArrPReprs -----------------------------------------------------------
-- | Build the 'toArrPReprs' instance for the PA class.
--   This converts a PData of elements into the generic representation.
buildToArrPReprs :: PAInstanceBuilder
buildToArrPReprs vect_tc repr_co _ pdatas_tc r
 = do
    -- The argument type of the instance.
    --  eg: 'PDatas (Tree a b)'
    arg_ty    <- mkPDatasType el_ty

    -- The result type. 
    --  eg: 'PDatas (PRepr (Tree a b))'
    res_ty    <- mkPDatasType =<< mkPReprType el_ty

    -- Variable to bind the argument to the instance
    -- eg: (xss :: PDatas (Tree a b))
    varg      <- newLocalVar (fsLit "xss") arg_ty

    -- Coersion to case between the (PRepr a) type and its instance.
    pdatas_co <- mkBuiltinCo pdatasTyCon
    let co           = mkAppCo pdatas_co
                     . mkSymCo
                     $ mkUnbranchedAxInstCo repr_co ty_args

    let scrut        = unwrapFamInstScrut pdatas_tc ty_args (Var varg)
    (vars, result)  <- to_sum r

    return  $ Lam varg
            $ mkWildCase scrut (mkTyConApp pdatas_tc ty_args) res_ty
                    [(DataAlt pdatas_dc, vars, mkCast result co)]

 where
    -- The element type of the argument.
    --  eg: 'Tree a b'.
    ty_args = mkTyVarTys $ tyConTyVars vect_tc
    el_ty   = mkTyConApp vect_tc ty_args
        
    -- PDatas data constructor
    [pdatas_dc] = tyConDataCons pdatas_tc
                  
    to_sum ss
     = case ss of
        -- We can't convert data types with no data.
        -- See Note: [Empty PDatas].
        EmptySum        -> do dflags <- getDynFlags
                              return ([], errorEmptyPDatas dflags el_ty)
        UnarySum r      -> do dflags <- getDynFlags
                              to_con (errorEmptyPDatas dflags el_ty) r

        Sum{}
         -> do  let psums_tc     = repr_psums_tc ss
                let [psums_con]  = tyConDataCons psums_tc
                sels             <- newLocalVar (fsLit "sels") (repr_sels_ty ss)

                -- Take the number of selectors to serve as the length of 
                -- and PDatas Void arrays in the product. See Note [Empty PDatas].
                let xSums        =  App (repr_selsLength_v ss) (Var sels)

                (vars, exprs)    <- mapAndUnzipM (to_con xSums) (repr_cons ss)
                return ( sels : concat vars
                       , wrapFamInstBody psums_tc (repr_con_tys ss)
                         $ mkConApp psums_con 
                         $ map Type (repr_con_tys ss) ++ (Var sels : exprs))        

    to_prod xSums ss
     = case ss of
        EmptyProd    
         -> do  pvoids  <- builtin pvoidsVar
                return ([], App (Var pvoids) xSums )

        UnaryProd r
         -> do  pty  <- mkPDatasType (compOrigType r)
                var  <- newLocalVar (fsLit "x") pty
                expr <- to_comp (Var var) r
                return ([var], expr)

        Prod{}
         -> do  let [ptups_con]  = tyConDataCons (repr_ptups_tc ss)
                ptys   <- mapM (mkPDatasType . compOrigType) (repr_comps ss)
                vars   <- newLocalVars (fsLit "x") ptys
                exprs  <- zipWithM to_comp (map Var vars) (repr_comps ss)
                return ( vars
                       , wrapFamInstBody (repr_ptups_tc ss) (repr_comp_tys ss)
                         $ mkConApp ptups_con
                         $ map Type (repr_comp_tys ss) ++ exprs)

    to_con xSums (ConRepr _ r)
        = to_prod xSums r

    to_comp expr (Keep _ _) = return expr
    to_comp expr (Wrap ty)  = wrapNewTypeBodyOfPDatasWrap expr ty


-- buildFromArrPReprs ---------------------------------------------------------
buildFromArrPReprs :: PAInstanceBuilder
buildFromArrPReprs vect_tc repr_co _ pdatas_tc r
 = do   
    -- The argument type of the instance.
    --  eg: 'PDatas (PRepr (Tree a b))'
    arg_ty      <- mkPDatasType =<< mkPReprType el_ty

    -- The result type. 
    --  eg: 'PDatas (Tree a b)'
    res_ty      <- mkPDatasType el_ty
        
    -- Variable to bind the argument to the instance
    -- eg: (xss :: PDatas (PRepr (Tree a b)))
    varg        <- newLocalVar (fsLit "xss") arg_ty
        
    -- Build the coercion between PRepr and the instance type
    pdatas_co <- mkBuiltinCo pdatasTyCon
    let co           = mkAppCo pdatas_co
                     $ mkUnbranchedAxInstCo repr_co var_tys

    let scrut        = mkCast (Var varg) co

    let mk_result args
            = wrapFamInstBody pdatas_tc var_tys
            $ mkConApp pdatas_con
            $ map Type var_tys ++ args

    (expr, _) <- fixV $ \ ~(_, args) ->
                     from_sum res_ty (mk_result args) scrut r

    return $ Lam varg expr
 where
    -- The element type of the argument.
    --  eg: 'Tree a b'.
    ty_args      = mkTyVarTys $ tyConTyVars vect_tc
    el_ty        = mkTyConApp vect_tc ty_args

    var_tys      = mkTyVarTys $ tyConTyVars vect_tc
    [pdatas_con] = tyConDataCons pdatas_tc

    from_sum res_ty res expr ss
     = case ss of
        -- We can't convert data types with no data.
        -- See Note: [Empty PDatas].
        EmptySum        -> do dflags <- getDynFlags
                              return (res, errorEmptyPDatas dflags el_ty)
        UnarySum r      -> from_con res_ty res expr r

        Sum {}
         -> do  let psums_tc    =  repr_psums_tc ss
                let [psums_con] =  tyConDataCons psums_tc
                sel             <- newLocalVar (fsLit "sels") (repr_sels_ty ss)
                ptys            <- mapM mkPDatasType (repr_con_tys ss)
                vars            <- newLocalVars (fsLit "xs") ptys
                (res', args)    <- fold from_con res_ty res (map Var vars) (repr_cons ss)
                let scrut       =  unwrapFamInstScrut psums_tc (repr_con_tys ss) expr
                let body        =  mkWildCase scrut (exprType scrut) res_ty
                                    [(DataAlt psums_con, sel : vars, res')]
                return (body, Var sel : args)

    from_prod res_ty res expr ss
     = case ss of
        EmptyProd   -> return (res, [])
        UnaryProd r -> from_comp res_ty res expr r
        Prod {}
         -> do  let ptups_tc    =  repr_ptups_tc ss
                let [ptups_con] =  tyConDataCons ptups_tc
                ptys            <- mapM mkPDatasType (repr_comp_tys ss)
                vars            <- newLocalVars (fsLit "ys") ptys
                (res', args)    <- fold from_comp res_ty res (map Var vars) (repr_comps ss)
                let scrut       =  unwrapFamInstScrut ptups_tc (repr_comp_tys ss) expr
                let body        =  mkWildCase scrut (exprType scrut) res_ty
                                    [(DataAlt ptups_con, vars, res')]
                return (body, args)      

    from_con res_ty res expr (ConRepr _ r)
        = from_prod res_ty res expr r

    from_comp _ res expr (Keep _ _) = return (res, [expr])
    from_comp _ res expr (Wrap ty)  = do { expr' <- unwrapNewTypeBodyOfPDatasWrap expr ty
                                         ; return (res, [expr'])
                                         }

    fold f res_ty res exprs rs
      = foldrM f' (res, []) (zip exprs rs)
      where
        f' (expr, r) (res, args) 
         = do (res', args') <- f res_ty res expr r
              return (res', args' ++ args)


-- Notes ----------------------------------------------------------------------
{-
Note [Empty PDatas]
~~~~~~~~~~~~~~~~~~~
We don't support "empty" data types like the following:

  data Empty0
  data Empty1 = MkEmpty1
  data Empty2 = MkEmpty2 Empty0
  ...

There is no parallel data associcated with these types, so there is no where
to store the length of the PDatas array with our standard representation.

Enumerations like the following are ok:
  data Bool = True | False

The native and generic representations are:
  type instance (PDatas Bool)        = VPDs:Bool Sels2
  type instance (PDatas (Repr Bool)) = PSum2s Sels2 (PDatas Void) (PDatas Void)

To take the length of a (PDatas Bool) we take the length of the contained Sels2.
When converting a (PDatas Bool) to a (PDatas (Repr Bool)) we use this length to
initialise the two (PDatas Void) arrays.

However, with this:
  data Empty1 = MkEmpty1
 
The native and generic representations would be:
  type instance (PDatas Empty1)        = VPDs:Empty1
  type instance (PDatas (Repr Empty1)) = PVoids Int
 
The 'Int' argument of PVoids is supposed to store the length of the PDatas 
array. When converting the (PDatas Empty1) to a (PDatas (Repr Empty1)) we
need to come up with a value for it, but there isn't one.

To fix this we'd need to add an Int field to VPDs:Empty1 as well, but that's
too much hassle and there's no point running a parallel computation on no
data anyway.
-}
errorEmptyPDatas :: DynFlags -> Type -> a
errorEmptyPDatas dflags tc
    = cantVectorise dflags "Vectorise.PAMethods"
    $ vcat  [ text "Cannot vectorise data type with no parallel data " <> quotes (ppr tc)
            , text "Data types to be vectorised must contain at least one constructor"
            , text "with at least one field." ]
