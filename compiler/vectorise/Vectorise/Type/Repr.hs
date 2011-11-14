
-- | Compute the generic representation type for data types.
module Vectorise.Type.Repr ( 
  CompRepr (..), ProdRepr (..), ConRepr (..), SumRepr (..),
  tyConRepr, sumReprType, conReprType, prodReprType, compReprType, compOrigType
) where

import Vectorise.Utils
import Vectorise.Monad
import Vectorise.Builtins

import CoreSyn
import DataCon
import TyCon
import Type
import Control.Monad
import Outputable


-- | Describes the generic representation of a data type.
data SumRepr 
        =  -- | Data type has no data constructors.
           EmptySum

        -- | Data type has a single constructor.
        | UnarySum ConRepr

        -- | Data type has multiple constructors.
        | Sum  { -- | Representation type for the sum (eg Sum2)
                 repr_sum_tc    :: TyCon

               -- | PData version of the sum TyCon    (eg PDataSum2)
               --   This TyCon doesn't appear explicitly in the source program.
               --   See Note [PData TyCons].
               , repr_psum_tc   :: TyCon

               -- | PDatas version of the sum TyCon   (eg PDatasSum2)
               --   Not all lifted backends use `PDatas`.
               , repr_psums_tc  :: Maybe TyCon

               -- | Type of selector (eg Sel2)
               , repr_sel_ty    :: Type

               -- | Type of each data constructor.
               , repr_con_tys   :: [Type]

               -- | Representation types of each data constructor.
               , repr_cons      :: [ConRepr]
               }

data ConRepr  
        = ConRepr 
                { repr_dc       :: DataCon
                , repr_prod     :: ProdRepr 
                }

data ProdRepr 
        = EmptyProd
        | UnaryProd CompRepr
        | Prod { repr_tup_tc   :: TyCon         -- representation tuple tycon
               , repr_ptup_tc  :: TyCon         -- PData representation tycon
               , repr_comp_tys :: [Type]        -- representation types of
               , repr_comps    :: [CompRepr]    --      components
               }

data CompRepr
        = Keep Type
               CoreExpr     -- PR dictionary for the type
        | Wrap Type


-- | Determine the representation type of a data type constructor.
--
tyConRepr :: TyCon -> VM SumRepr
tyConRepr tc 
  = do  result  <- sum_repr (tyConDataCons tc)
        {-pprTrace "tyConRepr" (ppr result)-} 
        return result

  where
    -- Build the representation type for a data type with the given constructors.
    sum_repr :: [DataCon] -> VM SumRepr
    sum_repr []    = return EmptySum
    sum_repr [con] = liftM UnarySum (con_repr con)
    sum_repr cons  
     = do  let arity    = length cons
           rs           <- mapM con_repr cons
           tys          <- mapM conReprType rs
           sum_tc       <- builtin (sumTyCon arity)
           
           -- Get the 'PData' and 'PDatas' tycons for the sum.
           let sumapp   = mkTyConApp sum_tc tys
           psum_tc      <- liftM fst         $ pdataReprTyCon sumapp
           psums_tc     <- liftM (liftM fst) $ pdatasReprTyCon_maybe sumapp
           
           sel_ty       <- builtin (selTy arity)
           return $ Sum 
                  { repr_sum_tc   = sum_tc
                  , repr_psum_tc  = psum_tc
                  , repr_psums_tc = psums_tc
                  , repr_sel_ty   = sel_ty
                  , repr_con_tys  = tys
                  , repr_cons     = rs
                  }

    con_repr con   = liftM (ConRepr con) (prod_repr (dataConRepArgTys con))

    prod_repr :: [Type] -> VM ProdRepr
    prod_repr []   = return EmptyProd
    prod_repr [ty] = liftM UnaryProd (comp_repr ty)
    prod_repr tys  
     = do  let arity    = length tys
           rs           <- mapM comp_repr tys
           tup_tc       <- builtin (prodTyCon arity)
           tys'         <- mapM compReprType rs
           (ptup_tc, _) <- pdataReprTyCon (mkTyConApp tup_tc tys')
           return $ Prod 
                  { repr_tup_tc   = tup_tc
                  , repr_ptup_tc  = ptup_tc
                  , repr_comp_tys = tys'
                  , repr_comps    = rs
                  }
    
    comp_repr ty = liftM (Keep ty) (prDictOfReprType ty)
                   `orElseV` return (Wrap ty)


sumReprType :: SumRepr -> VM Type
sumReprType EmptySum     = voidType
sumReprType (UnarySum r) = conReprType r
sumReprType (Sum { repr_sum_tc  = sum_tc, repr_con_tys = tys })
  = return $ mkTyConApp sum_tc tys


conReprType :: ConRepr -> VM Type
conReprType (ConRepr _ r) = prodReprType r


prodReprType :: ProdRepr -> VM Type
prodReprType EmptyProd     = voidType
prodReprType (UnaryProd r) = compReprType r
prodReprType (Prod { repr_tup_tc = tup_tc, repr_comp_tys = tys })
  = return $ mkTyConApp tup_tc tys


compReprType :: CompRepr -> VM Type
compReprType (Keep ty _) = return ty
compReprType (Wrap ty)
  = do  wrap_tc <- builtin wrapTyCon
        return $ mkTyConApp wrap_tc [ty]
       

compOrigType :: CompRepr -> Type
compOrigType (Keep ty _) = ty
compOrigType (Wrap ty)   = ty


-- Outputable instances -------------------------------------------------------
instance Outputable SumRepr where
 ppr ss
  = case ss of
        EmptySum
         -> text "EmptySum"

        UnarySum con
         -> sep [text "UnarySum", ppr con]

        Sum sumtc psumtc psumstc selty contys cons
         -> text "Sum" $+$ braces (nest 4 
                $ sep   [ text "repr_sum_tc   = " <> ppr sumtc
                        , text "repr_psum_tc  = " <> ppr psumtc
                        , text "repr_psums_tc = " <> ppr psumstc
                        , text "repr_sel_ty   = " <> ppr selty
                        , text "repr_con_tys  = " <> ppr contys
                        , text "repr_cons     = " <> ppr cons])


instance Outputable ConRepr where
 ppr (ConRepr dc pr)
        = text "ConRepr" $+$ braces (nest 4
                $ sep   [ text "repr_dc      = " <> ppr dc
                        , text "repr_prod    = " <> ppr pr])


instance Outputable ProdRepr where
 ppr ss
  = case ss of
        EmptyProd
         -> text "EmptyProd"
         
        UnaryProd cr
         -> sep [text "UnaryProd", ppr cr]
         
        Prod tuptcs ptuptcs comptys comps
         -> sep [text "Prod", ppr tuptcs, ppr ptuptcs, ppr comptys, ppr comps]


instance Outputable CompRepr where
 ppr ss
  = case ss of
        Keep t ce
         -> text "Keep" $+$ sep [ppr t, ppr ce]
        
        Wrap t
         -> sep [text "Wrap", ppr t]


-- Notes ----------------------------------------------------------------------
{-
Note [PData TyCons]
~~~~~~~~~~~~~~~~~~~
When PData is a type family, the compiler generates a type constructor for each
instance, which is named after the family and instance type. This type
constructor does not appear in the source program. Rather, it is implicitly
defined by the data instance. For example with:

  data family PData a

  data instance PData (Sum2 a b)
        = PSum2  U.Sel2
                 (PData a)
                 (PData b)

The type constructor corresponding to the instance will be named 'PDataSum2',
and this is what we will get in the repr_psum_tc field of SumRepr.Sum.

-}