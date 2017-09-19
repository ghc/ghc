-- |Compute a description of the generic representation that we use for a user defined data type.
--
-- During vectorisation, we generate a PRepr and PA instance for each user defined
-- data type. The PA dictionary contains methods to convert the user type to and
-- from our generic representation. This module computes a description of what
-- that generic representation is.
--
module Vectorise.Generic.Description
  ( CompRepr(..)
  , ProdRepr(..)
  , ConRepr(..)
  , SumRepr(..)
  , tyConRepr
  , sumReprType
  , compOrigType
  )
where

import GhcPrelude

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
--   If the data type has multiple constructors then we bundle them
--   together into a generic sum type.
data SumRepr
        =  -- | Data type has no data constructors.
           EmptySum

        -- | Data type has a single constructor.
        | UnarySum ConRepr

        -- | Data type has multiple constructors.
        | Sum  { -- | Representation tycon for the sum (eg Sum2)
                 repr_sum_tc    :: TyCon

               -- | PData version of the sum tycon     (eg PDataSum2)
               --   This TyCon doesn't appear explicitly in the source program.
               --   See Note [PData TyCons].
               , repr_psum_tc   :: TyCon

               -- | PDatas version of the sum tycon    (eg PDatasSum2)
               , repr_psums_tc  :: TyCon

               -- | Type of the selector               (eg Sel2)
               , repr_sel_ty    :: Type

               -- | Type of multi-selector             (eg Sel2s)
               , repr_sels_ty   :: Type

               -- | Function to get the length of a Sels of this type.
               , repr_selsLength_v :: CoreExpr

               -- | Type of each data constructor.
               , repr_con_tys   :: [Type]

               -- | Generic representation types of each data constructor.
               , repr_cons      :: [ConRepr]
               }


-- | Describes the representation type of a data constructor.
data ConRepr
        = ConRepr
                { repr_dc       :: DataCon
                , repr_prod     :: ProdRepr
                }

-- | Describes the representation type of the fields \/ components of a constructor.
--   If the data constructor has multiple fields then we bundle them
--   together into a generic product type.
data ProdRepr
        = -- | Data constructor has no fields.
          EmptyProd

        -- | Data constructor has a single field.
        | UnaryProd CompRepr

        -- | Data constructor has several fields.
        | Prod { -- | Representation tycon for the product (eg Tuple2)
                 repr_tup_tc   :: TyCon

                 -- | PData  version of the product tycon  (eg PDataTuple2)
               , repr_ptup_tc  :: TyCon

                 -- | PDatas version of the product tycon  (eg PDatasTuple2s)
                 --   Not all lifted backends use `PDatas`.
               , repr_ptups_tc :: TyCon

                 -- | Types of each field.
               , repr_comp_tys :: [Type]

                 -- | Generic representation types for each field.
               , repr_comps    :: [CompRepr]
               }


-- | Describes the representation type of a data constructor field.
data CompRepr
        = Keep Type
               CoreExpr     -- PR dictionary for the type
        | Wrap Type


-------------------------------------------------------------------------------

-- |Determine the generic representation of a data type, given its tycon.
--
tyConRepr :: TyCon -> VM SumRepr
tyConRepr tc
  = sum_repr (tyConDataCons tc)
  where
    -- Build the representation type for a data type with the given constructors.
    -- The representation types for each individual constructor are bundled
    -- together into a generic sum type.
    sum_repr :: [DataCon] -> VM SumRepr
    sum_repr []    = return EmptySum
    sum_repr [con] = liftM UnarySum (con_repr con)
    sum_repr cons
     = do  let arity    = length cons
           rs           <- mapM con_repr cons
           tys          <- mapM conReprType rs

           -- Get the 'Sum' tycon of this arity (eg Sum2).
           sum_tc       <- builtin (sumTyCon arity)

           -- Get the 'PData' and 'PDatas' tycons for the sum.
           psum_tc      <- pdataReprTyConExact  sum_tc
           psums_tc     <- pdatasReprTyConExact sum_tc

           sel_ty       <- builtin (selTy      arity)
           sels_ty      <- builtin (selsTy     arity)
           selsLength_v <- builtin (selsLength arity)
           return $ Sum
                  { repr_sum_tc         = sum_tc
                  , repr_psum_tc        = psum_tc
                  , repr_psums_tc       = psums_tc
                  , repr_sel_ty         = sel_ty
                  , repr_sels_ty        = sels_ty
                  , repr_selsLength_v   = selsLength_v
                  , repr_con_tys        = tys
                  , repr_cons           = rs
                  }

    -- Build the representation type for a single data constructor.
    con_repr con   = liftM (ConRepr con) (prod_repr (dataConRepArgTys con))

    -- Build the representation type for the fields of a data constructor.
    -- The representation types for each individual field are bundled
    -- together into a generic product type.
    prod_repr :: [Type] -> VM ProdRepr
    prod_repr []   = return EmptyProd
    prod_repr [ty] = liftM UnaryProd (comp_repr ty)
    prod_repr tys
     = do  let arity    = length tys
           rs           <- mapM comp_repr tys
           tys'         <- mapM compReprType rs

           -- Get the Prod \/ Tuple tycon of this arity (eg Tuple2)
           tup_tc       <- builtin (prodTyCon arity)

           -- Get the 'PData' and 'PDatas' tycons for the product.
           ptup_tc      <- pdataReprTyConExact  tup_tc
           ptups_tc     <- pdatasReprTyConExact tup_tc

           return $ Prod
                  { repr_tup_tc   = tup_tc
                  , repr_ptup_tc  = ptup_tc
                  , repr_ptups_tc = ptups_tc
                  , repr_comp_tys = tys'
                  , repr_comps    = rs
                  }

    -- Build the representation type for a single data constructor field.
    comp_repr ty = liftM (Keep ty) (prDictOfReprType ty)
                   `orElseV` return (Wrap ty)

-- |Yield the type of this sum representation.
--
sumReprType :: SumRepr -> VM Type
sumReprType EmptySum     = voidType
sumReprType (UnarySum r) = conReprType r
sumReprType (Sum { repr_sum_tc  = sum_tc, repr_con_tys = tys })
  = return $ mkTyConApp sum_tc tys

-- Yield the type of this constructor representation.
--
conReprType :: ConRepr -> VM Type
conReprType (ConRepr _ r) = prodReprType r

-- Yield the type of of this product representation.
--
prodReprType :: ProdRepr -> VM Type
prodReprType EmptyProd     = voidType
prodReprType (UnaryProd r) = compReprType r
prodReprType (Prod { repr_tup_tc = tup_tc, repr_comp_tys = tys })
  = return $ mkTyConApp tup_tc tys

-- Yield the type of this data constructor field \/ component representation.
--
compReprType :: CompRepr -> VM Type
compReprType (Keep ty _) = return ty
compReprType (Wrap ty)   = mkWrapType ty

-- |Yield the original component type of a data constructor component representation.
--
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

        Sum sumtc psumtc psumstc selty selsty selsLength contys cons
         -> text "Sum" $+$ braces (nest 4
                $ sep   [ text "repr_sum_tc       = " <> ppr sumtc
                        , text "repr_psum_tc      = " <> ppr psumtc
                        , text "repr_psums_tc     = " <> ppr psumstc
                        , text "repr_sel_ty       = " <> ppr selty
                        , text "repr_sels_ty      = " <> ppr selsty
                        , text "repr_selsLength_v = " <> ppr selsLength
                        , text "repr_con_tys      = " <> ppr contys
                        , text "repr_cons         = " <> ppr cons])


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

        Prod tuptcs ptuptcs ptupstcs comptys comps
         -> sep [text "Prod", ppr tuptcs, ppr ptuptcs, ppr ptupstcs, ppr comptys, ppr comps]


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

