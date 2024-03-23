{-# LANGUAGE TypeFamilies #-}
module GHC.Iface.Type.Map where

import GHC.Prelude
import GHC.Data.TrieMap
import GHC.Iface.Type
import qualified Data.Map as Map
import Data.Functor.Compose
import GHC.Types.Basic
import Control.Monad ((>=>))
import GHC.Types.Unique.DFM
import Data.Functor.Product
import GHC.Types.Var (VarBndr(..))
import GHC.Utils.Binary


newtype IfaceTypeMap a = IfaceTypeMap (IfaceTypeMapG a)

instance Functor IfaceTypeMap where
  fmap f (IfaceTypeMap m) = IfaceTypeMap (fmap f m)

instance TrieMap IfaceTypeMap where
  type Key IfaceTypeMap = IfaceType

  emptyTM = IfaceTypeMap emptyTM

  lookupTM k (IfaceTypeMap m) = lookupTM k m

  alterTM k f (IfaceTypeMap m) = IfaceTypeMap (alterTM k f m)

  filterTM f (IfaceTypeMap m) = IfaceTypeMap (filterTM f m)

  foldTM f (IfaceTypeMap m) = foldTM f m

type IfaceTypeMapG = GenMap IfaceTypeMapX

data IfaceTypeMapX a
  = IFM { ifm_lit :: IfaceLiteralMap a
        , ifm_var :: UniqDFM IfLclName a
        , ifm_app :: IfaceTypeMapG (IfaceAppArgsMap a)
        , ifm_fun_ty :: FunTyFlagMap (IfaceTypeMapG (IfaceTypeMapG (IfaceTypeMapG a)))
        , ifm_ty_con_app :: IfaceTyConMap (IfaceAppArgsMap a)
        , ifm_forall_ty :: IfaceForAllBndrMap (IfaceTypeMapG a)
        , ifm_cast_ty :: IfaceTypeMapG (IfaceCoercionMap a)
        , ifm_coercion_ty :: IfaceCoercionMap a
        , ifm_tuple_ty :: TupleSortMap (PromotionFlagMap (IfaceAppArgsMap a))
        , ifm_serialised_ty :: ForeignBinDataMap (OffsetBinDataMap a) }

type IfaceLiteralMap = Map.Map IfaceTyLit
type FunTyFlagMap = Map.Map FunTyFlag
type IfaceTyConMap = Map.Map IfaceTyCon
type ForAllTyFlagMap = Map.Map ForAllTyFlag
type IfaceCoercionMap = Map.Map IfaceCoercion
type TupleSortMap = Map.Map TupleSort
type PromotionFlagMap = Map.Map PromotionFlag
type ForeignBinDataMap = Map.Map BinArray
type OffsetBinDataMap = Map.Map Int
type IfaceForAllBndrMap = Compose IfaceBndrMap ForAllTyFlagMap

type IfaceIdBndrMap = Compose IfaceTypeMapG (Compose (UniqDFM IfLclName) IfaceTypeMapG)
type IfaceTvBndrMap = Compose (UniqDFM IfLclName) IfaceTypeMapG

type IfaceBndrMap = Product IfaceIdBndrMap IfaceTvBndrMap




type IfaceAppArgsMap a = ListMap (Compose IfaceTypeMapG ForAllTyFlagMap) a

emptyE :: IfaceTypeMapX a
emptyE = IFM { ifm_lit = emptyTM
             , ifm_var = emptyTM
             , ifm_app = emptyTM
             , ifm_fun_ty = emptyTM
             , ifm_ty_con_app = emptyTM
             , ifm_forall_ty = emptyTM
             , ifm_cast_ty = emptyTM
             , ifm_coercion_ty = emptyTM
             , ifm_tuple_ty = emptyTM
             , ifm_serialised_ty = emptyTM
             }

instance Functor IfaceTypeMapX where
  fmap f  IFM { ifm_lit = ilit
          , ifm_var = ivar
          , ifm_app = iapp
          , ifm_fun_ty = ift
          , ifm_ty_con_app = itc
          , ifm_forall_ty = ifal
          , ifm_cast_ty = icast
          , ifm_coercion_ty = ico
          , ifm_tuple_ty = itup
          , ifm_serialised_ty = iser }

    = IFM { ifm_lit = fmap f ilit
          , ifm_var = fmap f ivar
          , ifm_app = fmap (fmap f) iapp
          , ifm_fun_ty = fmap (fmap (fmap (fmap f))) ift
          , ifm_ty_con_app = fmap (fmap f) itc
          , ifm_forall_ty = fmap (fmap f) ifal
          , ifm_cast_ty = fmap (fmap f) icast
          , ifm_coercion_ty = fmap f ico
          , ifm_tuple_ty = fmap (fmap (fmap f)) itup
          , ifm_serialised_ty = fmap (fmap f) iser
          }

instance TrieMap IfaceTypeMapX where
  type Key IfaceTypeMapX = IfaceType

  emptyTM = emptyE
  lookupTM = lkE
  alterTM = xtE
  foldTM = fdE
  filterTM = ftE
  {-# INLINE lookupTM #-}
  {-# INLINE alterTM #-}

{-# INLINE ftE #-}
ftE :: (a -> Bool) -> IfaceTypeMapX a -> IfaceTypeMapX a
ftE f  IFM { ifm_lit = ilit
          , ifm_var = ivar
          , ifm_app = iapp
          , ifm_fun_ty = ift
          , ifm_ty_con_app = itc
          , ifm_forall_ty = ifal
          , ifm_cast_ty = icast
          , ifm_coercion_ty = ico
          , ifm_tuple_ty = itup
          , ifm_serialised_ty = iser  }

    = IFM { ifm_lit = filterTM f ilit
          , ifm_var = filterTM f ivar
          , ifm_app = fmap (filterTM f) iapp
          , ifm_fun_ty = fmap (fmap (fmap (filterTM f))) ift
          , ifm_ty_con_app = fmap (filterTM f) itc
          , ifm_forall_ty = fmap (filterTM f) ifal
          , ifm_cast_ty = fmap (filterTM f) icast
          , ifm_coercion_ty = filterTM f ico
          , ifm_tuple_ty = fmap (fmap (filterTM f)) itup
          , ifm_serialised_ty = fmap (filterTM f) iser
          }

{-# INLINE fdE #-}
fdE :: (a -> b -> b) -> IfaceTypeMapX a -> b -> b
fdE f  IFM { ifm_lit = ilit
          , ifm_var = ivar
          , ifm_app = iapp
          , ifm_fun_ty = ift
          , ifm_ty_con_app = itc
          , ifm_forall_ty = ifal
          , ifm_cast_ty = icast
          , ifm_coercion_ty = ico
          , ifm_tuple_ty = itup
          , ifm_serialised_ty= iser }
  = foldTM f ilit . foldTM f ivar . foldTM (foldTM f) iapp
  . foldTM (foldTM (foldTM (foldTM f))) ift
  . foldTM (foldTM f) itc
  . foldTM (foldTM f) ifal
  . foldTM (foldTM f) icast
  . foldTM f ico
  . foldTM (foldTM (foldTM f)) itup
  . foldTM (foldTM f) iser

bndrToKey :: IfaceBndr -> Either (IfaceType, (IfLclName, IfaceType)) IfaceTvBndr
bndrToKey (IfaceIdBndr (a,b,c)) = Left (a, (b,c))
bndrToKey (IfaceTvBndr k) = Right k

{-# INLINE lkE #-}
lkE :: IfaceType -> IfaceTypeMapX a -> Maybe a
lkE it ifm = go it ifm
  where
    go (IfaceSerialisedType binData) = ifm_serialised_ty >.> lookupTM (fbd_buffer binData) >=> lookupTM (fbd_off_s binData)
    go (IfaceFreeTyVar {}) = error "ftv"
    go (IfaceTyVar var) = ifm_var >.> lookupTM var
    go (IfaceLitTy l) = ifm_lit >.> lookupTM l
    go (IfaceAppTy ift args) = ifm_app >.> lkG ift >=> lookupTM (appArgsIfaceTypesForAllTyFlags args)
    go (IfaceFunTy ft t1 t2 t3) = ifm_fun_ty >.> lookupTM ft >=> lkG t1 >=> lkG t2 >=> lkG t3
    go (IfaceForAllTy (Bndr a b) t) = ifm_forall_ty >.> lookupTM (bndrToKey a,b) >=> lkG t
    go (IfaceTyConApp tc args) = ifm_ty_con_app >.> lookupTM tc >=> lookupTM (appArgsIfaceTypesForAllTyFlags args)
    go (IfaceCastTy ty co) = ifm_cast_ty >.> lkG ty >=> lookupTM co
    go (IfaceCoercionTy co) = ifm_coercion_ty >.> lookupTM co
    go (IfaceTupleTy sort prom args) = ifm_tuple_ty >.> lookupTM sort >=> lookupTM prom >=> lookupTM (appArgsIfaceTypesForAllTyFlags args)

{-# INLINE xtE #-}
xtE :: IfaceType -> XT a -> IfaceTypeMapX a -> IfaceTypeMapX a
xtE (IfaceSerialisedType binData) f m = m { ifm_serialised_ty = ifm_serialised_ty m |> alterTM (fbd_buffer binData) |>> alterTM (fbd_off_s binData) f }
xtE (IfaceFreeTyVar {}) _ _ = error "ftv"
xtE (IfaceTyVar var) f m = m { ifm_var = ifm_var m |> alterTM var f }
xtE (IfaceLitTy l) f m = m { ifm_lit = ifm_lit m |> alterTM l f }
xtE (IfaceAppTy ift args) f m = m { ifm_app = ifm_app m |> xtG ift |>> alterTM (appArgsIfaceTypesForAllTyFlags args) f }
xtE (IfaceFunTy ft t1 t2 t3) f m = m { ifm_fun_ty = ifm_fun_ty m |> alterTM ft |>> xtG t1 |>> xtG t2 |>> xtG t3 f }
xtE (IfaceForAllTy (Bndr a b) t) f m = m { ifm_forall_ty = ifm_forall_ty m |> alterTM (bndrToKey a,b) |>> xtG t f }
xtE (IfaceTyConApp tc args) f m = m { ifm_ty_con_app = ifm_ty_con_app m |> alterTM tc |>> alterTM (appArgsIfaceTypesForAllTyFlags args) f }
xtE (IfaceCastTy ty co) f m = m { ifm_cast_ty = ifm_cast_ty m |> xtG ty |>> alterTM co f }
xtE (IfaceCoercionTy co) f m = m { ifm_coercion_ty = ifm_coercion_ty m |> alterTM co f }
xtE (IfaceTupleTy sort prom args) f m = m { ifm_tuple_ty = ifm_tuple_ty m |> alterTM sort |>> alterTM prom |>> alterTM (appArgsIfaceTypesForAllTyFlags args) f }
