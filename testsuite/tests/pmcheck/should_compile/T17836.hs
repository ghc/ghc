{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module PM where

import Data.Type.Equality ( type (:~:)(..) )
import qualified Data.Kind

data Type :: Data.Kind.Type -> Data.Kind.Type where
  SRecNil :: Type ()
  SRecCons :: String -> Type a -> Type b -> Type (a, b)
  SIntTy  :: Type Int

pattern RecCons1 a <- (SRecCons _ _ a)

eqType :: Type ty1 -> Type ty2 -> Maybe (ty1 :~: ty2)
eqType SRecNil SRecNil = Just Refl
eqType (SRecCons l1 s1 t1) (SRecCons l2 s2 t2)
  | Just Refl <- s1 `eqType` s2
  , Just Refl <- t1 `eqType` t2
  , l1 == l2
  = Just Refl
eqType SIntTy  SIntTy  = Just Refl
eqType _ _ = Nothing

massive :: Int -> Type ty -> Type recty -> (forall ty'. Type ty' -> m r) -> m r
massive fieldN sFieldTy sRecTy k =
  case (fieldN, sFieldTy, sRecTy) of
    (0, t, SRecCons _ t' _)
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (1, t, RecCons1 (SRecCons _ t' _))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (2, t, RecCons1 (RecCons1 (SRecCons _ t' _)))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (3, t, RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (4, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (5, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (6, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (7, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (8, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (9, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (10, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (11, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (12, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (13, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (14, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (15, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (16, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (17, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (18, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (19, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (20, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (21, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (22, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (23, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (24, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (25, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (26, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (27, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (28, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (29, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (30, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (31, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _))))))))))))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    (32, t, RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (RecCons1 (SRecCons _ t' _)))))))))))))))))))))))))))))))))
        |  Just Refl <- t `eqType` t' -> k sFieldTy
    _ -> error "TODO support records >32"
