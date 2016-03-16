{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module T11711 where

import Data.Kind (Type)

data (:~~:) (a :: k1) (b :: k2) where
    HRefl :: a :~~: a

data TypeRep (a :: k) where
    TrTyCon :: String -> TypeRep k -> TypeRep (a :: k)
    TrApp   :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
               TypeRep (a :: k1 -> k2)
            -> TypeRep (b :: k1)
            -> TypeRep (a b)

class Typeable (a :: k) where
    typeRep :: TypeRep a

data TypeRepX where
    TypeRepX :: forall k (a :: k). TypeRep a -> TypeRepX

eqTypeRep :: TypeRep a -> TypeRep b -> Maybe (a :~~: b)
eqTypeRep = undefined

typeRepKind :: forall k (a :: k). TypeRep a -> TypeRep k
typeRepKind = undefined

instance Typeable Type where
  typeRep = TrTyCon "Type" typeRep

funResultTy :: TypeRepX -> TypeRepX -> Maybe TypeRepX
funResultTy (TypeRepX f) (TypeRepX x)
  | Just HRefl <- (typeRep :: TypeRep Type) `eqTypeRep` typeRepKind f
  , TRFun arg res <- f
  , Just HRefl <- arg `eqTypeRep` x
  = Just (TypeRepX res)
  | otherwise
  = Nothing

trArrow :: TypeRep (->)
trArrow = undefined

pattern TRFun :: forall fun. ()
              => forall arg res. (fun ~ (arg -> res))
              => TypeRep arg
              -> TypeRep res
              -> TypeRep fun
pattern TRFun arg res <- TrApp (TrApp (eqTypeRep trArrow -> Just HRefl) arg) res
