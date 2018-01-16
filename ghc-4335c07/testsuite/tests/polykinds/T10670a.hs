{-# LANGUAGE GADTs , PolyKinds #-}

module Bug2 where

import Unsafe.Coerce

data TyConT (a::k) = TyConT String

eqTyConT :: TyConT a -> TyConT b -> Bool
eqTyConT (TyConT a) (TyConT b) = a == b



tyConTArr :: TyConT (->)
tyConTArr = TyConT "(->)"


data TypeRepT (a::k) where
  TRCon :: TyConT a -> TypeRepT a
  TRApp :: TypeRepT a -> TypeRepT b -> TypeRepT (a b)


data GetAppT a where
  GA :: TypeRepT a -> TypeRepT b -> GetAppT (a b)

getAppT :: TypeRepT a -> Maybe (GetAppT a)
getAppT (TRApp a b) = Just $ GA a b
getAppT _ = Nothing



eqTT :: TypeRepT (a::k1) -> TypeRepT (b::k2) -> Bool
eqTT (TRCon a) (TRCon b) = eqTyConT a b
eqTT (TRApp c a) (TRApp d b) = eqTT c d && eqTT a b
eqTT _ _ = False


data G2 c a where
  G2 :: TypeRepT a -> TypeRepT b -> G2 c (c a b)


getT2 :: TypeRepT (c :: k2 -> k1 -> k) -> TypeRepT (a :: k) -> Maybe (G2 c a)
getT2 c t = do GA t' b <- getAppT t
               GA c' a <- getAppT t'
               if eqTT c c'
                 then Just (unsafeCoerce $ G2 a b :: G2 c a)
                 else Nothing

tyRepTArr :: TypeRepT (->)
tyRepTArr = TRCon tyConTArr

s tf = case getT2 tyRepTArr tf
       of Just (G2 _ _) -> Nothing
          _ -> Nothing
