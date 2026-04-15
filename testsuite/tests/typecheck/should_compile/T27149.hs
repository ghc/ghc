{-# LANGUAGE TypeFamilies #-}
module T27149 where

import Data.Kind (Type)

type T :: Type -> Type
data T a where
  MkT :: T Bool

type F :: Type -> Type
type family F a where
  F Bool = Int

f :: IO (T a) -> (Bool -> Int) -> IO (F a)
f mt g = do
  t <- mt
  case t of
    MkT -> return $ g True
