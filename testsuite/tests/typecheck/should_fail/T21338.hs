{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module T21338 where

import Data.Kind ( Type, Constraint )
import Data.Proxy ( Proxy(..) )

newtype K a b = K a

type NP :: (Type -> Type) -> [Type] -> Type
data NP f xs where

data FieldInfo a

type All :: [Type] -> Constraint
type family All xs where {}

data ConstructorInfo :: [Type] -> Type where
  Record :: All xs => NP (K String) xs -> ConstructorInfo xs

hmap :: (forall a. f a -> g a) -> h f xs -> h g xs
hmap _ _ = undefined

foo :: forall a flds. ConstructorInfo flds
foo = undefined

fieldNames :: forall (a :: Type) flds. NP (K String) flds
fieldNames = case foo @a {- @flds -} of
  Record np -> hmap id np
  _ -> hmap undefined @flds
       -- The last line caused a "No skolem info" panic on GHC 9.2 and below.
