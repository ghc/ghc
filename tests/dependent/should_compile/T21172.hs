{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module T21172 where

import Data.Kind (Type)

type SubTagKind :: forall k . k -> Type
type family SubTagKind tag

type SubTag :: forall tag -> SubTagKind tag
type family SubTag tag

type Tag :: Type
data Tag

type instance SubTagKind Tag = Type
type instance SubTag Tag = Tag

