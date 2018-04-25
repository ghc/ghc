{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T13272a where

import GHC.Generics

class TypeName a where
  typeName         :: proxy a -> String
  default typeName :: (Generic a, Rep a ~ gg, gg ~ D1 d f, Datatype d)
                   => proxy a -> String
  typeName _ = gtypeName $ from (undefined :: a)

gtypeName :: Datatype d => D1 d f p -> String
gtypeName = datatypeName

data T a = MkT a
  deriving (Generic, TypeName)
