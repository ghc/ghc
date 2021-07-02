{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bug where

type family F a

g :: forall a. a
g = f
  where
    f :: (F b ~ a) => a
    f = undefined
