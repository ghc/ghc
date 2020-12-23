{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module T17139 where

import T17139a

type family TypeFam f fun where
  TypeFam f (a -> b) = f a -> TypeFam f b

lift :: (a -> b) -> TypeFam f (a -> b)
lift f = \x -> _ (f <*> x)
