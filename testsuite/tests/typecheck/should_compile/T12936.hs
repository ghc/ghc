{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Token where

class S s t | s -> t

m :: forall s t . S s t => s
m = undefined

o :: forall s t . S s t => s -> s
o = undefined

c :: forall s . s -> s -> s
c = undefined

p :: forall s . S s () => s -> s
p d = f
  where

    -- declaring either of these type signatures will cause the bug to go away

    -- f :: s
    f = c d (o e)

    -- e :: s
    e = c m m
