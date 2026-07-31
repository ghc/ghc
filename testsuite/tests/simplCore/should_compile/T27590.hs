{-# LANGUAGE RequiredTypeArguments #-}

module Foo where

wombat :: forall a -> a -> Maybe a
{-# INLINE wombat #-}
wombat t x = Just x

g y = wombat Int (y+y)
      -- wombat /should/ inline here
