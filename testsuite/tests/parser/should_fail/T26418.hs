{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoExplicitNamespaces #-}

module T26418 (data HeadC) where

pattern HeadC :: forall a. a -> [a]
pattern HeadC x <- x:_xs where
  HeadC x = [x]

