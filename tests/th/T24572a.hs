{-# LANGUAGE TemplateHaskell, RequiredTypeArguments, ImpredicativeTypes #-}

module T24572a where

idee :: forall a -> a -> a
idee _ x = x

f = $([| idee (forall a. a ~ Int => a -> a) |])

type (!@#) = Bool

g :: Bool -> Bool
g = $([| idee (!@#) |])
