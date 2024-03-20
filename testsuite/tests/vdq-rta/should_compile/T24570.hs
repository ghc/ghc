{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T24570 where

import Language.Haskell.TH

idee :: forall a -> a -> a
idee _ x = x

type (:!@#) = Bool

f :: Bool -> Bool
f = idee (:!@#)

type (!@#) = Bool

g :: Bool -> Bool
g = idee (!@#)