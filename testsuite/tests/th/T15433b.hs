{-# LANGUAGE TemplateHaskell, TypeApplications, TypeFamilies, PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module T15433b where

import T15433_aux ( wild )

type family F a where
  F $wild = Int

f :: Maybe a -> Maybe a
f (x :: Maybe $wild) = x

g :: forall a. Maybe a -> Maybe a
g (Just @($wild) x) = Just x
g Nothing = Nothing

h :: a -> $wild
h x = x
