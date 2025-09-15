{-# OPTIONS_GHC -fdefer-type-errors #-}     -- Very important to this bug!
{-# Language PartialTypeSignatures #-}
{-# Language KindSignatures #-}
{-# Language PolyKinds #-}
{-# Language ScopedTypeVariables #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TypeApplications #-}

module T14584a where

f :: forall m. ()
f = id @m :: _

g :: forall m. ()
g = let h = id @m
    in h
