{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies #-}

-- This one caught a bug in the implementation of functional
-- dependencies, where improvement must happen when
-- checking the call in 'test4'

module ShouldCompile where

newtype M s a = M a

class Modular s a | s -> a

wim ::  forall a w. Integral a
                      => a -> (forall s. Modular s a => M s w) -> w
wim i k = error "urk"

test4'  ::  (Modular s a, Integral a) => M s a
test4'  =   error "urk"

test4   =   wim 4 test4'
--   Integral a0, (Modular s a0 => Modular s1 a1, Integral a1, M s1 a1 ~ M s w0)
--   Under the implication, [D] a1 ~ a0, [W] a1 ~ w0
--   Hence a1 := w0, [D] w0 ~ a0
