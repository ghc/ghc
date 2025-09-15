{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

-- Tests the special case of
--      non-recursive, function binding,
--      with no type signature

module ShouldCompile where

f = \ (x :: forall a. a->a) -> (x True, x 'c')

g (x :: forall a. a->a) = x
