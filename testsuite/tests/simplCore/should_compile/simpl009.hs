{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -dcore-lint #-}

-- This one killed GHC 6.0, with a "panic: applyTys"
-- Only with -O, though

module ShouldCompile where

newtype R = R (forall a. a->a)

foo = case undefined of
	R f -> f ()
