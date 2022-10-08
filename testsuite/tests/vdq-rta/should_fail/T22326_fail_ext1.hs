{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE NoExplicitNamespaces #-}

module T22326_fail_ext1 where

-- Using the (type ...) syntax requires the ExplicitNamespaces extension,
-- which is disabled in this module.
--
-- This is not related to whether the RequiredTypeArguments extension is
-- enabled, as Part 2 of the proposal (https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst)
-- shows how the feature could be used without the ‘type’ keyword.
x = undefined (type Int)