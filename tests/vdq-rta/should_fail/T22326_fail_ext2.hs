{-# LANGUAGE NoRequiredTypeArguments #-}

module T22326_fail_ext2 where

-- Using visible forall in the type of a term is only possible
-- with the RequiredTypeArguments extension, which is disabled
-- in this module.
f :: forall x -> ()
f = f