{-# LANGUAGE ConstraintKinds, TypeFamilies #-}

module T11515 where

type family ShowSyn a where ShowSyn a = Show a

foo :: (ShowSyn a, _) => a -> String
foo x = show x
