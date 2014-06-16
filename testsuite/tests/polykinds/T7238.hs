{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module T7238 where

import GHC.Exts

class Pair p where
	type Ctxt p a :: Constraint
	l :: Ctxt p a => p a -> a

data Unit a = Unit

instance Pair Unit where
	type Ctxt Unit a = a ~ ()
	l _ = ()