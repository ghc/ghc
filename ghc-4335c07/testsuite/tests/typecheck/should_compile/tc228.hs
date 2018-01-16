{-# LANGUAGE ExistentialQuantification #-}

-- Without a type sig this is slightly tricky.
-- See Trac #1430

-- Reason: we get an implication constraint (forall a. Typeable a => Typeable b),
-- when generalising unExTypeable.  We want to infer a context for the 
-- whole thing of (Typeable b).
-- See Note [Inference and implication constraints] in TcSimplify


module Foo where

import Data.Typeable

data ExTypeable = forall a. Typeable a => ExTypeable a

-- unExTypeable :: Typeable h => ExTypeable -> Maybe h
unExTypeable (ExTypeable a) = cast a

