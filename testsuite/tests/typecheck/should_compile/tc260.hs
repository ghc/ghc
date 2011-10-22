-- Test we don't get a cycle for "phantom" superclasses,
-- even if the phantomness is behind a type synonym
{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, FlexibleContexts #-}
module TcOK where

class A ctxt c where
    meth :: ctxt => c -> c

type Bish = B Int

class A Bish c => B c where
