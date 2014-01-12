-- Test we don't get a cycle for "phantom" superclasses
{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, FlexibleContexts #-}
module TcOK where

class A cls c where
    meth :: cls c => c -> c

class A B c => B c where
