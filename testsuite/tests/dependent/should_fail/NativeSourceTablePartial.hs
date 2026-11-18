{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeSourceTablePartial where

type Answer = oneRow 'False

oneRow :: Bool -> Bool
oneRow False = True
