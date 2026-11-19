{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module NativeLocalOpenSibling where
import Data.Kind (Type)
type Selected :: Type -> Type
type Selected a = NativeLocalOpenSibling.left a
left x = keep x (right x)
right x = left x
keep x _ = x
{-# RULES "right/strict" forall x. right x = x `seq` x #-}
