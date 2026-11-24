{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}

module NativeSizeListNoDescentSource (withoutDescent) where

import PreludeMapLengthSize (Nat(..), Size)

withoutDescent :: Size -> [a] -> [a]
withoutDescent Zero _ = []
withoutDescent (Succ _) [] = []
withoutDescent bound@(Succ _) input@(x : _) = x : withoutDescent bound input
