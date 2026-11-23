{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, LinearTypes #-}

module NativeNamedOpaqueCallbackPublic (stored, owns, called, ownsCall) where

import GHC.Matchability (U)
import qualified NativeNamedOpaqueCallbackAlias as Alias
import qualified NativeNamedOpaqueCallbackProvider as Left

data Stored (f :: Bool %U -> Char) = Stored

store :: foreach (f :: Bool -> Char) -> Stored f
store _ = Stored

stored :: foreach (mapper :: Bool -> Char) -> Stored Left.mapper
#ifdef WRONG_LEXICAL_BINDER
stored mapper = store mapper
#else
stored _ = store Alias.mapper
#endif

owns :: foreach (f :: Bool -> Char) -> Stored f -> Bool
owns _ Stored = True

data Call (result :: Char) = Call

call :: foreach (f :: Bool -> Char) -> foreach (x :: Bool) -> Call (f x)
call _ _ = Call

called :: foreach (x :: Bool) -> Call ((Left.mapper :: Bool %U -> Char) x)
called x = call Alias.mapper x

ownsCall :: foreach (f :: Bool -> Char) -> foreach (x :: Bool) -> Call (f x) -> Bool
ownsCall _ _ Call = True
