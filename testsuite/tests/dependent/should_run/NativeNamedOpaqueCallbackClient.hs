{-# LANGUAGE CPP, NoExperimentalDependentTypes #-}

module Main where

import qualified NativeNamedOpaqueCallbackAlias as Alias
import qualified NativeNamedOpaqueCallbackProvider as Other
import NativeNamedOpaqueCallbackPublic (called, owns, ownsCall, stored)

main :: IO ()
#ifdef WRONG_OWNER
main = print (owns Other.otherMapper (stored Other.otherMapper))
#elif defined(WRONG_CALL)
main = print (ownsCall Other.otherMapper True (called True))
#else
main = print (owns Alias.mapper (stored Other.otherMapper), ownsCall Alias.mapper True (called True))
#endif
