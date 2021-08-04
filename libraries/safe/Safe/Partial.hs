{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE ImplicitParams  #-}

-- | ConstraintKind synonym for marking partial functions
module Safe.Partial(Partial) where

-- Let things work through ghci alone
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-- GHC has changed its opinion on the location a few times
-- v0: GHC 7.4.1, has ConstraintKinds
-- v1: GHC 7.10.2, base 4.8.1.0 = CallStack
-- v2: GHC 8.0.1, base 4.9.0.0 = HasCallStack

#if __GLASGOW_HASKELL__ >= 800
#define OPTION 2
#elif __GLASGOW_HASKELL__ >= 710 && MIN_VERSION_base(4,8,1)
#define OPTION 1
#else
#define OPTION 0
#endif


#if OPTION == 0
import GHC.Exts
#else
import GHC.Stack
#endif

-- | A constraint synonym which denotes that the function is partial, and will
--   (on GHC 8.* and up) produce a stack trace on failure.
--   You may mark your own non-total functions as Partial, if necessary, and this
--   will ensure that they produce useful stack traces.
#if OPTION == 0
type Partial = (() :: Constraint)
#elif OPTION == 1
type Partial = (?loc :: CallStack)
#else
type Partial = HasCallStack
#endif
