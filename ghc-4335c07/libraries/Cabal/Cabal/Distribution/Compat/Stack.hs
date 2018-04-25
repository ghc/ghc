{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
module Distribution.Compat.Stack (
    WithCallStack,
    CallStack,
    annotateCallStackIO,
    withFrozenCallStack,
    withLexicalCallStack,
    callStack,
    prettyCallStack,
    parentSrcLocPrefix
) where

import System.IO.Error

#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,8,1)
#define GHC_STACK_SUPPORTED 1
#endif
#endif

#ifdef GHC_STACK_SUPPORTED
import GHC.Stack
#endif

#ifdef GHC_STACK_SUPPORTED

#if MIN_VERSION_base(4,9,0)
type WithCallStack a = HasCallStack => a
#elif MIN_VERSION_base(4,8,1)
type WithCallStack a = (?callStack :: CallStack) => a
#endif

#if !MIN_VERSION_base(4,9,0)
-- NB: Can't say WithCallStack (WithCallStack a -> a);
-- Haskell doesn't support this kind of implicit parameter!
-- See https://mail.haskell.org/pipermail/ghc-devs/2016-January/011096.html
-- Since this function doesn't do anything, it's OK to
-- give it a less good type.
withFrozenCallStack :: WithCallStack (a -> a)
withFrozenCallStack x = x

callStack :: (?callStack :: CallStack) => CallStack
callStack = ?callStack

prettyCallStack :: CallStack -> String
prettyCallStack = showCallStack
#endif

-- | Give the *parent* of the person who invoked this;
-- so it's most suitable for being called from a utility function.
-- You probably want to call this using 'withFrozenCallStack'; otherwise
-- it's not very useful.  We didn't implement this for base-4.8.1
-- because we cannot rely on freezing to have taken place.
--
parentSrcLocPrefix :: WithCallStack String
#if MIN_VERSION_base(4,9,0)
parentSrcLocPrefix =
  case getCallStack callStack of
    (_:(_, loc):_) -> showLoc loc
    [(_, loc)] -> showLoc loc
    [] -> error "parentSrcLocPrefix: empty call stack"
 where
  showLoc loc =
    srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ ": "
#else
parentSrcLocPrefix = "Call sites not available with base < 4.9.0.0 (GHC 8.0): "
#endif

-- Yeah, this uses skivvy implementation details.
withLexicalCallStack :: (a -> WithCallStack (IO b)) -> WithCallStack (a -> IO b)
withLexicalCallStack f =
    let stk = ?callStack
    in \x -> let ?callStack = stk in f x

#else

data CallStack = CallStack
    deriving (Eq, Show)

type WithCallStack a = a

withFrozenCallStack :: a -> a
withFrozenCallStack x = x

callStack :: CallStack
callStack = CallStack

prettyCallStack :: CallStack -> String
prettyCallStack _ = "Call stacks not available with base < 4.8.1.0 (GHC 7.10)"

parentSrcLocPrefix :: String
parentSrcLocPrefix = "Call sites not available with base < 4.9.0.0 (GHC 8.0): "

withLexicalCallStack :: (a -> IO b) -> a -> IO b
withLexicalCallStack f = f

#endif

-- | This function is for when you *really* want to add a call
-- stack to raised IO, but you don't have a
-- 'Distribution.Verbosity.Verbosity' so you can't use
-- 'Distribution.Simple.Utils.annotateIO'.  If you have a 'Verbosity',
-- please use that function instead.
annotateCallStackIO :: WithCallStack (IO a -> IO a)
annotateCallStackIO = modifyIOError f
  where
    f ioe = ioeSetErrorString ioe
          . wrapCallStack
          $ ioeGetErrorString ioe
    wrapCallStack s =
        prettyCallStack callStack ++ "\n" ++ s
