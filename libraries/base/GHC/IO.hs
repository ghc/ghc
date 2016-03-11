{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , RankNTypes
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Definitions for the 'IO' monad and its friends.
--
-----------------------------------------------------------------------------

module GHC.IO (
        IO(..), unIO, failIO, liftIO, mplusIO,
        unsafePerformIO, unsafeInterleaveIO,
        unsafeDupablePerformIO, unsafeDupableInterleaveIO,
        noDuplicate,

        -- To and from from ST
        stToIO, ioToST, unsafeIOToST, unsafeSTToIO,

        FilePath,

        catchException, catchAny, throwIO,
        mask, mask_, uninterruptibleMask, uninterruptibleMask_,
        MaskingState(..), getMaskingState,
        unsafeUnmask, interruptible,
        onException, bracket, finally, evaluate
    ) where

import GHC.Base
import GHC.ST
import GHC.Exception
import GHC.Show
import GHC.IO.Unsafe

import {-# SOURCE #-} GHC.IO.Exception ( userError, IOError )

-- ---------------------------------------------------------------------------
-- The IO Monad

{-
The IO Monad is just an instance of the ST monad, where the state is
the real world.  We use the exception mechanism (in GHC.Exception) to
implement IO exceptions.

NOTE: The IO representation is deeply wired in to various parts of the
system.  The following list may or may not be exhaustive:

Compiler  - types of various primitives in PrimOp.lhs

RTS       - forceIO (StgMiscClosures.hc)
          - catchzh_fast, (un)?blockAsyncExceptionszh_fast, raisezh_fast
            (Exceptions.hc)
          - raiseAsync (Schedule.c)

Prelude   - GHC.IO.lhs, and several other places including
            GHC.Exception.lhs.

Libraries - parts of hslibs/lang.

--SDM
-}

liftIO :: IO a -> State# RealWorld -> STret RealWorld a
liftIO (IO m) = \s -> case m s of (# s', r #) -> STret s' r

failIO :: String -> IO a
failIO s = IO (raiseIO# (toException (userError s)))

-- ---------------------------------------------------------------------------
-- Coercions between IO and ST

-- | A monad transformer embedding strict state transformers in the 'IO'
-- monad.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO        :: ST RealWorld a -> IO a
stToIO (ST m) = IO m

ioToST        :: IO a -> ST RealWorld a
ioToST (IO m) = (ST m)

-- This relies on IO and ST having the same representation modulo the
-- constraint on the type of the state
--
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST (IO io) = ST $ \ s -> (unsafeCoerce# io) s

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST m) = IO (unsafeCoerce# m)

-- -----------------------------------------------------------------------------
-- | File and directory names are values of type 'String', whose precise
-- meaning is operating system dependent. Files can be opened, yielding a
-- handle which can then be used to operate on the contents of that file.

type FilePath = String

-- -----------------------------------------------------------------------------
-- Primitive catch and throwIO

{-
catchException used to handle the passing around of the state to the
action and the handler.  This turned out to be a bad idea - it meant
that we had to wrap both arguments in thunks so they could be entered
as normal (remember IO returns an unboxed pair...).

Now catch# has type

    catch# :: IO a -> (b -> IO a) -> IO a

(well almost; the compiler doesn't know about the IO newtype so we
have to work around that in the definition of catchException below).
-}

-- | Catch an exception in the 'IO' monad.
--
-- Note that this function is /strict/ in the action. That is,
-- @catchException undefined b == _|_@. See #exceptions_and_strictness#
-- for details.
catchException :: Exception e => IO a -> (e -> IO a) -> IO a
catchException (IO io) handler = IO $ catch# io handler'
    where handler' e = case fromException e of
                       Just e' -> unIO (handler e')
                       Nothing -> raiseIO# e

-- | Catch any 'Exception' type in the 'IO' monad.
--
-- Note that this function is /strict/ in the action. That is,
-- @catchException undefined b == _|_@. See #exceptions_and_strictness# for
-- details.
catchAny :: IO a -> (forall e . Exception e => e -> IO a) -> IO a
catchAny (IO io) handler = IO $ catch# io handler'
    where handler' (SomeException e) = unIO (handler e)


mplusIO :: IO a -> IO a -> IO a
mplusIO m n = m `catchIOError` \ _ -> n
    where catchIOError :: IO a -> (IOError -> IO a) -> IO a
          catchIOError = catchException

-- | A variant of 'throw' that can only be used within the 'IO' monad.
--
-- Although 'throwIO' has a type that is an instance of the type of 'throw', the
-- two functions are subtly different:
--
-- > throw e   `seq` x  ===> throw e
-- > throwIO e `seq` x  ===> x
--
-- The first example will cause the exception @e@ to be raised,
-- whereas the second one won\'t.  In fact, 'throwIO' will only cause
-- an exception to be raised when it is used within the 'IO' monad.
-- The 'throwIO' variant should be used in preference to 'throw' to
-- raise an exception within the 'IO' monad because it guarantees
-- ordering with respect to other 'IO' operations, whereas 'throw'
-- does not.
throwIO :: Exception e => e -> IO a
throwIO e = IO (raiseIO# (toException e))

-- -----------------------------------------------------------------------------
-- Controlling asynchronous exception delivery

-- Applying 'block' to a computation will
-- execute that computation with asynchronous exceptions
-- /blocked/.  That is, any thread which
-- attempts to raise an exception in the current thread with 'Control.Exception.throwTo' will be
-- blocked until asynchronous exceptions are unblocked again.  There\'s
-- no need to worry about re-enabling asynchronous exceptions; that is
-- done automatically on exiting the scope of
-- 'block'.
--
-- Threads created by 'Control.Concurrent.forkIO' inherit the blocked
-- state from the parent; that is, to start a thread in blocked mode,
-- use @block $ forkIO ...@.  This is particularly useful if you need to
-- establish an exception handler in the forked thread before any
-- asynchronous exceptions are received.
block :: IO a -> IO a
block (IO io) = IO $ maskAsyncExceptions# io

-- To re-enable asynchronous exceptions inside the scope of
-- 'block', 'unblock' can be
-- used.  It scopes in exactly the same way, so on exit from
-- 'unblock' asynchronous exception delivery will
-- be disabled again.
unblock :: IO a -> IO a
unblock = unsafeUnmask

unsafeUnmask :: IO a -> IO a
unsafeUnmask (IO io) = IO $ unmaskAsyncExceptions# io

-- | Allow asynchronous exceptions to be raised even inside 'mask', making
-- the operation interruptible (see the discussion of "Interruptible operations"
-- in 'Control.Exception').
--
-- When called outside 'mask', or inside 'uninterruptibleMask', this
-- function has no effect.
--
-- @since 4.9.0.0
interruptible :: IO a -> IO a
interruptible act = do
  st <- getMaskingState
  case st of
    Unmasked              -> act
    MaskedInterruptible   -> unsafeUnmask act
    MaskedUninterruptible -> act

blockUninterruptible :: IO a -> IO a
blockUninterruptible (IO io) = IO $ maskUninterruptible# io

-- | Describes the behaviour of a thread when an asynchronous
-- exception is received.
data MaskingState
  = Unmasked -- ^ asynchronous exceptions are unmasked (the normal state)
  | MaskedInterruptible
      -- ^ the state during 'mask': asynchronous exceptions are masked, but blocking operations may still be interrupted
  | MaskedUninterruptible
      -- ^ the state during 'uninterruptibleMask': asynchronous exceptions are masked, and blocking operations may not be interrupted
 deriving (Eq,Show)

-- | Returns the 'MaskingState' for the current thread.
getMaskingState :: IO MaskingState
getMaskingState  = IO $ \s ->
  case getMaskingState# s of
     (# s', i #) -> (# s', case i of
                             0# -> Unmasked
                             1# -> MaskedUninterruptible
                             _  -> MaskedInterruptible #)

onException :: IO a -> IO b -> IO a
onException io what = io `catchException` \e -> do _ <- what
                                                   throwIO (e :: SomeException)

-- | Executes an IO computation with asynchronous
-- exceptions /masked/.  That is, any thread which attempts to raise
-- an exception in the current thread with 'Control.Exception.throwTo'
-- will be blocked until asynchronous exceptions are unmasked again.
--
-- The argument passed to 'mask' is a function that takes as its
-- argument another function, which can be used to restore the
-- prevailing masking state within the context of the masked
-- computation.  For example, a common way to use 'mask' is to protect
-- the acquisition of a resource:
--
-- > mask $ \restore -> do
-- >     x <- acquire
-- >     restore (do_something_with x) `onException` release
-- >     release
--
-- This code guarantees that @acquire@ is paired with @release@, by masking
-- asynchronous exceptions for the critical parts. (Rather than write
-- this code yourself, it would be better to use
-- 'Control.Exception.bracket' which abstracts the general pattern).
--
-- Note that the @restore@ action passed to the argument to 'mask'
-- does not necessarily unmask asynchronous exceptions, it just
-- restores the masking state to that of the enclosing context.  Thus
-- if asynchronous exceptions are already masked, 'mask' cannot be used
-- to unmask exceptions again.  This is so that if you call a library function
-- with exceptions masked, you can be sure that the library call will not be
-- able to unmask exceptions again.  If you are writing library code and need
-- to use asynchronous exceptions, the only way is to create a new thread;
-- see 'Control.Concurrent.forkIOWithUnmask'.
--
-- Asynchronous exceptions may still be received while in the masked
-- state if the masked thread /blocks/ in certain ways; see
-- "Control.Exception#interruptible".
--
-- Threads created by 'Control.Concurrent.forkIO' inherit the
-- 'MaskingState' from the parent; that is, to start a thread in the
-- 'MaskedInterruptible' state,
-- use @mask_ $ forkIO ...@.  This is particularly useful if you need
-- to establish an exception handler in the forked thread before any
-- asynchronous exceptions are received.  To create a a new thread in
-- an unmasked state use 'Control.Concurrent.forkIOUnmasked'.
--
mask  :: ((forall a. IO a -> IO a) -> IO b) -> IO b

-- | Like 'mask', but does not pass a @restore@ action to the argument.
mask_ :: IO a -> IO a

-- | Like 'mask', but the masked computation is not interruptible (see
-- "Control.Exception#interruptible").  THIS SHOULD BE USED WITH
-- GREAT CARE, because if a thread executing in 'uninterruptibleMask'
-- blocks for any reason, then the thread (and possibly the program,
-- if this is the main thread) will be unresponsive and unkillable.
-- This function should only be necessary if you need to mask
-- exceptions around an interruptible operation, and you can guarantee
-- that the interruptible operation will only block for a short period
-- of time.
--
uninterruptibleMask :: ((forall a. IO a -> IO a) -> IO b) -> IO b

-- | Like 'uninterruptibleMask', but does not pass a @restore@ action
-- to the argument.
uninterruptibleMask_ :: IO a -> IO a

mask_ io = mask $ \_ -> io

mask io = do
  b <- getMaskingState
  case b of
    Unmasked              -> block $ io unblock
    MaskedInterruptible   -> io block
    MaskedUninterruptible -> io blockUninterruptible

uninterruptibleMask_ io = uninterruptibleMask $ \_ -> io

uninterruptibleMask io = do
  b <- getMaskingState
  case b of
    Unmasked              -> blockUninterruptible $ io unblock
    MaskedInterruptible   -> blockUninterruptible $ io block
    MaskedUninterruptible -> io blockUninterruptible

bracket
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracket before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r

finally :: IO a         -- ^ computation to run first
        -> IO b         -- ^ computation to run afterward (even if an exception
                        -- was raised)
        -> IO a         -- returns the value from the first computation
a `finally` sequel =
  mask $ \restore -> do
    r <- restore a `onException` sequel
    _ <- sequel
    return r

-- | Evaluate the argument to weak head normal form.
--
-- 'evaluate' is typically used to uncover any exceptions that a lazy value
-- may contain, and possibly handle them.
--
-- 'evaluate' only evaluates to /weak head normal form/. If deeper
-- evaluation is needed, the @force@ function from @Control.DeepSeq@
-- may be handy:
--
-- > evaluate $ force x
--
-- There is a subtle difference between @'evaluate' x@ and @'return' '$!' x@,
-- analogous to the difference between 'throwIO' and 'throw'. If the lazy
-- value @x@ throws an exception, @'return' '$!' x@ will fail to return an
-- 'IO' action and will throw an exception instead. @'evaluate' x@, on the
-- other hand, always produces an 'IO' action; that action will throw an
-- exception upon /execution/ iff @x@ throws an exception upon /evaluation/.
--
-- The practical implication of this difference is that due to the
-- /imprecise exceptions/ semantics,
--
-- > (return $! error "foo") >> error "bar"
--
-- may throw either @"foo"@ or @"bar"@, depending on the optimizations
-- performed by the compiler. On the other hand,
--
-- > evaluate (error "foo") >> error "bar"
--
-- is guaranteed to throw @"foo"@.
--
-- The rule of thumb is to use 'evaluate' to force or handle exceptions in
-- lazy values. If, on the other hand, you are forcing a lazy value for
-- efficiency reasons only and do not care about exceptions, you may
-- use @'return' '$!' x@.
evaluate :: a -> IO a
evaluate a = IO $ \s -> seq# a s -- NB. see #2273, #5129

{- $exceptions_and_strictness

Laziness can interact with @catch@-like operations in non-obvious ways (see,
e.g. GHC Trac #11555). For instance, consider these subtly-different examples,

> test1 = Control.Exception.catch (error "uh oh") (\(_ :: SomeException) -> putStrLn "it failed")
>
> test2 = GHC.IO.catchException (error "uh oh") (\(_ :: SomeException) -> putStrLn "it failed")

While the first case is always guaranteed to print "it failed", the behavior of
@test2@ may vary with optimization level.

The unspecified behavior of @test2@ is due to the fact that GHC may assume that
'catchException' (and the 'catch#' primitive operation which it is built upon)
is strict in its first argument. This assumption allows the compiler to better
optimize @catchException@ calls at the expense of deterministic behavior when
the action may be bottom.

Namely, the assumed strictness means that exceptions thrown while evaluating the
action-to-be-executed may not be caught; only exceptions thrown during execution
of the action will be handled by the exception handler.

Since this strictness is a small optimization and may lead to surprising
results, all of the @catch@ and @handle@ variants offered by "Control.Exception"
are lazy in their first argument. If you are certain that that the action to be
executed won't bottom in performance-sensitive code, you might consider using
'GHC.IO.catchException' or 'GHC.IO.catchAny' for a small speed-up.
-}
