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

-- #hide
module GHC.IO (
        IO(..), unIO, failIO, liftIO,
        unsafePerformIO, unsafeInterleaveIO,
        unsafeDupablePerformIO, unsafeDupableInterleaveIO,
        noDuplicate,

        -- To and from from ST
        stToIO, ioToST, unsafeIOToST, unsafeSTToIO,

        FilePath,

        catchException, catchAny, throwIO,
        mask, mask_, uninterruptibleMask, uninterruptibleMask_, 
        MaskingState(..), getMaskingState,
        block, unblock, blocked, unsafeUnmask,
        onException, bracket, finally, evaluate
    ) where

import GHC.Base
import GHC.ST
import GHC.Exception
import GHC.Show
import Data.Maybe

import {-# SOURCE #-} GHC.IO.Exception ( userError )

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

-- ---------------------------------------------------------------------------
-- Unsafe IO operations

{-|
This is the \"back door\" into the 'IO' monad, allowing
'IO' computation to be performed at any time.  For
this to be safe, the 'IO' computation should be
free of side effects and independent of its environment.

If the I\/O computation wrapped in 'unsafePerformIO' performs side
effects, then the relative order in which those side effects take
place (relative to the main I\/O trunk, or other calls to
'unsafePerformIO') is indeterminate.  Furthermore, when using
'unsafePerformIO' to cause side-effects, you should take the following
precautions to ensure the side effects are performed as many times as
you expect them to be.  Note that these precautions are necessary for
GHC, but may not be sufficient, and other compilers may require
different precautions:

  * Use @{\-\# NOINLINE foo \#-\}@ as a pragma on any function @foo@
        that calls 'unsafePerformIO'.  If the call is inlined,
        the I\/O may be performed more than once.

  * Use the compiler flag @-fno-cse@ to prevent common sub-expression
        elimination being performed on the module, which might combine
        two side effects that were meant to be separate.  A good example
        is using multiple global variables (like @test@ in the example below).

  * Make sure that the either you switch off let-floating (@-fno-full-laziness@), or that the 
        call to 'unsafePerformIO' cannot float outside a lambda.  For example, 
        if you say:
        @
           f x = unsafePerformIO (newIORef [])
        @
        you may get only one reference cell shared between all calls to @f@.
        Better would be
        @
           f x = unsafePerformIO (newIORef [x])
        @
        because now it can't float outside the lambda.

It is less well known that
'unsafePerformIO' is not type safe.  For example:

>     test :: IORef [a]
>     test = unsafePerformIO $ newIORef []
>     
>     main = do
>             writeIORef test [42]
>             bang <- readIORef test
>             print (bang :: [Char])

This program will core dump.  This problem with polymorphic references
is well known in the ML community, and does not arise with normal
monadic use of references.  There is no easy way to make it impossible
once you use 'unsafePerformIO'.  Indeed, it is
possible to write @coerce :: a -> b@ with the
help of 'unsafePerformIO'.  So be careful!
-}
unsafePerformIO :: IO a -> a
unsafePerformIO m = unsafeDupablePerformIO (noDuplicate >> m)

{-| 
This version of 'unsafePerformIO' is more efficient
because it omits the check that the IO is only being performed by a
single thread.  Hence, when you use 'unsafeDupablePerformIO',
there is a possibility that the IO action may be performed multiple
times (on a multiprocessor), and you should therefore ensure that
it gives the same results each time.
-}
{-# NOINLINE unsafeDupablePerformIO #-}
unsafeDupablePerformIO  :: IO a -> a
unsafeDupablePerformIO (IO m) = lazy (case m realWorld# of (# _, r #) -> r)

-- Why do we NOINLINE unsafeDupablePerformIO?  See the comment with
-- GHC.ST.runST.  Essentially the issue is that the IO computation
-- inside unsafePerformIO must be atomic: it must either all run, or
-- not at all.  If we let the compiler see the application of the IO
-- to realWorld#, it might float out part of the IO.

-- Why is there a call to 'lazy' in unsafeDupablePerformIO?
-- If we don't have it, the demand analyser discovers the following strictness
-- for unsafeDupablePerformIO:  C(U(AV))
-- But then consider
--      unsafeDupablePerformIO (\s -> let r = f x in 
--                             case writeIORef v r s of (# s1, _ #) ->
--                             (# s1, r #)
-- The strictness analyser will find that the binding for r is strict,
-- (because of uPIO's strictness sig), and so it'll evaluate it before 
-- doing the writeIORef.  This actually makes tests/lib/should_run/memo002
-- get a deadlock!  
--
-- Solution: don't expose the strictness of unsafeDupablePerformIO,
--           by hiding it with 'lazy'

{-|
'unsafeInterleaveIO' allows 'IO' computation to be deferred lazily.
When passed a value of type @IO a@, the 'IO' will only be performed
when the value of the @a@ is demanded.  This is used to implement lazy
file reading, see 'System.IO.hGetContents'.
-}
{-# INLINE unsafeInterleaveIO #-}
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO m = unsafeDupableInterleaveIO (noDuplicate >> m)

-- We used to believe that INLINE on unsafeInterleaveIO was safe,
-- because the state from this IO thread is passed explicitly to the
-- interleaved IO, so it cannot be floated out and shared.
--
-- HOWEVER, if the compiler figures out that r is used strictly here,
-- then it will eliminate the thunk and the side effects in m will no
-- longer be shared in the way the programmer was probably expecting,
-- but can be performed many times.  In #5943, this broke our
-- definition of fixIO, which contains
--
--    ans <- unsafeInterleaveIO (takeMVar m)
--
-- after inlining, we lose the sharing of the takeMVar, so the second
-- time 'ans' was demanded we got a deadlock.  We could fix this with
-- a readMVar, but it seems wrong for unsafeInterleaveIO to sometimes
-- share and sometimes not (plus it probably breaks the noDuplicate).
-- So now, we do not inline unsafeDupableInterleaveIO.

{-# NOINLINE unsafeDupableInterleaveIO #-}
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO (IO m)
  = IO ( \ s -> let
                   r = case m s of (# _, res #) -> res
                in
                (# s, r #))

{-| 
Ensures that the suspensions under evaluation by the current thread
are unique; that is, the current thread is not evaluating anything
that is also under evaluation by another thread that has also executed
'noDuplicate'.

This operation is used in the definition of 'unsafePerformIO' to
prevent the IO action from being executed multiple times, which is usually
undesirable.
-}
noDuplicate :: IO ()
noDuplicate = IO $ \s -> case noDuplicate# s of s' -> (# s', () #)

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

catchException :: Exception e => IO a -> (e -> IO a) -> IO a
catchException (IO io) handler = IO $ catch# io handler'
    where handler' e = case fromException e of
                       Just e' -> unIO (handler e')
                       Nothing -> raiseIO# e

catchAny :: IO a -> (forall e . Exception e => e -> IO a) -> IO a
catchAny (IO io) handler = IO $ catch# io handler'
    where handler' (SomeException e) = unIO (handler e)

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

{-# DEPRECATED block "use Control.Exception.mask instead" #-} -- deprecated in 7.0
-- | Note: this function is deprecated, please use 'mask' instead.
--
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

{-# DEPRECATED unblock "use Control.Exception.mask instead" #-} -- deprecated in 7.0
-- | Note: this function is deprecated, please use 'mask' instead.
--
-- To re-enable asynchronous exceptions inside the scope of
-- 'block', 'unblock' can be
-- used.  It scopes in exactly the same way, so on exit from
-- 'unblock' asynchronous exception delivery will
-- be disabled again.
unblock :: IO a -> IO a
unblock = unsafeUnmask

unsafeUnmask :: IO a -> IO a
unsafeUnmask (IO io) = IO $ unmaskAsyncExceptions# io

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

{-# DEPRECATED blocked "use Control.Exception.getMaskingState instead" #-} -- deprecated in 7.2
-- | returns True if asynchronous exceptions are blocked in the
-- current thread.
blocked :: IO Bool
blocked = fmap (/= Unmasked) getMaskingState

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
-- Threads created by 'Control.Concurrent.forkIO' inherit the masked
-- state from the parent; that is, to start a thread in blocked mode,
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
    Unmasked -> block $ io unblock
    _        -> io id

uninterruptibleMask_ io = uninterruptibleMask $ \_ -> io

uninterruptibleMask io = do
  b <- getMaskingState
  case b of
    Unmasked              -> blockUninterruptible $ io unblock
    MaskedInterruptible   -> blockUninterruptible $ io block
    MaskedUninterruptible -> io id

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

-- | Forces its argument to be evaluated to weak head normal form when
-- the resultant 'IO' action is executed. It can be used to order
-- evaluation with respect to other 'IO' operations; its semantics are
-- given by
--
-- >   evaluate x `seq` y    ==>  y
-- >   evaluate x `catch` f  ==>  (return $! x) `catch` f
-- >   evaluate x >>= f      ==>  (return $! x) >>= f
--
-- /Note:/ the first equation implies that @(evaluate x)@ is /not/ the
-- same as @(return $! x)@.  A correct definition is
--
-- >   evaluate x = (return $! x) >>= return
--
evaluate :: a -> IO a
evaluate a = IO $ \s -> seq# a s -- NB. see #2273, #5129

