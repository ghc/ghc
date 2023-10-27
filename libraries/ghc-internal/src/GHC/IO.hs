{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , RankNTypes
           , MagicHash
           , ScopedTypeVariables
           , UnboxedTuples
           , PolyKinds
           , DataKinds
  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO
-- Copyright   :  (c) The University of Glasgow 1994-2023
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Definitions for the 'IO' monad and its friends.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

module GHC.IO (
        IO(..), unIO, liftIO, mplusIO,
        unsafePerformIO, unsafeInterleaveIO,
        unsafeDupablePerformIO, unsafeDupableInterleaveIO,
        noDuplicate,

        -- To and from ST
        stToIO, ioToST, unsafeIOToST, unsafeSTToIO,

        FilePath,

        catch, catchException, catchAny, raiseIO#, throwIO,
        mask, mask_, uninterruptibleMask, uninterruptibleMask_,
        MaskingState(..), getMaskingState,
        unsafeUnmask, interruptible,
        onException, bracket, finally, evaluate, evaluate2, strictnessBarrier,
        mkUserError
    ) where

import GHC.Base
import GHC.ST
import GHC.Exception
import GHC.Show
import GHC.IO.Unsafe
import Unsafe.Coerce ( unsafeCoerce )

import {-# SOURCE #-} GHC.IO.Exception ( userError, IOError )

-- ---------------------------------------------------------------------------
-- The IO Monad

{-
The IO Monad is just an instance of the ST monad, where the state thread
is the real world.  We use the exception mechanism (in GHC.Exception) to
implement IO exceptions.

NOTE: The IO representation is deeply wired in to various parts of the
system.  The following list may or may not be exhaustive:

Compiler  - types of various primitives in GHC.Builtin.PrimOps

RTS       - forceIO (StgStartup.cmm)
          - catchzh_fast, (un)?blockAsyncExceptionszh_fast, raisezh_fast
            (Exception.cmm)
          - raiseAsync (RaiseAsync.c)

Prelude   - GHC.IO.hs, and several other places including
            GHC.Exception.hs.

Libraries - parts of hslibs/lang.

--SDM
-}

liftIO :: IO a -> State# RealWorld -> STret RealWorld a
liftIO (IO m) = \s -> case m s of (# s', r #) -> STret s' r

-- ---------------------------------------------------------------------------
-- Coercions between IO and ST

-- | Embed a strict state thread in an 'IO'
-- action.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO        :: ST RealWorld a -> IO a
stToIO (ST m) = IO m

-- | Convert an 'IO' action into an 'ST' action. The type of the result
-- is constrained to use a 'RealWorld' state thread, and therefore the
-- result cannot be passed to 'runST'.
ioToST        :: IO a -> ST RealWorld a
ioToST (IO m) = (ST m)

-- | Convert an 'IO' action to an 'ST' action.
-- This relies on 'IO' and 'ST' having the same representation modulo the
-- constraint on the state thread type parameter.
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST (IO io) = ST $ \ s -> (unsafeCoerce io) s

-- | Convert an 'ST' action to an 'IO' action.
-- This relies on 'IO' and 'ST' having the same representation modulo the
-- constraint on the state thread type parameter.
--
-- For an example demonstrating why this is unsafe, see
-- https://mail.haskell.org/pipermail/haskell-cafe/2009-April/060719.html
unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST m) = IO (unsafeCoerce m)

-- -----------------------------------------------------------------------------
-- | File and directory names are values of type 'String', whose precise
-- meaning is operating system dependent. Files can be opened, yielding a
-- handle which can then be used to operate on the contents of that file.

type FilePath = String

-- -----------------------------------------------------------------------------
-- Primitive catch and throwIO

{-
catchException/catch used to handle the passing around of the state to the
action and the handler.  This turned out to be a bad idea - it meant
that we had to wrap both arguments in thunks so they could be entered
as normal (remember IO returns an unboxed pair...).

Now catch# has type

    catch# :: IO a -> (b -> IO a) -> IO a

(well almost; the compiler doesn't know about the IO newtype so we
have to work around that in the definition of catch below).
-}

-- | Catch an exception in the 'IO' monad.
--
-- Note that this function is /strict/ in the action. That is,
-- @catchException undefined b == _|_@. See #exceptions_and_strictness#
-- for details.
catchException :: Exception e => IO a -> (e -> IO a) -> IO a
catchException !io handler = catch io handler

-- | This is the simplest of the exception-catching functions.  It
-- takes a single argument, runs it, and if an exception is raised
-- the \"handler\" is executed, with the value of the exception passed as an
-- argument.  Otherwise, the result is returned as normal.  For example:
--
-- >   catch (readFile f)
-- >         (\e -> do let err = show (e :: IOException)
-- >                   hPutStr stderr ("Warning: Couldn't open " ++ f ++ ": " ++ err)
-- >                   return "")
--
-- Note that we have to give a type signature to @e@, or the program
-- will not typecheck as the type is ambiguous. While it is possible
-- to catch exceptions of any type, see the section \"Catching all
-- exceptions\" (in "Control.Exception") for an explanation of the problems with doing so.
--
-- For catching exceptions in pure (non-'IO') expressions, see the
-- function 'evaluate'.
--
-- Note that due to Haskell\'s unspecified evaluation order, an
-- expression may throw one of several possible exceptions: consider
-- the expression @(error \"urk\") + (1 \`div\` 0)@.  Does
-- the expression throw
-- @ErrorCall \"urk\"@, or @DivideByZero@?
--
-- The answer is \"it might throw either\"; the choice is
-- non-deterministic. If you are catching any type of exception then you
-- might catch either. If you are calling @catch@ with type
-- @IO Int -> (ArithException -> IO Int) -> IO Int@ then the handler may
-- get run with @DivideByZero@ as an argument, or an @ErrorCall \"urk\"@
-- exception may be propagated further up. If you call it again, you
-- might get the opposite behaviour. This is ok, because 'catch' is an
-- 'IO' computation.
--
catch   :: Exception e
        => IO a         -- ^ The computation to run
        -> (e -> IO a)  -- ^ Handler to invoke if an exception is raised
        -> IO a
-- See #exceptions_and_strictness#.
catch (IO io) handler = IO $ catch# io handler'
    where handler' e = case fromException e of
                       Just e' -> unIO (handler e')
                       Nothing -> raiseIO# e


-- | Catch any 'Exception' type in the 'IO' monad.
--
-- Note that this function is /strict/ in the action. That is,
-- @catchAny undefined b == _|_@. See #exceptions_and_strictness# for
-- details.
catchAny :: IO a -> (forall e . Exception e => e -> IO a) -> IO a
catchAny !(IO io) handler = IO $ catch# io handler'
    where handler' (SomeException e) = unIO (handler e)

-- Using catchException here means that if `m` throws an
-- 'IOError' /as an imprecise exception/, we will not catch
-- it. No one should really be doing that anyway.
mplusIO :: IO a -> IO a -> IO a
mplusIO m n = m `catchException` \ (_ :: IOError) -> n

raiseIO# :: forall (l :: Levity) (r :: RuntimeRep) (a :: TYPE ('BoxedRep l)) (b :: TYPE r). a -> State# RealWorld -> (# State# RealWorld, b #)
raiseIO# = unsafeCoerce $ \a s -> case strictnessBarrier# s of _s' -> raise# a
{-# NOINLINE raiseIO# #-}

-- | A variant of 'throw' that can only be used within the 'IO' monad.
--
-- Although 'throwIO' has a type that is an instance of the type of 'throw', the
-- two functions are subtly different:
--
-- > throw e   `seq` ()  ===> throw e
-- > throwIO e `seq` ()  ===> ()
--
-- The first example will cause the exception @e@ to be raised,
-- whereas the second one won\'t.  In fact, 'throwIO' will only cause
-- an exception to be raised when it is used within the 'IO' monad.
--
-- The 'throwIO' variant should be used in preference to 'throw' to
-- raise an exception within the 'IO' monad because it guarantees
-- ordering with respect to other operations, whereas 'throw'
-- does not. We say that 'throwIO' throws *precise* exceptions and
-- 'throw', 'error', etc. all throw *imprecise* exceptions.
-- For example
--
-- > throw e + error "boom" ===> error "boom"
-- > throw e + error "boom" ===> throw e
--
-- are both valid reductions and the compiler may pick any (loop, even), whereas
--
-- > throwIO e >> error "boom" ===> throwIO e
--
-- will always throw @e@ when executed.
--
-- See also the
-- [GHC wiki page on precise exceptions](https://gitlab.haskell.org/ghc/ghc/-/wikis/exceptions/precise-exceptions)
-- for a more technical introduction to how GHC optimises around precise vs.
-- imprecise exceptions.
--
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
 deriving ( Eq   -- ^ @since 4.3.0.0
          , Show -- ^ @since 4.3.0.0
          )

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
-- asynchronous exceptions are received.  To create a new thread in
-- an unmasked state use 'Control.Concurrent.forkIOWithUnmask'.
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
-- may contain, and possibly handle them, before the rest of the program is
-- executed.
--
-- 'evaluate' only evaluates to /weak head normal form/. If deeper
-- evaluation is needed, the @force@ function from @Control.DeepSeq@
-- may be handy: @evaluate $ force x@.
--
-- The semantics of 'evaluate' is to be taken with care.
-- Consider the following example:
--
-- > f :: Int -> Int -> IO ()
-- > f x y = evaluate y >> print $! x
-- > {-# NOINLINE f #-}
-- > main = f (error "x") (error "y")
--
-- 'evaluate' guarantees that no side-effect that follows (e.g., @print $! x@)
-- will be observed before @y@ has been evaluated.
-- However, it does /not/ guarantee that evaluation of @y@ will be observed
-- at all!
-- The program above will error out with "y" without optimisations but error out
-- with "x" with optimisations, because `x` will be unboxed and thus eagerly
-- evaluated.
--
-- We provide 'evaluate2' as a saner alternative, and keep 'evaluate' only for
-- backwards compatibility.
evaluate :: a -> IO a
-- See #implementation_of_evaluate#.
evaluate y = IO $ \s -> seq# (lazy y) s

-- | Evaluate the argument to weak head normal form.
--
-- 'evaluate2' is typically used to uncover any exceptions that a lazy value
-- may contain, and possibly handle them.
--
-- 'evaluate2' only evaluates to /weak head normal form/. If deeper
-- evaluation is needed, the @force@ function from @Control.DeepSeq@
-- may be handy: @evaluate2 $ force x@.
--
-- 'evaluate2' is an evolution of 'evaluate' with saner semantics.
--
-- How is @evaluate2 x@ different to @x `seq` return ()@?
-- Consider the following example:
--
-- > f1 :: Int -> Int -> IO ()
-- > f1 x y = y `seq` print x
-- > f2 :: Int -> Int -> IO ()
-- > f2 x y = evaluate2 y >> print $! x
-- > {-# NOINLINE f1, f2 #-}
-- > main = f (error "x") (error "y")
--
-- If @f = f1@, then you will observe @error "y"@ without optimisations
-- and @error "x"@ with optimisations, because GHC is eager to unbox
-- the strictly used @x@.
-- There are many more examples like this.
--
-- If on the other @f = f2@, then the user will always observe @error "y"@,
-- regardless of optimisation level.
--
-- More precisely, 'evaluate2' guarantees that no side-effect that follows
-- (e.g., @print $! x@) will be observed before @y@ has been evaluated.
-- Moreover (and in contrast to 'evaluate'), it guarantees that evaluation of
-- @y@ will be observed whenever @f@ is called.
--
-- The rule of thumb is to use 'evaluate2' to observe evaluation, for example to
-- squeeze out an exception in lazy values, at exactly the point in the
-- @do@-block where it is called.
-- Doing so comes at a performance price; for example in the program above,
-- neither @x@ nor @y@ are unboxed.
-- If, on the other hand, you are forcing a lazy value for efficiency reasons
-- only and do not care about evaluation order, you should use 'seq' or
-- bang patterns.
evaluate2 :: a -> IO a
-- See #implementation_of_evaluate2#.
evaluate2 y = IO $ \s ->
  case seq# (lazy y) s of
    (# s1, y' #) -> case strictnessBarrier# s1 of
      s2 -> (# s2, y' #)

-- | 'strictnessBarrier' is used to tell GHC to discard any strict uses it
-- infers for what comes after it. For example, consider
--
-- > import Data.IORef
-- > import Control.Exception
-- >
-- > f :: Int -> IORef Bool -> IO ()
-- > f x ref = do
-- >   atomicWriteIORef ref True
-- >   print $! x
-- > {-# NOINLINE f #-}
-- >
-- > main = do
-- >   ref <- newIORef False
-- >   catch (f (error "x") ref)
-- >         (\(_ :: SomeException) ->  readIORef ref >>= print)
--
-- @f@ should write to the 'IORef' before evaluating @x@.
-- Indeed, if compiled with @-O0@, the exception handler will print @True@.
-- But not so with @-O1@; then the program will print @False@.
-- That is because GHC sees @f@ as strict in @x@, so it will unbox it and
-- evaluate @x@ before the call.
-- Hence the error will be thrown before @True@ has been written to @ref@.
--
-- With 'strictnessBarrier', we could have written
--
-- > f x ref = do
-- >   atomicWriteIORef ref True
-- >   strictnessBarrier
-- >   print $! x
--
-- and now GHC will no longer unbox @x@, because we told it to forget any strict
-- uses in the continuation @print $! x@.
strictnessBarrier :: IO ()
strictnessBarrier = IO (\s -> (# strictnessBarrier# s, () #))

{- $exceptions_and_strictness

Laziness can interact with @catch@-like operations in non-obvious ways (see,
e.g. GHC #11555 and #13330). For instance, consider these subtly-different
examples:

> test1 = Control.Exception.catch (error "uh oh") (\(_ :: SomeException) -> putStrLn "it failed")
>
> test2 = GHC.IO.catchException (error "uh oh") (\(_ :: SomeException) -> putStrLn "it failed")

While @test1@ will print "it failed", @test2@ will print "uh oh".

When using 'catchException', exceptions thrown while evaluating the
action-to-be-executed will not be caught; only exceptions thrown during
execution of the action will be handled by the exception handler.

Since this strictness is a small optimization and may lead to surprising
results, all of the @catch@ and @handle@ variants offered by "Control.Exception"
use 'catch' rather than 'catchException'.
-}

{- $legacy_implementation_of_evaluate

Related tickets: #2273, #5129, #15226, #22935, #23847

First read the haddock on 'evaluate'. The main example is

  f :: Int -> Int -> IO ()
  f x y = evaluate y >> print $! x
  {-# NOINLINE f #-}
  main = f (error "x") (error "y")

The important bit is

> @evaluate y@ guarantees that no side-effect that follows (e.g., @print $! x@)
> will be observed before @y@ has been evaluated.
> However, it does /not/ guarantee that evaluation of @y@ will be observed at
> all!

This reflects as follows in the implementation of 'evaluate':

1. We `seq#` the input value `y` (see Note [seq# magic] in GHC.Types.Id.Make),
   so that the input is really evaluated before the next side-effect.
2. For backwards compatibility, `y` is wrapped in `lazy`
   (see Note [lazyId magic]). Presumably the intention of `evaluate` being lazy
   was so that

     f x y = evaluate y >> evaluate x

   is not discovered as strict in both `x` and `y` by strictness analysis, which
   would subsequently lead to unboxing and thus evaluating `x` before `y`,
   despite what the user probably intended.

   But this is all in vain, as the haddock explains (for @evaluate y >> print x@).
   Hence this Note is merely to *document* the status quo, not to justify it.
-}

{- $implementation_of_evaluate2

Related tickets: #2273, #5129, #15226, #22935, #23847

First read the haddock on 'evaluate2'. The main example is

  f :: Int -> Int -> IO ()
  f x y = evaluate y >> print $! x
  {-# NOINLINE f #-}
  main = f (error "x") (error "y")

The important bit specifying the semantics of 'evaluate2' is

> @evaluate2 y@ guarantees that no side-effect that follows (e.g., @print $! x@)
> will be observed before @y@ has been evaluated.
> Moreover (and in contrast to 'evaluate'), it guarantees that evaluation of @y@
> will be observed whenever @f@ is called.

This reflects as follows in the implementation of 'evaluate':

  evaluate2 y = IO $ \s ->
    case seq# (lazy y) s of (# s1, y' #) ->
      case strictnessBarrier# s1 of s2 ->
        (# s2, y' #)

1. We @seq#@ the input value @y@ (see Note [seq# magic] in GHC.Types.Id.Make), so
   that the input is really evaluated before the next side-effect.
2. We wrap the input in @lazy@ so that strictness analysis does not
   evaluate @y@ before a /previous/ side-effect, e.g.,
     f a b = print b >> evaluate a
   should print @b@ before evaluating @a@, and without @lazy@ strictness
   analysis would evaluate @a@ before the call to @f@.
   The user can always write @evaluate $! a@ to recover the non-@lazy@ behavior.
3. To guarantee that nothing in @f@ is evaluated before @y@ (in particular, @x@)
   this is followed up with a `strictnessBarrier#` (see Note [strictnessBarrier# magic]),
   so that strictness analysis forgets that `x` is evaluated strictly.
-}

-- For SOURCE import by GHC.Base to define failIO.
mkUserError       :: [Char]  -> SomeException
mkUserError str   = toException (userError str)
