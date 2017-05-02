{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Unsafe
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Unsafe IO operations
--
-----------------------------------------------------------------------------

module GHC.IO.Unsafe (
    unsafePerformIO, unsafeInterleaveIO,
    unsafeDupablePerformIO, unsafeDupableInterleaveIO,
    noDuplicate,
  ) where

import GHC.Base
import GHC.MVar

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
it gives the same results each time. It may even happen that one
of the duplicated IO actions is only run partially, and then interrupted
in the middle without an exception being raised. Therefore, functions
like 'bracket' cannot be used safely within 'unsafeDupablePerformIO'.

@since 4.4.0.0
-}
unsafeDupablePerformIO  :: IO a -> a
unsafeDupablePerformIO (IO m) = case runRW# m of (# _, a #) -> a

{-|
'unsafeInterleaveIO' allows an 'IO' computation to be deferred lazily.
When passed a value of type @IO a@, the 'IO' will only be performed
when the value of the @a@ is demanded.  This is used to implement lazy
file reading, see 'System.IO.hGetContents'.
-}
{-# INLINE unsafeInterleaveIO #-}
unsafeInterleaveIO :: IO a -> IO a
-- See Note [Null pointers in unsafeInterleaveIO]
unsafeInterleaveIO m = do
  v <- case unclaimed of
         Box r -> unsafeCoerce# newMVar r
  unsafeDupableInterleaveIO $ do
  a <- takeMVar v
  if isUnclaimed a
    then do
      res <- m
      putMVar v res
      pure res
    else a <$ putMVar v a

-- The 'Unclaimed' constructor must not be exported.
data Unclaimed = Unclaimed
data Box = Box !Unclaimed

-- We use 'unclaimed' as a "null pointer" in 'unsafeInterleaveIO'.
-- It must not be exported!
-- See Note [Null pointers in unsafeInterleaveIO]
{-# NOINLINE unclaimed #-}
unclaimed :: Box
unclaimed = Box Unclaimed

isUnclaimed :: a -> Bool
isUnclaimed a = case unclaimed of
  Box r -> isTrue# (unsafeCoerce# reallyUnsafePtrEquality# a r)

-- Note [Null pointers in unsafeInterleaveIO]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Conceptually, we're implementing this:
--
-- unsafeInterleaveIO :: IO a -> IO a
-- unsafeInterleaveIO m = do
--   v <- newMVar Nothing
--   unsafeDupableInterleaveIO $ do
--     r <- takeMVar v
--     case r of
--       -- We're the first ones to get the MVar, so we actually
--       -- do the work.
--       Nothing -> do
--         a <- m
--         putMVar v (Just a)
--         pure a
--
--       -- Someone else has claimed the action, so we use
--       -- their result and put it back in the MVar.
--       j@(Just a) -> a <$ putMVar v j
--
-- The MVar starts out full, with Nothing in it. When the interleaved
-- computation is complete, the result will be stored in the MVar in a Just
-- constructor. The interleaved computation, which may run in multiple
-- threads, takes the MVar, checks whether it's Nothing or Just, and either
-- performs the interleaved computation or just puts the Just back.
--
-- However, allocating Just constructors is wasteful; we can pretend we're
-- writing in C and use a distinguished "null pointer" to represent Nothing
-- instead. We magic up a single, global null pointer and use that every time.
-- The usual problem with null pointers is that they can't distinguish, among
-- Nothing, Just Nothing, Just (Just Nothing), etc. Fortunately, we don't have
-- to worry about that here. The null pointer is private to this module, so
-- it is impossible for the computation passed to 'unsafeInterleaveIO' to
-- produce it.
--
-- Why do we have to build a box around the distinguished null? I don't
-- actually know. But without this box, 'reallyUnsafePtrEquality#' does not
-- seem to detect equality! Note that we rely on the fact that GHC uses
-- distinct heap locations to represent nullary constructors of distinct
-- datatypes. If this changes, we can recover the correct behavior by using
-- 'unsafePerformIO' to allocate something like an 'IORef' and use the
-- embedded 'MutVar#' as a null pointer.

-- Note [unsafeDupableInterleaveIO should not be inlined]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We used to believe that INLINE on unsafeDupableInterleaveIO was safe,
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

{-|
'unsafeDupableInterleaveIO' allows an 'IO' computation to be deferred lazily.
When passed a value of type @IO a@, the 'IO' will only be performed
when the value of the @a@ is demanded.

The computation may be performed multiple times by different threads,
possibly at the same time. To ensure that the computation is performed
only once, use 'unsafeInterleaveIO' instead.
-}

{-# NOINLINE unsafeDupableInterleaveIO #-}
-- See Note [unsafeDupableInterleaveIO should not be inlined]
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
