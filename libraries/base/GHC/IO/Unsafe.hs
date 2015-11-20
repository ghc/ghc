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

-- Note [unsafeDupablePerformIO is NOINLINE]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Why do we NOINLINE unsafeDupablePerformIO?  See the comment with
-- GHC.ST.runST.  Essentially the issue is that the IO computation
-- inside unsafePerformIO must be atomic: it must either all run, or
-- not at all.  If we let the compiler see the application of the IO
-- to realWorld#, it might float out part of the IO.

-- Note [unsafeDupablePerformIO has a lazy RHS]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Why is there a call to 'lazy' in unsafeDupablePerformIO?
-- If we don't have it, the demand analyser discovers the following strictness
-- for unsafeDupablePerformIO:  C(U(AV))
-- But then consider
--      unsafeDupablePerformIO (\s -> let r = f x in
--                             case writeIORef v r s of (# s1, _ #) ->
--                             (# s1, r #) )
-- The strictness analyser will find that the binding for r is strict,
-- (because of uPIO's strictness sig), and so it'll evaluate it before
-- doing the writeIORef.  This actually makes libraries/base/tests/memo002
-- get a deadlock, where we specifically wanted to write a lazy thunk
-- into the ref cell.
--
-- Solution: don't expose the strictness of unsafeDupablePerformIO,
--           by hiding it with 'lazy'
-- But see discussion in Trac #9390 (comment:33)

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
