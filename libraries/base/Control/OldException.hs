{-# OPTIONS_GHC -XNoImplicitPrelude #-}

#include "Typeable.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.OldException
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (extended exceptions)
--
-- This module provides support for raising and catching both built-in
-- and user-defined exceptions.
--
-- In addition to exceptions thrown by 'IO' operations, exceptions may
-- be thrown by pure code (imprecise exceptions) or by external events
-- (asynchronous exceptions), but may only be caught in the 'IO' monad.
-- For more details, see:
--
--  * /A semantics for imprecise exceptions/, by Simon Peyton Jones,
--    Alastair Reid, Tony Hoare, Simon Marlow, Fergus Henderson,
--    in /PLDI'99/.
--
--  * /Asynchronous exceptions in Haskell/, by Simon Marlow, Simon Peyton
--    Jones, Andy Moran and John Reppy, in /PLDI'01/.
--
-----------------------------------------------------------------------------

module Control.OldException {-# DEPRECATED "Future versions of base will not support the old exceptions style. Please switch to extensible exceptions." #-} (

        -- * The Exception type
        Exception(..),          -- instance Eq, Ord, Show, Typeable
        New.IOException,        -- instance Eq, Ord, Show, Typeable
        New.ArithException(..), -- instance Eq, Ord, Show, Typeable
        New.ArrayException(..), -- instance Eq, Ord, Show, Typeable
        New.AsyncException(..), -- instance Eq, Ord, Show, Typeable

        -- * Throwing exceptions
        throwIO,        -- :: Exception -> IO a
        throw,          -- :: Exception -> a
        ioError,        -- :: IOError -> IO a
#ifdef __GLASGOW_HASKELL__
        -- XXX Need to restrict the type of this:
        New.throwTo,        -- :: ThreadId -> Exception -> a
#endif

        -- * Catching Exceptions

        -- |There are several functions for catching and examining
        -- exceptions; all of them may only be used from within the
        -- 'IO' monad.

        -- ** The @catch@ functions
        catch,     -- :: IO a -> (Exception -> IO a) -> IO a
        catchJust, -- :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a

        -- ** The @handle@ functions
        handle,    -- :: (Exception -> IO a) -> IO a -> IO a
        handleJust,-- :: (Exception -> Maybe b) -> (b -> IO a) -> IO a -> IO a

        -- ** The @try@ functions
        try,       -- :: IO a -> IO (Either Exception a)
        tryJust,   -- :: (Exception -> Maybe b) -> a    -> IO (Either b a)

        -- ** The @evaluate@ function
        evaluate,  -- :: a -> IO a

        -- ** The @mapException@ function
        mapException,           -- :: (Exception -> Exception) -> a -> a

        -- ** Exception predicates
        
        -- $preds

        ioErrors,               -- :: Exception -> Maybe IOError
        arithExceptions,        -- :: Exception -> Maybe ArithException
        errorCalls,             -- :: Exception -> Maybe String
        dynExceptions,          -- :: Exception -> Maybe Dynamic
        assertions,             -- :: Exception -> Maybe String
        asyncExceptions,        -- :: Exception -> Maybe AsyncException
        userErrors,             -- :: Exception -> Maybe String

        -- * Dynamic exceptions

        -- $dynamic
        throwDyn,       -- :: Typeable ex => ex -> b
#ifdef __GLASGOW_HASKELL__
        throwDynTo,     -- :: Typeable ex => ThreadId -> ex -> b
#endif
        catchDyn,       -- :: Typeable ex => IO a -> (ex -> IO a) -> IO a
        
        -- * Asynchronous Exceptions

        -- $async

        -- ** Asynchronous exception control

        -- |The following two functions allow a thread to control delivery of
        -- asynchronous exceptions during a critical region.

        block,          -- :: IO a -> IO a
        unblock,        -- :: IO a -> IO a

        -- *** Applying @block@ to an exception handler

        -- $block_handler

        -- *** Interruptible operations

        -- $interruptible

        -- * Assertions

        assert,         -- :: Bool -> a -> a

        -- * Utilities

        bracket,        -- :: IO a -> (a -> IO b) -> (a -> IO c) -> IO ()
        bracket_,       -- :: IO a -> IO b -> IO c -> IO ()
        bracketOnError,

        finally,        -- :: IO a -> IO b -> IO a
        
#ifdef __GLASGOW_HASKELL__
        setUncaughtExceptionHandler,      -- :: (Exception -> IO ()) -> IO ()
        getUncaughtExceptionHandler       -- :: IO (Exception -> IO ())
#endif
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Num
import GHC.Show
-- import GHC.IO ( IO )
import GHC.IO.Handle.FD ( stdout )
import qualified GHC.IO as New
import qualified GHC.IO.Exception as New
import GHC.Conc hiding (setUncaughtExceptionHandler,
                        getUncaughtExceptionHandler)
import Data.IORef       ( IORef, newIORef, readIORef, writeIORef )
import Foreign.C.String ( CString, withCString )
import GHC.IO.Handle ( hFlush )
#endif

#ifdef __HUGS__
import Prelude          hiding (catch)
import Hugs.Prelude     as New (ExitCode(..))
#endif

import qualified Control.Exception as New
import           Control.Exception ( toException, fromException, throw, block, unblock, mask, evaluate, throwIO )
import System.IO.Error  hiding ( catch, try )
import System.IO.Unsafe (unsafePerformIO)
import Data.Dynamic
import Data.Either
import Data.Maybe

#ifdef __NHC__
import System.IO.Error (catch, ioError)
import IO              (bracket)
import DIOError         -- defn of IOError type

-- minimum needed for nhc98 to pretend it has Exceptions
type Exception   = IOError
type IOException = IOError
data ArithException
data ArrayException
data AsyncException

throwIO  :: Exception -> IO a
throwIO   = ioError
throw    :: Exception -> a
throw     = unsafePerformIO . throwIO

evaluate :: a -> IO a
evaluate x = x `seq` return x

ioErrors        :: Exception -> Maybe IOError
ioErrors e       = Just e
arithExceptions :: Exception -> Maybe ArithException
arithExceptions  = const Nothing
errorCalls      :: Exception -> Maybe String
errorCalls       = const Nothing
dynExceptions   :: Exception -> Maybe Dynamic
dynExceptions    = const Nothing
assertions      :: Exception -> Maybe String
assertions       = const Nothing
asyncExceptions :: Exception -> Maybe AsyncException
asyncExceptions  = const Nothing
userErrors      :: Exception -> Maybe String
userErrors (UserError _ s) = Just s
userErrors  _              = Nothing

block   :: IO a -> IO a
block    = id
unblock :: IO a -> IO a
unblock  = id

assert :: Bool -> a -> a
assert True  x = x
assert False _ = throw (UserError "" "Assertion failed")
#endif

-----------------------------------------------------------------------------
-- Catching exceptions

-- |This is the simplest of the exception-catching functions.  It
-- takes a single argument, runs it, and if an exception is raised
-- the \"handler\" is executed, with the value of the exception passed as an
-- argument.  Otherwise, the result is returned as normal.  For example:
--
-- >   catch (openFile f ReadMode) 
-- >       (\e -> hPutStr stderr ("Couldn't open "++f++": " ++ show e))
--
-- For catching exceptions in pure (non-'IO') expressions, see the
-- function 'evaluate'.
--
-- Note that due to Haskell\'s unspecified evaluation order, an
-- expression may return one of several possible exceptions: consider
-- the expression @error \"urk\" + 1 \`div\` 0@.  Does
-- 'catch' execute the handler passing
-- @ErrorCall \"urk\"@, or @ArithError DivideByZero@?
--
-- The answer is \"either\": 'catch' makes a
-- non-deterministic choice about which exception to catch.  If you
-- call it again, you might get a different exception back.  This is
-- ok, because 'catch' is an 'IO' computation.
--
-- Note that 'catch' catches all types of exceptions, and is generally
-- used for \"cleaning up\" before passing on the exception using
-- 'throwIO'.  It is not good practice to discard the exception and
-- continue, without first checking the type of the exception (it
-- might be a 'ThreadKilled', for example).  In this case it is usually better
-- to use 'catchJust' and select the kinds of exceptions to catch.
--
-- Also note that the "Prelude" also exports a function called
-- 'Prelude.catch' with a similar type to 'Control.OldException.catch',
-- except that the "Prelude" version only catches the IO and user
-- families of exceptions (as required by Haskell 98).  
--
-- We recommend either hiding the "Prelude" version of 'Prelude.catch'
-- when importing "Control.OldException": 
--
-- > import Prelude hiding (catch)
--
-- or importing "Control.OldException" qualified, to avoid name-clashes:
--
-- > import qualified Control.OldException as C
--
-- and then using @C.catch@
--

catch   :: IO a                 -- ^ The computation to run
        -> (Exception -> IO a)  -- ^ Handler to invoke if an exception is raised
        -> IO a
-- note: bundling the exceptions is done in the New.Exception
-- instance of Exception; see below.
catch = New.catch

-- | The function 'catchJust' is like 'catch', but it takes an extra
-- argument which is an /exception predicate/, a function which
-- selects which type of exceptions we\'re interested in.  There are
-- some predefined exception predicates for useful subsets of
-- exceptions: 'ioErrors', 'arithExceptions', and so on.  For example,
-- to catch just calls to the 'error' function, we could use
--
-- >   result <- catchJust errorCalls thing_to_try handler
--
-- Any other exceptions which are not matched by the predicate
-- are re-raised, and may be caught by an enclosing
-- 'catch' or 'catchJust'.
catchJust
        :: (Exception -> Maybe b) -- ^ Predicate to select exceptions
        -> IO a                   -- ^ Computation to run
        -> (b -> IO a)            -- ^ Handler
        -> IO a
catchJust p a handler = catch a handler'
  where handler' e = case p e of 
                        Nothing -> throw e
                        Just b  -> handler b

-- | A version of 'catch' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.  For example:
--
-- >   do handle (\e -> exitWith (ExitFailure 1)) $
-- >      ...
handle     :: (Exception -> IO a) -> IO a -> IO a
handle     =  flip catch

-- | A version of 'catchJust' with the arguments swapped around (see
-- 'handle').
handleJust :: (Exception -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust p =  flip (catchJust p)

-----------------------------------------------------------------------------
-- 'mapException'

-- | This function maps one exception into another as proposed in the
-- paper \"A semantics for imprecise exceptions\".

-- Notice that the usage of 'unsafePerformIO' is safe here.

mapException :: (Exception -> Exception) -> a -> a
mapException f v = unsafePerformIO (catch (evaluate v)
                                          (\x -> throw (f x)))

-----------------------------------------------------------------------------
-- 'try' and variations.

-- | Similar to 'catch', but returns an 'Either' result which is
-- @('Right' a)@ if no exception was raised, or @('Left' e)@ if an
-- exception was raised and its value is @e@.
--
-- >  try a = catch (Right `liftM` a) (return . Left)
--
-- Note: as with 'catch', it is only polite to use this variant if you intend
-- to re-throw the exception after performing whatever cleanup is needed.
-- Otherwise, 'tryJust' is generally considered to be better.
--
-- Also note that "System.IO.Error" also exports a function called
-- 'System.IO.Error.try' with a similar type to 'Control.OldException.try',
-- except that it catches only the IO and user families of exceptions
-- (as required by the Haskell 98 @IO@ module).

try :: IO a -> IO (Either Exception a)
try a = catch (a >>= \ v -> return (Right v)) (\e -> return (Left e))

-- | A variant of 'try' that takes an exception predicate to select
-- which exceptions are caught (c.f. 'catchJust').  If the exception
-- does not match the predicate, it is re-thrown.
tryJust :: (Exception -> Maybe b) -> IO a -> IO (Either b a)
tryJust p a = do
  r <- try a
  case r of
        Right v -> return (Right v)
        Left  e -> case p e of
                        Nothing -> throw e
                        Just b  -> return (Left b)

-----------------------------------------------------------------------------
-- Dynamic exceptions

-- $dynamic
--  #DynamicExceptions# Because the 'Exception' datatype is not extensible, there is an
-- interface for throwing and catching exceptions of type 'Dynamic'
-- (see "Data.Dynamic") which allows exception values of any type in
-- the 'Typeable' class to be thrown and caught.

-- | Raise any value as an exception, provided it is in the
-- 'Typeable' class.
throwDyn :: Typeable exception => exception -> b
#ifdef __NHC__
throwDyn exception = throw (UserError "" "dynamic exception")
#else
throwDyn exception = throw (DynException (toDyn exception))
#endif

#ifdef __GLASGOW_HASKELL__
-- | A variant of 'throwDyn' that throws the dynamic exception to an
-- arbitrary thread (GHC only: c.f. 'throwTo').
throwDynTo :: Typeable exception => ThreadId -> exception -> IO ()
throwDynTo t exception = New.throwTo t (DynException (toDyn exception))
#endif /* __GLASGOW_HASKELL__ */

-- | Catch dynamic exceptions of the required type.  All other
-- exceptions are re-thrown, including dynamic exceptions of the wrong
-- type.
--
-- When using dynamic exceptions it is advisable to define a new
-- datatype to use for your exception type, to avoid possible clashes
-- with dynamic exceptions used in other libraries.
--
catchDyn :: Typeable exception => IO a -> (exception -> IO a) -> IO a
#ifdef __NHC__
catchDyn m k = m        -- can't catch dyn exceptions in nhc98
#else
catchDyn m k = New.catch m handler
  where handler ex = case ex of
                           (DynException dyn) ->
                                case fromDynamic dyn of
                                    Just exception  -> k exception
                                    Nothing -> throw ex
                           _ -> throw ex
#endif

-----------------------------------------------------------------------------
-- Exception Predicates

-- $preds
-- These pre-defined predicates may be used as the first argument to
-- 'catchJust', 'tryJust', or 'handleJust' to select certain common
-- classes of exceptions.
#ifndef __NHC__
ioErrors                :: Exception -> Maybe IOError
arithExceptions         :: Exception -> Maybe New.ArithException
errorCalls              :: Exception -> Maybe String
assertions              :: Exception -> Maybe String
dynExceptions           :: Exception -> Maybe Dynamic
asyncExceptions         :: Exception -> Maybe New.AsyncException
userErrors              :: Exception -> Maybe String

ioErrors (IOException e) = Just e
ioErrors _ = Nothing

arithExceptions (ArithException e) = Just e
arithExceptions _ = Nothing

errorCalls (ErrorCall e) = Just e
errorCalls _ = Nothing

assertions (AssertionFailed e) = Just e
assertions _ = Nothing

dynExceptions (DynException e) = Just e
dynExceptions _ = Nothing

asyncExceptions (AsyncException e) = Just e
asyncExceptions _ = Nothing

userErrors (IOException e) | isUserError e = Just (ioeGetErrorString e)
userErrors _ = Nothing
#endif
-----------------------------------------------------------------------------
-- Some Useful Functions

-- | When you want to acquire a resource, do some work with it, and
-- then release the resource, it is a good idea to use 'bracket',
-- because 'bracket' will install the necessary exception handler to
-- release the resource in the event that an exception is raised
-- during the computation.  If an exception is raised, then 'bracket' will 
-- re-raise the exception (after performing the release).
--
-- A common example is opening a file:
--
-- > bracket
-- >   (openFile "filename" ReadMode)
-- >   (hClose)
-- >   (\handle -> do { ... })
--
-- The arguments to 'bracket' are in this order so that we can partially apply 
-- it, e.g.:
--
-- > withFile name mode = bracket (openFile name mode) hClose
--
#ifndef __NHC__
bracket 
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracket before after thing =
  mask $ \restore -> do
    a <- before 
    r <- catch 
           (restore (thing a))
           (\e -> do { _ <- after a; throw e })
    _ <- after a
    return r
#endif

-- | A specialised variant of 'bracket' with just a computation to run
-- afterward.
-- 
finally :: IO a         -- ^ computation to run first
        -> IO b         -- ^ computation to run afterward (even if an exception 
                        -- was raised)
        -> IO a         -- returns the value from the first computation
a `finally` sequel =
  mask $ \restore -> do
    r <- catch 
             (restore a)
             (\e -> do { _ <- sequel; throw e })
    _ <- sequel
    return r

-- | A variant of 'bracket' where the return value from the first computation
-- is not required.
bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

-- | Like bracket, but only performs the final action if there was an 
-- exception raised by the in-between computation.
bracketOnError
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracketOnError before after thing =
  mask $ \restore -> do
    a <- before 
    catch 
        (restore (thing a))
        (\e -> do { _ <- after a; throw e })

-- -----------------------------------------------------------------------------
-- Asynchronous exceptions

{- $async

 #AsynchronousExceptions# Asynchronous exceptions are so-called because they arise due to
external influences, and can be raised at any point during execution.
'StackOverflow' and 'HeapOverflow' are two examples of
system-generated asynchronous exceptions.

The primary source of asynchronous exceptions, however, is
'throwTo':

>  throwTo :: ThreadId -> Exception -> IO ()

'throwTo' (also 'throwDynTo' and 'Control.Concurrent.killThread') allows one
running thread to raise an arbitrary exception in another thread.  The
exception is therefore asynchronous with respect to the target thread,
which could be doing anything at the time it receives the exception.
Great care should be taken with asynchronous exceptions; it is all too
easy to introduce race conditions by the over zealous use of
'throwTo'.
-}

{- $block_handler
There\'s an implied 'mask_' around every exception handler in a call
to one of the 'catch' family of functions.  This is because that is
what you want most of the time - it eliminates a common race condition
in starting an exception handler, because there may be no exception
handler on the stack to handle another exception if one arrives
immediately.  If asynchronous exceptions are blocked on entering the
handler, though, we have time to install a new exception handler
before being interrupted.  If this weren\'t the default, one would have
to write something like

>      mask $ \restore ->
>           catch (restore (...))
>                      (\e -> handler)

If you need to unblock asynchronous exceptions again in the exception
handler, just use 'unblock' as normal.

Note that 'try' and friends /do not/ have a similar default, because
there is no exception handler in this case.  If you want to use 'try'
in an asynchronous-exception-safe way, you will need to use
'mask'.
-}

{- $interruptible

Some operations are /interruptible/, which means that they can receive
asynchronous exceptions even in the scope of a 'mask'.  Any function
which may itself block is defined as interruptible; this includes
'Control.Concurrent.MVar.takeMVar'
(but not 'Control.Concurrent.MVar.tryTakeMVar'),
and most operations which perform
some I\/O with the outside world.  The reason for having
interruptible operations is so that we can write things like

>      mask $ \restore -> do
>         a <- takeMVar m
>         catch (restore (...))
>               (\e -> ...)

if the 'Control.Concurrent.MVar.takeMVar' was not interruptible,
then this particular
combination could lead to deadlock, because the thread itself would be
blocked in a state where it can\'t receive any asynchronous exceptions.
With 'Control.Concurrent.MVar.takeMVar' interruptible, however, we can be
safe in the knowledge that the thread can receive exceptions right up
until the point when the 'Control.Concurrent.MVar.takeMVar' succeeds.
Similar arguments apply for other interruptible operations like
'System.IO.openFile'.
-}

#if !(__GLASGOW_HASKELL__ || __NHC__)
assert :: Bool -> a -> a
assert True x = x
assert False _ = throw (AssertionFailed "")
#endif


#ifdef __GLASGOW_HASKELL__
{-# NOINLINE uncaughtExceptionHandler #-}
uncaughtExceptionHandler :: IORef (Exception -> IO ())
uncaughtExceptionHandler = unsafePerformIO (newIORef defaultHandler)
   where
      defaultHandler :: Exception -> IO ()
      defaultHandler ex = do
         (hFlush stdout) `New.catchAny` (\ _ -> return ())
         let msg = case ex of
               Deadlock    -> "no threads to run:  infinite loop or deadlock?"
               ErrorCall s -> s
               other       -> showsPrec 0 other ""
         withCString "%s" $ \cfmt ->
          withCString msg $ \cmsg ->
            errorBelch cfmt cmsg

-- don't use errorBelch() directly, because we cannot call varargs functions
-- using the FFI.
foreign import ccall unsafe "HsBase.h errorBelch2"
   errorBelch :: CString -> CString -> IO ()

setUncaughtExceptionHandler :: (Exception -> IO ()) -> IO ()
setUncaughtExceptionHandler = writeIORef uncaughtExceptionHandler

getUncaughtExceptionHandler :: IO (Exception -> IO ())
getUncaughtExceptionHandler = readIORef uncaughtExceptionHandler
#endif

-- ------------------------------------------------------------------------
-- Exception datatype and operations

-- |The type of exceptions.  Every kind of system-generated exception
-- has a constructor in the 'Exception' type, and values of other
-- types may be injected into 'Exception' by coercing them to
-- 'Data.Dynamic.Dynamic' (see the section on Dynamic Exceptions:
-- "Control.OldException\#DynamicExceptions").
data Exception
  = ArithException      New.ArithException
        -- ^Exceptions raised by arithmetic
        -- operations.  (NOTE: GHC currently does not throw
        -- 'ArithException's except for 'DivideByZero').
  | ArrayException      New.ArrayException
        -- ^Exceptions raised by array-related
        -- operations.  (NOTE: GHC currently does not throw
        -- 'ArrayException's).
  | AssertionFailed     String
        -- ^This exception is thrown by the
        -- 'assert' operation when the condition
        -- fails.  The 'String' argument contains the
        -- location of the assertion in the source program.
  | AsyncException      New.AsyncException
        -- ^Asynchronous exceptions (see section on Asynchronous Exceptions: "Control.OldException\#AsynchronousExceptions").
  | BlockedOnDeadMVar
        -- ^The current thread was executing a call to
        -- 'Control.Concurrent.MVar.takeMVar' that could never return,
        -- because there are no other references to this 'MVar'.
  | BlockedIndefinitely
        -- ^The current thread was waiting to retry an atomic memory transaction
        -- that could never become possible to complete because there are no other
        -- threads referring to any of the TVars involved.
  | NestedAtomically
        -- ^The runtime detected an attempt to nest one STM transaction
        -- inside another one, presumably due to the use of 
        -- 'unsafePeformIO' with 'atomically'.
  | Deadlock
        -- ^There are no runnable threads, so the program is
        -- deadlocked.  The 'Deadlock' exception is
        -- raised in the main thread only (see also: "Control.Concurrent").
  | DynException        Dynamic
        -- ^Dynamically typed exceptions (see section on Dynamic Exceptions: "Control.OldException\#DynamicExceptions").
  | ErrorCall           String
        -- ^The 'ErrorCall' exception is thrown by 'error'.  The 'String'
        -- argument of 'ErrorCall' is the string passed to 'error' when it was
        -- called.
  | ExitException       New.ExitCode
        -- ^The 'ExitException' exception is thrown by 'System.Exit.exitWith' (and
        -- 'System.Exit.exitFailure').  The 'ExitCode' argument is the value passed 
        -- to 'System.Exit.exitWith'.  An unhandled 'ExitException' exception in the
        -- main thread will cause the program to be terminated with the given 
        -- exit code.
  | IOException         New.IOException
        -- ^These are the standard IO exceptions generated by
        -- Haskell\'s @IO@ operations.  See also "System.IO.Error".
  | NoMethodError       String
        -- ^An attempt was made to invoke a class method which has
        -- no definition in this instance, and there was no default
        -- definition given in the class declaration.  GHC issues a
        -- warning when you compile an instance which has missing
        -- methods.
  | NonTermination
        -- ^The current thread is stuck in an infinite loop.  This
        -- exception may or may not be thrown when the program is
        -- non-terminating.
  | PatternMatchFail    String
        -- ^A pattern matching failure.  The 'String' argument should contain a
        -- descriptive message including the function name, source file
        -- and line number.
  | RecConError         String
        -- ^An attempt was made to evaluate a field of a record
        -- for which no value was given at construction time.  The
        -- 'String' argument gives the location of the
        -- record construction in the source program.
  | RecSelError         String
        -- ^A field selection was attempted on a constructor that
        -- doesn\'t have the requested field.  This can happen with
        -- multi-constructor records when one or more fields are
        -- missing from some of the constructors.  The
        -- 'String' argument gives the location of the
        -- record selection in the source program.
  | RecUpdError         String
        -- ^An attempt was made to update a field in a record,
        -- where the record doesn\'t have the requested field.  This can
        -- only occur with multi-constructor records, when one or more
        -- fields are missing from some of the constructors.  The
        -- 'String' argument gives the location of the
        -- record update in the source program.
INSTANCE_TYPEABLE0(Exception,exceptionTc,"Exception")

-- helper type for simplifying the type casting logic below
data Caster = forall e . New.Exception e => Caster (e -> Exception)

instance New.Exception Exception where
  -- We need to collect all the sorts of exceptions that used to be
  -- bundled up into the Exception type, and rebundle them for
  -- legacy handlers.
  fromException exc0 = foldr tryCast Nothing casters where
    tryCast (Caster f) e = case fromException exc0 of
      Just exc -> Just (f exc)
      _        -> e
    casters =
      [Caster (\exc -> ArithException exc),
       Caster (\exc -> ArrayException exc),
       Caster (\(New.AssertionFailed err) -> AssertionFailed err),
       Caster (\exc -> AsyncException exc),
       Caster (\New.BlockedIndefinitelyOnMVar -> BlockedOnDeadMVar),
       Caster (\New.BlockedIndefinitelyOnSTM -> BlockedIndefinitely),
       Caster (\New.NestedAtomically -> NestedAtomically),
       Caster (\New.Deadlock -> Deadlock),
       Caster (\exc -> DynException exc),
       Caster (\(New.ErrorCall err) -> ErrorCall err),
       Caster (\exc -> ExitException exc),
       Caster (\exc -> IOException exc),
       Caster (\(New.NoMethodError err) -> NoMethodError err),
       Caster (\New.NonTermination -> NonTermination),
       Caster (\(New.PatternMatchFail err) -> PatternMatchFail err),
       Caster (\(New.RecConError err) -> RecConError err),
       Caster (\(New.RecSelError err) -> RecSelError err),
       Caster (\(New.RecUpdError err) -> RecUpdError err),
       -- Anything else gets taken as a Dynamic exception. It's
       -- important that we put all exceptions into the old Exception
       -- type somehow, or throwing a new exception wouldn't cause
       -- the cleanup code for bracket, finally etc to happen.
       Caster (\exc -> DynException (toDyn (exc :: New.SomeException)))]

  -- Unbundle exceptions.
  toException (ArithException exc)   = toException exc
  toException (ArrayException exc)   = toException exc
  toException (AssertionFailed err)  = toException (New.AssertionFailed err)
  toException (AsyncException exc)   = toException exc
  toException BlockedOnDeadMVar      = toException New.BlockedIndefinitelyOnMVar
  toException BlockedIndefinitely    = toException New.BlockedIndefinitelyOnSTM
  toException NestedAtomically       = toException New.NestedAtomically
  toException Deadlock               = toException New.Deadlock
  -- If a dynamic exception is a SomeException then resurrect it, so
  -- that bracket, catch+throw etc rethrow the same exception even
  -- when the exception is in the new style.
  -- If it's not a SomeException, then just throw the Dynamic.
  toException (DynException exc)     = case fromDynamic exc of
                                       Just exc' -> exc'
                                       Nothing -> toException exc
  toException (ErrorCall err)        = toException (New.ErrorCall err)
  toException (ExitException exc)    = toException exc
  toException (IOException exc)      = toException exc
  toException (NoMethodError err)    = toException (New.NoMethodError err)
  toException NonTermination         = toException New.NonTermination
  toException (PatternMatchFail err) = toException (New.PatternMatchFail err)
  toException (RecConError err)      = toException (New.RecConError err)
  toException (RecSelError err)      = toException (New.RecSelError err)
  toException (RecUpdError err)      = toException (New.RecUpdError err)

instance Show Exception where
  showsPrec _ (IOException err)          = shows err
  showsPrec _ (ArithException err)       = shows err
  showsPrec _ (ArrayException err)       = shows err
  showsPrec _ (ErrorCall err)            = showString err
  showsPrec _ (ExitException err)        = showString "exit: " . shows err
  showsPrec _ (NoMethodError err)        = showString err
  showsPrec _ (PatternMatchFail err)     = showString err
  showsPrec _ (RecSelError err)          = showString err
  showsPrec _ (RecConError err)          = showString err
  showsPrec _ (RecUpdError err)          = showString err
  showsPrec _ (AssertionFailed err)      = showString err
  showsPrec _ (DynException err)         = showString "exception :: " . showsTypeRep (dynTypeRep err)
  showsPrec _ (AsyncException e)         = shows e
  showsPrec p BlockedOnDeadMVar          = showsPrec p New.BlockedIndefinitelyOnMVar
  showsPrec p BlockedIndefinitely        = showsPrec p New.BlockedIndefinitelyOnSTM
  showsPrec p NestedAtomically           = showsPrec p New.NestedAtomically
  showsPrec p NonTermination             = showsPrec p New.NonTermination
  showsPrec p Deadlock                   = showsPrec p New.Deadlock

instance Eq Exception where
  IOException e1      == IOException e2      = e1 == e2
  ArithException e1   == ArithException e2   = e1 == e2
  ArrayException e1   == ArrayException e2   = e1 == e2
  ErrorCall e1        == ErrorCall e2        = e1 == e2
  ExitException e1    == ExitException e2    = e1 == e2
  NoMethodError e1    == NoMethodError e2    = e1 == e2
  PatternMatchFail e1 == PatternMatchFail e2 = e1 == e2
  RecSelError e1      == RecSelError e2      = e1 == e2
  RecConError e1      == RecConError e2      = e1 == e2
  RecUpdError e1      == RecUpdError e2      = e1 == e2
  AssertionFailed e1  == AssertionFailed e2  = e1 == e2
  DynException _      == DynException _      = False -- incomparable
  AsyncException e1   == AsyncException e2   = e1 == e2
  BlockedOnDeadMVar   == BlockedOnDeadMVar   = True
  NonTermination      == NonTermination      = True
  NestedAtomically    == NestedAtomically    = True
  Deadlock            == Deadlock            = True
  _                   == _                   = False

