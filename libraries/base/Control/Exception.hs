{-# OPTIONS_GHC -XNoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Exception
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

module Control.Exception (

        -- * The Exception type
        SomeException(..),
        Exception(..),          -- instance Eq, Ord, Show, Typeable
        IOException,            -- instance Eq, Ord, Show, Typeable
        ArithException(..),     -- instance Eq, Ord, Show, Typeable
        ArrayException(..),     -- instance Eq, Ord, Show, Typeable
        AssertionFailed(..),
        AsyncException(..),     -- instance Eq, Ord, Show, Typeable
        NonTermination(..), nonTermination,
        BlockedOnDeadMVar(..),
        BlockedIndefinitely(..),
        NestedAtomically(..), nestedAtomically,
        Deadlock(..),
        NoMethodError(..),
        PatternMatchFail(..),
        RecConError(..),
        RecSelError(..),
        RecUpdError(..),

        -- * Throwing exceptions
        throwIO,        -- :: Exception -> IO a
        throw,          -- :: Exception -> a
        ioError,        -- :: IOError -> IO a
#ifdef __GLASGOW_HASKELL__
        throwTo,        -- :: ThreadId -> Exception -> a
#endif

        -- * Catching Exceptions

        -- |There are several functions for catching and examining
        -- exceptions; all of them may only be used from within the
        -- 'IO' monad.

        -- ** The @catch@ functions
        catch,     -- :: IO a -> (Exception -> IO a) -> IO a
        catches, Handler(..),
        catchAny,
        catchJust, -- :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a

        -- ** The @handle@ functions
        handle,    -- :: (Exception -> IO a) -> IO a -> IO a
        handleAny,
        handleJust,-- :: (Exception -> Maybe b) -> (b -> IO a) -> IO a -> IO a

        -- ** The @try@ functions
        try,       -- :: IO a -> IO (Either Exception a)
        tryJust,   -- :: (Exception -> Maybe b) -> a    -> IO (Either b a)
        ignoreExceptions,
        onException,

        -- ** The @evaluate@ function
        evaluate,  -- :: a -> IO a

        -- ** The @mapException@ function
        mapException,           -- :: (Exception -> Exception) -> a -> a

        -- * Asynchronous Exceptions

        -- $async

        -- ** Asynchronous exception control

        -- |The following two functions allow a thread to control delivery of
        -- asynchronous exceptions during a critical region.

        block,          -- :: IO a -> IO a
        unblock,        -- :: IO a -> IO a
        blocked,        -- :: IO Bool

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

        recSelError, recConError, irrefutPatError, runtimeError,
        nonExhaustiveGuardsError, patError, noMethodBindingError,

#ifdef __GLASGOW_HASKELL__
        setUncaughtExceptionHandler,      -- :: (Exception -> IO ()) -> IO ()
        getUncaughtExceptionHandler       -- :: IO (Exception -> IO ())
#endif
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase
import {-# SOURCE #-} GHC.Handle
import GHC.List
import GHC.Num
import GHC.Show
import GHC.IOBase as ExceptionBase
import GHC.Exception hiding ( Exception )
import {-# SOURCE #-} GHC.Conc         ( ThreadId(ThreadId) )
import Foreign.C.String ( CString, withCString )
#endif

#ifdef __HUGS__
import Hugs.Exception   as ExceptionBase
#endif

import Data.Dynamic
import Data.Either
import Data.Maybe

#ifdef __NHC__
import qualified System.IO.Error as H'98 (catch)
import System.IO.Error (ioError)
import IO              (bracket)
import DIOError         -- defn of IOError type
import System          (ExitCode())

-- minimum needed for nhc98 to pretend it has Exceptions
data Exception   = IOException    IOException
                 | ArithException ArithException
                 | ArrayException ArrayException
                 | AsyncException AsyncException
                 | ExitException  ExitCode
                 deriving Show
type IOException = IOError
data ArithException
data ArrayException
data AsyncException
instance Show ArithException
instance Show ArrayException
instance Show AsyncException

catch    :: IO a -> (Exception -> IO a) -> IO a
a `catch` b = a `H'98.catch` (b . IOException)

throwIO  :: Exception -> IO a
throwIO (IOException e) = ioError e
throwIO _               = ioError (UserError "Control.Exception.throwIO"
                                             "unknown exception")
throw    :: Exception -> a
throw     = unsafePerformIO . throwIO

evaluate :: a -> IO a
evaluate x = x `seq` return x

assert :: Bool -> a -> a
assert True  x = x
assert False _ = throw (IOException (UserError "" "Assertion failed"))
#endif

#ifndef __GLASGOW_HASKELL__
-- Dummy definitions for implementations lacking asynchonous exceptions

block   :: IO a -> IO a
block    = id
unblock :: IO a -> IO a
unblock  = id
blocked :: IO Bool
blocked  = return False
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
-- 'Prelude.catch' with a similar type to 'Control.Exception.catch',
-- except that the "Prelude" version only catches the IO and user
-- families of exceptions (as required by Haskell 98).  
--
-- We recommend either hiding the "Prelude" version of 'Prelude.catch'
-- when importing "Control.Exception": 
--
-- > import Prelude hiding (catch)
--
-- or importing "Control.Exception" qualified, to avoid name-clashes:
--
-- > import qualified Control.Exception as C
--
-- and then using @C.catch@
--
#ifndef __NHC__
catch   :: Exception e
        => IO a         -- ^ The computation to run
        -> (e -> IO a)  -- ^ Handler to invoke if an exception is raised
        -> IO a
catch = ExceptionBase.catchException

catches :: IO a -> [Handler a] -> IO a
catches io handlers = io `catch` catchesHandler handlers

catchesHandler :: [Handler a] -> SomeException -> IO a
catchesHandler handlers e = foldr tryHandler (throw e) handlers
    where tryHandler (Handler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res

data Handler a = forall e . Exception e => Handler (e -> IO a)
#endif
-- | The function 'catchJust' is like 'catch', but it takes an extra
-- argument which is an /exception predicate/, a function which
-- selects which type of exceptions we\'re interested in.
--
-- >   result <- catchJust errorCalls thing_to_try handler
--
-- Any other exceptions which are not matched by the predicate
-- are re-raised, and may be caught by an enclosing
-- 'catch' or 'catchJust'.
catchJust
        :: Exception e
        => (e -> Maybe b)         -- ^ Predicate to select exceptions
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
handle     :: Exception e => (e -> IO a) -> IO a -> IO a
handle     =  flip catch

handleAny  :: (forall e . Exception e => e -> IO a) -> IO a -> IO a
handleAny  =  flip catchAny

-- | A version of 'catchJust' with the arguments swapped around (see
-- 'handle').
handleJust :: Exception e => (e -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust p =  flip (catchJust p)

-----------------------------------------------------------------------------
-- 'mapException'

-- | This function maps one exception into another as proposed in the
-- paper \"A semantics for imprecise exceptions\".

-- Notice that the usage of 'unsafePerformIO' is safe here.

mapException :: Exception e => (e -> e) -> a -> a
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
-- 'System.IO.Error.try' with a similar type to 'Control.Exception.try',
-- except that it catches only the IO and user families of exceptions
-- (as required by the Haskell 98 @IO@ module).

try :: Exception e => IO a -> IO (Either e a)
try a = catch (a >>= \ v -> return (Right v)) (\e -> return (Left e))

-- | A variant of 'try' that takes an exception predicate to select
-- which exceptions are caught (c.f. 'catchJust').  If the exception
-- does not match the predicate, it is re-thrown.
tryJust :: Exception e => (e -> Maybe b) -> IO a -> IO (Either b a)
tryJust p a = do
  r <- try a
  case r of
        Right v -> return (Right v)
        Left  e -> case p e of
                        Nothing -> throw e
                        Just b  -> return (Left b)

ignoreExceptions :: IO () -> IO ()
ignoreExceptions io = io `catchAny` \_ -> return ()

onException :: IO a -> IO () -> IO a
onException io what = io `catch` \e -> do what
                                          throw (e :: SomeException)

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
  block (do
    a <- before 
    r <- catchAny
           (unblock (thing a))
           (\e -> do { after a; throw e })
    after a
    return r
 )
#endif

-- | A specialised variant of 'bracket' with just a computation to run
-- afterward.
-- 
finally :: IO a         -- ^ computation to run first
        -> IO b         -- ^ computation to run afterward (even if an exception 
                        -- was raised)
        -> IO a         -- returns the value from the first computation
a `finally` sequel =
  block (do
    r <- catchAny
             (unblock a)
             (\e -> do { sequel; throw e })
    sequel
    return r
  )

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
  block (do
    a <- before 
    catchAny
        (unblock (thing a))
        (\e -> do { after a; throw e })
 )

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
There\'s an implied 'block' around every exception handler in a call
to one of the 'catch' family of functions.  This is because that is
what you want most of the time - it eliminates a common race condition
in starting an exception handler, because there may be no exception
handler on the stack to handle another exception if one arrives
immediately.  If asynchronous exceptions are blocked on entering the
handler, though, we have time to install a new exception handler
before being interrupted.  If this weren\'t the default, one would have
to write something like

>      block (
>           catch (unblock (...))
>                      (\e -> handler)
>      )

If you need to unblock asynchronous exceptions again in the exception
handler, just use 'unblock' as normal.

Note that 'try' and friends /do not/ have a similar default, because
there is no exception handler in this case.  If you want to use 'try'
in an asynchronous-exception-safe way, you will need to use
'block'.
-}

{- $interruptible

Some operations are /interruptible/, which means that they can receive
asynchronous exceptions even in the scope of a 'block'.  Any function
which may itself block is defined as interruptible; this includes
'Control.Concurrent.MVar.takeMVar'
(but not 'Control.Concurrent.MVar.tryTakeMVar'),
and most operations which perform
some I\/O with the outside world.  The reason for having
interruptible operations is so that we can write things like

>      block (
>         a <- takeMVar m
>         catch (unblock (...))
>               (\e -> ...)
>      )

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
uncaughtExceptionHandler :: IORef (SomeException -> IO ())
uncaughtExceptionHandler = unsafePerformIO (newIORef defaultHandler)
   where
      defaultHandler :: SomeException -> IO ()
      defaultHandler se@(SomeException ex) = do
         (hFlush stdout) `catchAny` (\ _ -> return ())
         let msg = case cast ex of
               Just Deadlock -> "no threads to run:  infinite loop or deadlock?"
               _ -> case cast ex of
                    Just (ErrorCall s) -> s
                    _                  -> showsPrec 0 se ""
         withCString "%s" $ \cfmt ->
          withCString msg $ \cmsg ->
            errorBelch cfmt cmsg

-- don't use errorBelch() directly, because we cannot call varargs functions
-- using the FFI.
foreign import ccall unsafe "HsBase.h errorBelch2"
   errorBelch :: CString -> CString -> IO ()

setUncaughtExceptionHandler :: (SomeException -> IO ()) -> IO ()
setUncaughtExceptionHandler = writeIORef uncaughtExceptionHandler

getUncaughtExceptionHandler :: IO (SomeException -> IO ())
getUncaughtExceptionHandler = readIORef uncaughtExceptionHandler
#endif

recSelError, recConError, irrefutPatError, runtimeError,
             nonExhaustiveGuardsError, patError, noMethodBindingError
        :: Addr# -> a   -- All take a UTF8-encoded C string

recSelError              s = throw (RecSelError (unpackCStringUtf8# s)) -- No location info unfortunately
runtimeError             s = error (unpackCStringUtf8# s)               -- No location info unfortunately

nonExhaustiveGuardsError s = throw (PatternMatchFail (untangle s "Non-exhaustive guards in"))
irrefutPatError          s = throw (PatternMatchFail (untangle s "Irrefutable pattern failed for pattern"))
recConError              s = throw (RecConError      (untangle s "Missing field in record construction"))
noMethodBindingError     s = throw (NoMethodError    (untangle s "No instance nor default method for class operation"))
patError                 s = throw (PatternMatchFail (untangle s "Non-exhaustive patterns in"))

-----

data PatternMatchFail = PatternMatchFail String
    deriving Typeable

instance Exception PatternMatchFail

instance Show PatternMatchFail where
    showsPrec _ (PatternMatchFail err) = showString err

-----

data RecSelError = RecSelError String
    deriving Typeable

instance Exception RecSelError

instance Show RecSelError where
    showsPrec _ (RecSelError err) = showString err

-----

data RecConError = RecConError String
    deriving Typeable

instance Exception RecConError

instance Show RecConError where
    showsPrec _ (RecConError err) = showString err

-----

data RecUpdError = RecUpdError String
    deriving Typeable

instance Exception RecUpdError

instance Show RecUpdError where
    showsPrec _ (RecUpdError err) = showString err

-----

data NoMethodError = NoMethodError String
    deriving Typeable

instance Exception NoMethodError

instance Show NoMethodError where
    showsPrec _ (NoMethodError err) = showString err

-----

data AssertionFailed = AssertionFailed String
    deriving Typeable

instance Exception AssertionFailed

instance Show AssertionFailed where
    showsPrec _ (AssertionFailed err) = showString err

-----

data NonTermination = NonTermination
    deriving Typeable

instance Exception NonTermination

instance Show NonTermination where
    showsPrec _ NonTermination = showString "<<loop>>"

-- GHC's RTS calls this
nonTermination :: SomeException
nonTermination = toException NonTermination

-----

data Deadlock = Deadlock
    deriving Typeable

instance Exception Deadlock

instance Show Deadlock where
    showsPrec _ Deadlock = showString "<<deadlock>>"

-----

data NestedAtomically = NestedAtomically
    deriving Typeable

instance Exception NestedAtomically

instance Show NestedAtomically where
    showsPrec _ NestedAtomically = showString "Control.Concurrent.STM.atomically was nested"

-- GHC's RTS calls this
nestedAtomically :: SomeException
nestedAtomically = toException NestedAtomically

-----

instance Exception Dynamic

-----

assertError :: Addr# -> Bool -> a -> a
assertError str pred v
  | pred      = v
  | otherwise = throw (AssertionFailed (untangle str "Assertion failed"))

{-
(untangle coded message) expects "coded" to be of the form
        "location|details"
It prints
        location message details
-}
untangle :: Addr# -> String -> String
untangle coded message
  =  location
  ++ ": " 
  ++ message
  ++ details
  ++ "\n"
  where
    coded_str = unpackCStringUtf8# coded

    (location, details)
      = case (span not_bar coded_str) of { (loc, rest) ->
        case rest of
          ('|':det) -> (loc, ' ' : det)
          _         -> (loc, "")
        }
    not_bar c = c /= '|'

-- XXX From GHC.Conc
throwTo :: Exception e => ThreadId -> e -> IO ()
throwTo (ThreadId id) ex = IO $ \ s ->
   case (killThread# id (toException ex) s) of s1 -> (# s1, () #)

