-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Exception
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides support for raising and catching both built-in
-- and user-defined exceptions.
--
-----------------------------------------------------------------------------

module Control.Exception (

	-- * The Exception type
	Exception(..),		-- instance Eq, Ord, Show, Typeable
	IOException,		-- instance Eq, Ord, Show, Typeable
	ArithException(..),	-- instance Eq, Ord, Show, Typeable
	ArrayException(..),	-- instance Eq, Ord, Show, Typeable
	AsyncException(..),	-- instance Eq, Ord, Show, Typeable

	-- * Throwing exceptions
	throwIO,	-- :: Exception -> IO a
	throw,		-- :: Exception -> a
	ioError,	-- :: IOError -> IO a
#ifdef __GLASGOW_HASKELL__
	throwTo,	-- :: ThreadId -> Exception -> a
#endif

	-- * Catching Exceptions

	-- |There are several functions for catching and examining
    	-- exceptions; all of them may only be used from within the
    	-- 'IO' monad.

	-- ** The @catch@ functions
	catch,     -- :: IO a -> (Exception -> IO a) -> IO a
	catchJust, -- :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a

	-- ** The @handle@ functions
	handle,	   -- :: (Exception -> IO a) -> IO a -> IO a
	handleJust,-- :: (Exception -> Maybe b) -> (b -> IO a) -> IO a -> IO a

	-- ** The @try@ functions
	try,       -- :: IO a -> IO (Either Exception a)
	tryJust,   -- :: (Exception -> Maybe b) -> a    -> IO (Either b a)

	-- ** The @evaluate@ function
	evaluate,  -- :: a -> IO a

	-- ** The @mapException@ function
	mapException,		-- :: (Exception -> Exception) -> a -> a

	-- ** Exception predicates
	
	-- $preds

	ioErrors,		-- :: Exception -> Maybe IOError
	arithExceptions, 	-- :: Exception -> Maybe ArithException
	errorCalls,		-- :: Exception -> Maybe String
	dynExceptions,		-- :: Exception -> Maybe Dynamic
	assertions,		-- :: Exception -> Maybe String
	asyncExceptions, 	-- :: Exception -> Maybe AsyncException
	userErrors,		-- :: Exception -> Maybe String

	-- * Dynamic exceptions

	-- $dynamic
	throwDyn, 	-- :: Typeable ex => ex -> b
#ifdef __GLASGOW_HASKELL__
	throwDynTo, 	-- :: Typeable ex => ThreadId -> ex -> b
#endif
	catchDyn, 	-- :: Typeable ex => IO a -> (ex -> IO a) -> IO a
	
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

	assert,		-- :: Bool -> a -> a

	-- * Utilities

	bracket,  	-- :: IO a -> (a -> IO b) -> (a -> IO c) -> IO ()
	bracket_, 	-- :: IO a -> IO b -> IO c -> IO ()

	finally, 	-- :: IO a -> IO b -> IO a
	
#ifdef __GLASGOW_HASKELL__
	setUncaughtExceptionHandler,      -- :: (Exception -> IO ()) -> IO ()
	getUncaughtExceptionHandler       -- :: IO (Exception -> IO ())
#endif
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base		( assert )
import GHC.Exception 	as ExceptionBase hiding (catch)
import GHC.Conc		( throwTo, ThreadId )
import Data.IORef	( IORef, newIORef, readIORef, writeIORef )
import Foreign.C.String ( CString, withCString )
import System.IO	( stdout, hFlush )
#endif

#ifdef __HUGS__
import Hugs.Exception	as ExceptionBase
#endif

import Prelude 		hiding ( catch )
import System.IO.Error	hiding ( catch, try )
import System.IO.Unsafe (unsafePerformIO)
import Data.Dynamic

-----------------------------------------------------------------------------
-- Catching exceptions

-- |This is the simplest of the exception-catching functions.  It
-- takes a single argument, runs it, and if an exception is raised
-- the \"handler\" is executed, with the value of the exception passed as an
-- argument.  Otherwise, the result is returned as normal.  For example:
--
-- >   catch (openFile f ReadMode) 
-- >       (\e -> hPutStr stderr (\"Couldn\'t open \"++f++\": \" ++ show e))
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
-- families of exceptions (as required by Haskell 98).  We recommend
-- either hiding the "Prelude" version of
-- 'Prelude.catch' when importing
-- "Control.Exception", or importing
-- "Control.Exception" qualified, to avoid name-clashes.

catch  	:: IO a 		-- ^ The computation to run
  	-> (Exception -> IO a)	-- ^ Handler to invoke if an exception is raised
  	-> IO a			
catch =  ExceptionBase.catchException

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
	-> IO a		 	  -- ^ Computation to run
	-> (b -> IO a)		  -- ^ Handler
	-> IO a
catchJust p a handler = catch a handler'
  where handler' e = case p e of 
			Nothing -> throw e
			Just b  -> handler b

-- | A version of 'catch' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.  For example:
--
-- >   do handle (\e -> exitWith (ExitFailure 1)) $
-- >	  ...
handle	   :: (Exception -> IO a) -> IO a -> IO a
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
-- >  try a = catch (Right \`liftM\` a) (return . Left)
--
-- Note: as with 'catch', it is only polite to use this variant if you intend
-- to re-throw the exception after performing whatever cleanup is needed.
-- Otherwise, 'tryJust' is generally considered to be better.
--
-- Also note that "System.IO.Error" also exports a function called
-- 'System.IO.Error.try' with a similar type to 'Control.Exception.try',
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
throwDyn exception = throw (DynException (toDyn exception))

#ifdef __GLASGOW_HASKELL__
-- | A variant of 'throwDyn' that throws the dynamic exception to an
-- arbitrary thread (GHC only: c.f. 'throwTo').
throwDynTo :: Typeable exception => ThreadId -> exception -> IO ()
throwDynTo t exception = throwTo t (DynException (toDyn exception))
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
catchDyn m k = catchException m handle
  where handle ex = case ex of
  			   (DynException dyn) ->
		  	  	case fromDynamic dyn of
				    Just exception  -> k exception
				    Nothing -> throw ex
			   _ -> throw ex

-----------------------------------------------------------------------------
-- Exception Predicates

-- $preds
-- These pre-defined predicates may be used as the first argument to
-- 'catchJust', 'tryJust', or 'handleJust' to select certain common
-- classes of exceptions.

ioErrors		:: Exception -> Maybe IOError
arithExceptions 	:: Exception -> Maybe ArithException
errorCalls		:: Exception -> Maybe String
assertions		:: Exception -> Maybe String
dynExceptions		:: Exception -> Maybe Dynamic
asyncExceptions 	:: Exception -> Maybe AsyncException
userErrors		:: Exception -> Maybe String

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
-- > withFile name = bracket (openFile name) hClose
--
bracket 
	:: IO a		-- ^ computation to run first (\"acquire resource\")
	-> (a -> IO b)  -- ^ computation to run last (\"release resource\")
	-> (a -> IO c)	-- ^ computation to run in-between
	-> IO c		-- returns the value from the in-between computation
bracket before after thing =
  block (do
    a <- before 
    r <- catch 
	   (unblock (thing a))
	   (\e -> do { after a; throw e })
    after a
    return r
 )
   

-- | A specialised variant of 'bracket' with just a computation to run
-- afterward.
-- 
finally :: IO a		-- ^ computation to run first
	-> IO b		-- ^ computation to run afterward (even if an exception 
			-- was raised)
	-> IO a		-- returns the value from the first computation
a `finally` sequel =
  block (do
    r <- catch 
	     (unblock a)
	     (\e -> do { sequel; throw e })
    sequel
    return r
  )

-- | A variant of 'bracket' where the return value from the first computation
-- is not required.
bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

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

-- -----------------------------------------------------------------------------
-- Assert

#ifdef __HADDOCK__
-- | If the first argument evaluates to 'True', then the result is the
-- second argument.  Otherwise an 'AssertionFailed' exception is raised,
-- containing a 'String' with the source file and line number of the
-- call to assert.
--
-- Assertions can normally be turned on or off with a compiler flag
-- (for GHC, assertions are normally on unless the @-fignore-asserts@
-- option is give).  When assertions are turned off, the first
-- argument to 'assert' is ignored, and the second argument is
-- returned as the result.
assert :: Bool -> a -> a
#endif

#ifndef __GLASGOW_HASKELL__
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
         (hFlush stdout) `catchException` (\ _ -> return ())
         let msg = case ex of
               Deadlock    -> "no threads to run:  infinite loop or deadlock?"
               ErrorCall s -> s
               other       -> showsPrec 0 other "\n"
         withCString "%s" $ \cfmt ->
          withCString msg $ \cmsg ->
            errorBelch cfmt cmsg

foreign import ccall unsafe errorBelch :: CString -> CString -> IO ()

setUncaughtExceptionHandler :: (Exception -> IO ()) -> IO ()
setUncaughtExceptionHandler = writeIORef uncaughtExceptionHandler

getUncaughtExceptionHandler :: IO (Exception -> IO ())
getUncaughtExceptionHandler = readIORef uncaughtExceptionHandler
#endif
