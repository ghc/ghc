-----------------------------------------------------------------------------
-- 
-- Module      :  Control.Exception
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Exception.hs,v 1.5 2001/12/21 15:07:21 simonmar Exp $
--
-- The External API for exceptions.  The functions provided in this
-- module allow catching of exceptions in the IO monad.
--
-----------------------------------------------------------------------------

module Control.Exception (

	Exception(..),		-- instance Eq, Ord, Show, Typeable
	IOException,		-- instance Eq, Ord, Show, Typeable
	ArithException(..),	-- instance Eq, Ord, Show, Typeable
	ArrayException(..),	-- instance Eq, Ord, Show, Typeable
	AsyncException(..),	-- instance Eq, Ord, Show, Typeable

	try,       -- :: IO a -> IO (Either Exception a)
	tryJust,   -- :: (Exception -> Maybe b) -> a    -> IO (Either b a)

	catch,     -- :: IO a -> (Exception -> IO a) -> IO a
	catchJust, -- :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a

	handle,	   -- :: (Exception -> IO a) -> IO a -> IO a
	handleJust,-- :: (Exception -> Maybe b) -> (b -> IO a) -> IO a -> IO a

	evaluate,  -- :: a -> IO a

	-- Exception predicates (for tryJust, catchJust, handleJust)

	ioErrors,		-- :: Exception -> Maybe IOError
	arithExceptions, 	-- :: Exception -> Maybe ArithException
	errorCalls,		-- :: Exception -> Maybe String
	dynExceptions,		-- :: Exception -> Maybe Dynamic
	assertions,		-- :: Exception -> Maybe String
	asyncExceptions, 	-- :: Exception -> Maybe AsyncException
	userErrors,		-- :: Exception -> Maybe String

	-- Throwing exceptions

	throw,		-- :: Exception -> a
	throwTo,	-- :: ThreadId -> Exception -> a

	-- Dynamic exceptions

	throwDyn, 	-- :: Typeable ex => ex -> b
	throwDynTo, 	-- :: Typeable ex => ThreadId -> ex -> b
	catchDyn, 	-- :: Typeable ex => IO a -> (ex -> IO a) -> IO a
	
	-- Async exception control

        block,          -- :: IO a -> IO a
        unblock,        -- :: IO a -> IO a

	-- Assertions

	-- for now
	assert,		-- :: Bool -> a -> a

	-- Utilities

	finally, 	-- :: IO a -> IO b -> IO b

	bracket,  	-- :: IO a -> (a -> IO b) -> (a -> IO c) -> IO ()
	bracket_, 	-- :: IO a -> IO b -> IO c -> IO ()

  ) where

#ifdef __GLASGOW_HASKELL__
import Prelude 		hiding (catch)
import GHC.Base		( assert )
import GHC.Exception 	hiding (try, catch, bracket, bracket_)
import GHC.Conc		( throwTo, ThreadId )
import GHC.IOBase	( IO(..) )
#endif

#ifdef __HUGS__
import Prelude hiding ( catch )
import PrelPrim	( catchException 
		, Exception(..)
		, throw
		, ArithException(..)
		, AsyncException(..)
		, assert
		)
#endif

import Data.Dynamic

#include "Dynamic.h"
INSTANCE_TYPEABLE0(Exception,exceptionTc,"Exception")
INSTANCE_TYPEABLE0(IOException,ioExceptionTc,"IOException")
INSTANCE_TYPEABLE0(ArithException,arithExceptionTc,"ArithException")
INSTANCE_TYPEABLE0(ArrayException,arrayExceptionTc,"ArrayException")
INSTANCE_TYPEABLE0(AsyncException,asyncExceptionTc,"AsyncException")

-----------------------------------------------------------------------------
-- Catching exceptions

-- GHC.Exception defines 'catchException' for us.

catch	  :: IO a -> (Exception -> IO a) -> IO a
catch 	  =  catchException

catchJust :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchJust p a handler = catch a handler'
  where handler' e = case p e of 
			Nothing -> throw e
			Just b  -> handler b

handle	   :: (Exception -> IO a) -> IO a -> IO a
handle     =  flip catch

handleJust :: (Exception -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust p =  flip (catchJust p)

-----------------------------------------------------------------------------
-- evaluate

evaluate :: a -> IO a
evaluate a = a `seq` return a

-----------------------------------------------------------------------------
-- 'try' and variations.

try :: IO a -> IO (Either Exception a)
try a = catch (a >>= \ v -> return (Right v)) (\e -> return (Left e))

tryJust :: (Exception -> Maybe b) -> IO a -> IO (Either b a)
tryJust p a = do
  r <- try a
  case r of
	Right v -> return (Right v)
	Left  e -> case p e of
			Nothing -> throw e
			Just b  -> return (Left b)

-----------------------------------------------------------------------------
-- Dynamic exception types.  Since one of the possible kinds of exception
-- is a dynamically typed value, we can effectively have polymorphic
-- exceptions.

-- throwDyn will raise any value as an exception, provided it is in the
-- Typeable class (see Dynamic.lhs).  

-- catchDyn will catch any exception of a given type (determined by the
-- handler function).  Any raised exceptions that don't match are
-- re-raised.

throwDyn :: Typeable exception => exception -> b
throwDyn exception = throw (DynException (toDyn exception))

throwDynTo :: Typeable exception => ThreadId -> exception -> IO ()
throwDynTo t exception = throwTo t (DynException (toDyn exception))

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

ioErrors		:: Exception -> Maybe IOError
arithExceptions 	:: Exception -> Maybe ArithException
errorCalls		:: Exception -> Maybe String
dynExceptions		:: Exception -> Maybe Dynamic
assertions		:: Exception -> Maybe String
asyncExceptions 	:: Exception -> Maybe AsyncException
userErrors		:: Exception -> Maybe String

ioErrors e@(IOException _) = Just e
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

userErrors (UserError e) = Just e
userErrors _ = Nothing

-----------------------------------------------------------------------------
-- Some Useful Functions

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing =
  block (do
    a <- before 
    r <- catch 
	   (unblock (thing a))
	   (\e -> do { after a; throw e })
    after a
    return r
 )
   
-- finally is an instance of bracket, but it's quite common
-- so we give the specialised version for efficiency.
finally :: IO a -> IO b -> IO a
a `finally` sequel =
  block (do
    r <- catch 
	     (unblock a)
	     (\e -> do { sequel; throw e })
    sequel
    return r
  )

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)
