% -----------------------------------------------------------------------------
% $Id: Exception.lhs,v 1.4 1999/01/14 18:15:29 sof Exp $
%
% (c) The GRAP/AQUA Project, Glasgow University, 1998
%

The External API for exceptions.  The functions provided in this
module allow catching of exceptions in the IO monad.

\begin{code}
module Exception (

	Exception(..),		-- instance Show
	ArithException(..),	-- instance Show
	AsyncException(..),	-- instance Show

	tryAll,    -- :: a    -> IO (Either Exception a)
	tryAllIO,  -- :: IO a -> IO (Either Exception a)
	try,	   -- :: (Exception -> Maybe b) -> a    -> IO (Either b a)
	tryIO,	   -- :: (Exception -> Maybe b) -> IO a -> IO (Either b a)

	catchAll,  -- :: a    -> (Exception -> IO a) -> IO a
	catchAllIO,-- :: IO a -> (Exception -> IO a) -> IO a
	catch,     -- :: (Exception -> Maybe b) -> a    -> (b -> IO a) -> IO a
	catchIO,   -- :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a

	-- Exception predicates

	justIoErrors,		-- :: Exception -> Maybe IOError
	justArithExceptions, 	-- :: Exception -> Maybe ArithException
	justErrors,		-- :: Exception -> Maybe String
	justDynExceptions,	-- :: Exception -> Maybe Dynamic
	justAssertions,		-- :: Exception -> Maybe String
	justAsyncExceptions, 	-- :: Exception -> Maybe AsyncException

	-- Throwing exceptions

	throw,		-- :: Exception -> a

	-- Dynamic exceptions

	throwDyn, 	-- :: Typeable ex => ex -> b
	catchDyn, 	-- :: Typeable ex => IO a -> (ex -> IO a) -> IO a

	-- Utilities
		
	finally, 	-- :: IO a -> IO b -> IO b

	bracket,  	-- :: IO a -> (a -> IO b) -> (a -> IO c) -> IO ()
	bracket_, 	-- :: IO a -> IO b -> IO c -> IO ()

  ) where

#ifdef __HUGS__
import PreludeBuiltin hiding (catch)
import Prelude        hiding (catch)
#else
import Prelude hiding (catch)
import PrelGHC (catch#)
import PrelException hiding (catch)
#endif

import Dynamic
\end{code}

-----------------------------------------------------------------------------
Catching exceptions

PrelException defines 'catchException' for us.

\begin{code}
catchAll  :: a    -> (Exception -> IO a) -> IO a
#ifdef __HUGS__
catchAll a handler = primCatch' (case primForce a of () -> return a) handler
#else
catchAll a handler = catch# (a `seq` return a) handler
#endif

catchAllIO :: IO a -> (Exception -> IO a) -> IO a
catchAllIO =  catchException

catch :: (Exception -> Maybe b) -> a -> (b -> IO a) -> IO a
catch p a handler = catchAll a handler'
  where handler' e = case p e of 
			Nothing -> throw e
			Just b  -> handler b

catchIO :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchIO p a handler = catchAllIO a handler'
  where handler' e = case p e of 
			Nothing -> throw e
			Just b  -> handler b
\end{code}

-----------------------------------------------------------------------------
'try' and variations.

\begin{code}
tryAll :: a    -> IO (Either Exception a)
#ifdef __HUGS__
tryAll a = primCatch' (case primForce a of { () -> return Nothing}) 
			    (\e -> return (Just e))
#else
tryAll a = catch# (a `seq` return (Right a)) (\e -> return (Left e))
#endif

tryAllIO :: IO a -> IO (Either Exception a)
tryAllIO a = catchAllIO (a >>= \ v -> return (Right v))
			(\e -> return (Left e))

try :: (Exception -> Maybe b) -> a -> IO (Either b a)
try p a = do
  r <- tryAll a
  case r of
	Right v -> return (Right v)
	Left  e -> case p e of
			Nothing -> throw e
			Just b  -> return (Left b)

tryIO :: (Exception -> Maybe b) -> IO a -> IO (Either b a)
tryIO p a = do
  r <- tryAllIO a
  case r of
	Right v -> return (Right v)
	Left  e -> case p e of
			Nothing -> throw e
			Just b  -> return (Left b)
\end{code}

-----------------------------------------------------------------------------
Dynamic exception types.  Since one of the possible kinds of exception
is a dynamically typed value, we can effectively have polymorphic
exceptions.

throwDyn will raise any value as an exception, provided it is in the
Typeable class (see Dynamic.lhs).  

catchDyn will catch any exception of a given type (determined by the
handler function).  Any raised exceptions that don't match are
re-raised.

\begin{code}
throwDyn :: Typeable exception => exception -> b
throwDyn exception = throw (DynException (toDyn exception))

catchDyn :: Typeable exception => IO a -> (exception -> IO a) -> IO a
catchDyn m k = catchException m handle
  where handle ex = case ex of
  			   (DynException dyn) ->
		  	  	case fromDynamic dyn of
				    Just exception  -> k exception
				    Nothing -> throw ex
			   _ -> throw ex
\end{code}

-----------------------------------------------------------------------------
Exception Predicates

\begin{code}
justIoErrors		:: Exception -> Maybe IOError
justArithExceptions 	:: Exception -> Maybe ArithException
justErrors		:: Exception -> Maybe String
justDynExceptions	:: Exception -> Maybe Dynamic
justAssertions		:: Exception -> Maybe String
justAsyncExceptions 	:: Exception -> Maybe AsyncException

justIoErrors (IOException e) = Just e
justIoErrors _ = Nothing

justArithExceptions (ArithException e) = Just e
justArithExceptions _ = Nothing

justErrors (ErrorCall e) = Just e
justErrors _ = Nothing

justAssertions (AssertionFailed e) = Just e
justAssertions _ = Nothing

justDynExceptions (DynException e) = Just e
justDynExceptions _ = Nothing

justAsyncExceptions (AsyncException e) = Just e
justAsyncExceptions _ = Nothing
\end{code}

-----------------------------------------------------------------------------
Some Useful Functions

\begin{code}
finally :: IO a -> IO b -> IO b
a `finally` sequel = do
   tryAllIO a
   sequel

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO ()
bracket before after thing = do
  a <- before 
  c <- tryAllIO (thing a)
  after a
  return ()

bracket_ :: IO a -> IO b -> IO c -> IO ()
bracket_ before after thing = do
  before 
  c <- tryAllIO thing
  after
  return ()
\end{code}
