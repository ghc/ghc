% -----------------------------------------------------------------------------
% $Id: Exception.lhs,v 1.2 1998/12/02 13:26:30 simonm Exp $
%
% (c) The GRAP/AQUA Project, Glasgow University, 1998
%

The External API for exceptions.  The functions provided in this
module allow catching of exceptions in the IO monad.

\begin{code}
module Exception (

	Exception(..),		-- instance Show
	ArithError(..),		-- instance Show

	-- Throwing exceptions

	throw,			-- :: Exception -> a

	-- Catching exceptions: The IO interface

	catchException,		-- :: IO a -> (Exception       -> IO a) -> IO a
	catch,         		-- :: IO a -> (IOError         -> IO a) -> IO a

	catchArith,		-- :: IO a -> (ArithError      -> IO a) -> IO a
	catchError,		-- :: IO a -> (String          -> IO a) -> IO a

	getException,		-- :: a    -> IO (Maybe Exception)
	getExceptionIO,		-- :: IO a -> IO (Either Exception a)

	throwDyn, 		-- :: Typeable exception => exception -> b
	catchDyn, 		-- :: Typeable exception => 
				--    IO a -> (exception -> IO a) -> IO a

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
Catch certain types of exception.

The following family of functions provide exception handling functions
for particular kinds of exceptions; all non-matching exceptions being
re-raised.

\begin{code}
catchIO = Prelude.catch
#ifdef __HUGS__
catch   = PreludeBuiltin.catchException
#else
catch   = PrelException.catchException
#endif

catchArith	:: IO a -> (ArithError -> IO a) -> IO a
catchArith m k	= catch m handler
  where handler (ArithException err) = k err
	handler other                = throw other

catchError	:: IO a -> (String -> IO a) -> IO a
catchError m k  = catch m handler
  where handler (ErrorCall err) = k err
	handler other           = throw other
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
			   other -> throw ex
\end{code}

-----------------------------------------------------------------------------
Some Useful Functions

\begin{code}
#ifdef __HUGS__
getException :: a -> IO (Maybe Exception)
getException a = primCatch' (case primForce a of { () -> return Nothing}) (\e -> return (Just e))
#else
getException :: a -> IO (Maybe Exception)
getException a = catch# (a `seq` return Nothing) (\e -> return (Just e))
#endif

getExceptionIO :: IO a -> IO (Either Exception a)
getExceptionIO m = catchException (m >>= \ r -> return (Right r)) 
			                (\ e -> return (Left  e))
\end{code}
