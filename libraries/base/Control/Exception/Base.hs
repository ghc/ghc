{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

#include "Typeable.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Exception.Base
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (extended exceptions)
--
-- Extensible exceptions, except for multiple handlers.
--
-----------------------------------------------------------------------------

module Control.Exception.Base (

        -- * The Exception type
        SomeException(..),
        Exception(..),
        IOException,
        ArithException(..),
        ArrayException(..),
        AssertionFailed(..),
        SomeAsyncException(..), AsyncException(..),
        asyncExceptionToException, asyncExceptionFromException,
        NonTermination(..),
        NestedAtomically(..),
        BlockedIndefinitelyOnMVar(..),
        BlockedIndefinitelyOnSTM(..),
        Deadlock(..),
        NoMethodError(..),
        PatternMatchFail(..),
        RecConError(..),
        RecSelError(..),
        RecUpdError(..),
        ErrorCall(..),

        -- * Throwing exceptions
        throwIO,
        throw,
        ioError,
        throwTo,

        -- * Catching Exceptions

        -- ** The @catch@ functions
        catch,
        catchJust,

        -- ** The @handle@ functions
        handle,
        handleJust,

        -- ** The @try@ functions
        try,
        tryJust,
        onException,

        -- ** The @evaluate@ function
        evaluate,

        -- ** The @mapException@ function
        mapException,

        -- * Asynchronous Exceptions

        -- ** Asynchronous exception control
        mask,
        mask_,
        uninterruptibleMask,
        uninterruptibleMask_,
        MaskingState(..),
        getMaskingState,

        -- * Assertions

        assert,

        -- * Utilities

        bracket,
        bracket_,
        bracketOnError,

        finally,

        -- * Calls for GHC runtime
        recSelError, recConError, irrefutPatError, runtimeError,
        nonExhaustiveGuardsError, patError, noMethodBindingError,
        absentError,
        nonTermination, nestedAtomically,
  ) where

import GHC.Base
import GHC.IO hiding (bracket,finally,onException)
import GHC.IO.Exception
import GHC.Exception
import GHC.Show
-- import GHC.Exception hiding ( Exception )
import GHC.Conc.Sync

import Data.Dynamic
import Data.Either
import Data.Maybe

-----------------------------------------------------------------------------
-- Catching exceptions

-- |This is the simplest of the exception-catching functions.  It
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
-- exception may be propogated further up. If you call it again, you
-- might get a the opposite behaviour. This is ok, because 'catch' is an
-- 'IO' computation.
--
catch   :: Exception e
        => IO a         -- ^ The computation to run
        -> (e -> IO a)  -- ^ Handler to invoke if an exception is raised
        -> IO a
catch = catchException

-- | The function 'catchJust' is like 'catch', but it takes an extra
-- argument which is an /exception predicate/, a function which
-- selects which type of exceptions we\'re interested in.
--
-- > catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
-- >           (readFile f)
-- >           (\_ -> do hPutStrLn stderr ("No such file: " ++ show f)
-- >                     return "")
--
-- Any other exceptions which are not matched by the predicate
-- are re-raised, and may be caught by an enclosing
-- 'catch', 'catchJust', etc.
catchJust
        :: Exception e
        => (e -> Maybe b)         -- ^ Predicate to select exceptions
        -> IO a                   -- ^ Computation to run
        -> (b -> IO a)            -- ^ Handler
        -> IO a
catchJust p a handler = catch a handler'
  where handler' e = case p e of
                        Nothing -> throwIO e
                        Just b  -> handler b

-- | A version of 'catch' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.  For example:
--
-- >   do handle (\NonTermination -> exitWith (ExitFailure 1)) $
-- >      ...
handle     :: Exception e => (e -> IO a) -> IO a -> IO a
handle     =  flip catch

-- | A version of 'catchJust' with the arguments swapped around (see
-- 'handle').
handleJust :: Exception e => (e -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust p =  flip (catchJust p)

-----------------------------------------------------------------------------
-- 'mapException'

-- | This function maps one exception into another as proposed in the
-- paper \"A semantics for imprecise exceptions\".

-- Notice that the usage of 'unsafePerformIO' is safe here.

mapException :: (Exception e1, Exception e2) => (e1 -> e2) -> a -> a
mapException f v = unsafePerformIO (catch (evaluate v)
                                          (\x -> throwIO (f x)))

-----------------------------------------------------------------------------
-- 'try' and variations.

-- | Similar to 'catch', but returns an 'Either' result which is
-- @('Right' a)@ if no exception of type @e@ was raised, or @('Left' ex)@
-- if an exception of type @e@ was raised and its value is @ex@.
-- If any other type of exception is raised than it will be propogated
-- up to the next enclosing exception handler.
--
-- >  try a = catch (Right `liftM` a) (return . Left)

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
                        Nothing -> throwIO e
                        Just b  -> return (Left b)

-- | Like 'finally', but only performs the final action if there was an
-- exception raised by the computation.
onException :: IO a -> IO b -> IO a
onException io what = io `catch` \e -> do _ <- what
                                          throwIO (e :: SomeException)

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
-- >   (\fileHandle -> do { ... })
--
-- The arguments to 'bracket' are in this order so that we can partially apply
-- it, e.g.:
--
-- > withFile name mode = bracket (openFile name mode) hClose
--
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

-- | A specialised variant of 'bracket' with just a computation to run
-- afterward.
--
finally :: IO a         -- ^ computation to run first
        -> IO b         -- ^ computation to run afterward (even if an exception
                        -- was raised)
        -> IO a         -- returns the value from the first computation
a `finally` sequel =
  mask $ \restore -> do
    r <- restore a `onException` sequel
    _ <- sequel
    return r

-- | A variant of 'bracket' where the return value from the first computation
-- is not required.
bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

-- | Like 'bracket', but only performs the final action if there was an
-- exception raised by the in-between computation.
bracketOnError
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracketOnError before after thing =
  mask $ \restore -> do
    a <- before
    restore (thing a) `onException` after a

-----

-- |A pattern match failed. The @String@ gives information about the
-- source location of the pattern.
data PatternMatchFail = PatternMatchFail String
INSTANCE_TYPEABLE0(PatternMatchFail,patternMatchFailTc,"PatternMatchFail")

instance Show PatternMatchFail where
    showsPrec _ (PatternMatchFail err) = showString err

instance Exception PatternMatchFail

-----

-- |A record selector was applied to a constructor without the
-- appropriate field. This can only happen with a datatype with
-- multiple constructors, where some fields are in one constructor
-- but not another. The @String@ gives information about the source
-- location of the record selector.
data RecSelError = RecSelError String
INSTANCE_TYPEABLE0(RecSelError,recSelErrorTc,"RecSelError")

instance Show RecSelError where
    showsPrec _ (RecSelError err) = showString err

instance Exception RecSelError

-----

-- |An uninitialised record field was used. The @String@ gives
-- information about the source location where the record was
-- constructed.
data RecConError = RecConError String
INSTANCE_TYPEABLE0(RecConError,recConErrorTc,"RecConError")

instance Show RecConError where
    showsPrec _ (RecConError err) = showString err

instance Exception RecConError

-----

-- |A record update was performed on a constructor without the
-- appropriate field. This can only happen with a datatype with
-- multiple constructors, where some fields are in one constructor
-- but not another. The @String@ gives information about the source
-- location of the record update.
data RecUpdError = RecUpdError String
INSTANCE_TYPEABLE0(RecUpdError,recUpdErrorTc,"RecUpdError")

instance Show RecUpdError where
    showsPrec _ (RecUpdError err) = showString err

instance Exception RecUpdError

-----

-- |A class method without a definition (neither a default definition,
-- nor a definition in the appropriate instance) was called. The
-- @String@ gives information about which method it was.
data NoMethodError = NoMethodError String
INSTANCE_TYPEABLE0(NoMethodError,noMethodErrorTc,"NoMethodError")

instance Show NoMethodError where
    showsPrec _ (NoMethodError err) = showString err

instance Exception NoMethodError

-----

-- |Thrown when the runtime system detects that the computation is
-- guaranteed not to terminate. Note that there is no guarantee that
-- the runtime system will notice whether any given computation is
-- guaranteed to terminate or not.
data NonTermination = NonTermination
INSTANCE_TYPEABLE0(NonTermination,nonTerminationTc,"NonTermination")

instance Show NonTermination where
    showsPrec _ NonTermination = showString "<<loop>>"

instance Exception NonTermination

-----

-- |Thrown when the program attempts to call @atomically@, from the @stm@
-- package, inside another call to @atomically@.
data NestedAtomically = NestedAtomically
INSTANCE_TYPEABLE0(NestedAtomically,nestedAtomicallyTc,"NestedAtomically")

instance Show NestedAtomically where
    showsPrec _ NestedAtomically = showString "Control.Concurrent.STM.atomically was nested"

instance Exception NestedAtomically

-----

recSelError, recConError, irrefutPatError, runtimeError,
  nonExhaustiveGuardsError, patError, noMethodBindingError,
  absentError
        :: Addr# -> a   -- All take a UTF8-encoded C string

recSelError              s = throw (RecSelError ("No match in record selector "
			                         ++ unpackCStringUtf8# s))  -- No location info unfortunately
runtimeError             s = error (unpackCStringUtf8# s)                   -- No location info unfortunately
absentError              s = error ("Oops!  Entered absent arg " ++ unpackCStringUtf8# s)

nonExhaustiveGuardsError s = throw (PatternMatchFail (untangle s "Non-exhaustive guards in"))
irrefutPatError          s = throw (PatternMatchFail (untangle s "Irrefutable pattern failed for pattern"))
recConError              s = throw (RecConError      (untangle s "Missing field in record construction"))
noMethodBindingError     s = throw (NoMethodError    (untangle s "No instance nor default method for class operation"))
patError                 s = throw (PatternMatchFail (untangle s "Non-exhaustive patterns in"))

-- GHC's RTS calls this
nonTermination :: SomeException
nonTermination = toException NonTermination

-- GHC's RTS calls this
nestedAtomically :: SomeException
nestedAtomically = toException NestedAtomically
