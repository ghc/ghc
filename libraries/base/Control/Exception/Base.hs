{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
#ifdef __HUGS__
        SomeException,
#else
        SomeException(..),
#endif
        Exception(..),
        IOException,
        ArithException(..),
        ArrayException(..),
        AssertionFailed(..),
        AsyncException(..),

#if __GLASGOW_HASKELL__ || __HUGS__
        NonTermination(..),
        NestedAtomically(..),
#endif

        BlockedOnDeadMVar(..),
        BlockedIndefinitely(..),
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
#ifdef __GLASGOW_HASKELL__
        throwTo,
#endif

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

        block,
        unblock,
        blocked,

        -- * Assertions

        assert,

        -- * Utilities

        bracket,
        bracket_,
        bracketOnError,

        finally,

#ifdef __GLASGOW_HASKELL__
        -- * Calls for GHC runtime
        recSelError, recConError, irrefutPatError, runtimeError,
        nonExhaustiveGuardsError, patError, noMethodBindingError,
        nonTermination, nestedAtomically,
#endif
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase
import GHC.Show
import GHC.IOBase
import GHC.Exception hiding ( Exception )
import GHC.Conc
#endif

#ifdef __HUGS__
import Prelude hiding (catch)
import Hugs.Prelude (ExitCode(..))
import Hugs.IOExts (unsafePerformIO)
import Hugs.Exception (SomeException(DynamicException, IOException,
                                     ArithException, ArrayException, ExitException),
                       evaluate, IOException, ArithException, ArrayException)
import qualified Hugs.Exception
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
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce    (unsafeCoerce)

-- minimum needed for nhc98 to pretend it has Exceptions

{-
data Exception   = IOException    IOException
                 | ArithException ArithException
                 | ArrayException ArrayException
                 | AsyncException AsyncException
                 | ExitException  ExitCode
                 deriving Show
-}
class ({-Typeable e,-} Show e) => Exception e where
    toException   :: e -> SomeException
    fromException :: SomeException -> Maybe e

data SomeException = forall e . Exception e => SomeException e

INSTANCE_TYPEABLE0(SomeException,someExceptionTc,"SomeException")

instance Show SomeException where
    showsPrec p (SomeException e) = showsPrec p e
instance Exception SomeException where
    toException se = se
    fromException = Just

type IOException = IOError
instance Exception IOError where
    toException                     = SomeException
    fromException (SomeException e) = Just (unsafeCoerce e)

instance Exception ExitCode where
    toException                     = SomeException
    fromException (SomeException e) = Just (unsafeCoerce e)

data ArithException
data ArrayException
data AsyncException
data AssertionFailed
data PatternMatchFail
data NoMethodError
data Deadlock
data BlockedOnDeadMVar
data BlockedIndefinitely
data ErrorCall
data RecConError
data RecSelError
data RecUpdError
instance Show ArithException
instance Show ArrayException
instance Show AsyncException
instance Show AssertionFailed
instance Show PatternMatchFail
instance Show NoMethodError
instance Show Deadlock
instance Show BlockedOnDeadMVar
instance Show BlockedIndefinitely
instance Show ErrorCall
instance Show RecConError
instance Show RecSelError
instance Show RecUpdError

catch   :: Exception e
        => IO a         -- ^ The computation to run
        -> (e -> IO a)  -- ^ Handler to invoke if an exception is raised
        -> IO a
catch io h = H'98.catch  io  (h . fromJust . fromException . toException)

throwIO  :: Exception e => e -> IO a
throwIO   = ioError . fromJust . fromException . toException

throw    :: Exception e => e -> a
throw     = unsafePerformIO . throwIO

evaluate :: a -> IO a
evaluate x = x `seq` return x

assert :: Bool -> a -> a
assert True  x = x
assert False _ = throw (toException (UserError "" "Assertion failed"))

#endif

#ifdef __HUGS__
class (Typeable e, Show e) => Exception e where
    toException   :: e -> SomeException
    fromException :: SomeException -> Maybe e

    toException e = DynamicException (toDyn e) (flip showsPrec e)
    fromException (DynamicException dyn _) = fromDynamic dyn
    fromException _ = Nothing

INSTANCE_TYPEABLE0(SomeException,someExceptionTc,"SomeException")
INSTANCE_TYPEABLE0(IOException,iOExceptionTc,"IOException")
INSTANCE_TYPEABLE0(ArithException,arithExceptionTc,"ArithException")
INSTANCE_TYPEABLE0(ArrayException,arrayExceptionTc,"ArrayException")
INSTANCE_TYPEABLE0(ExitCode,exitCodeTc,"ExitCode")
INSTANCE_TYPEABLE0(ErrorCall,errorCallTc,"ErrorCall")
INSTANCE_TYPEABLE0(AssertionFailed,assertionFailedTc,"AssertionFailed")
INSTANCE_TYPEABLE0(AsyncException,asyncExceptionTc,"AsyncException")
INSTANCE_TYPEABLE0(BlockedOnDeadMVar,blockedOnDeadMVarTc,"BlockedOnDeadMVar")
INSTANCE_TYPEABLE0(BlockedIndefinitely,blockedIndefinitelyTc,"BlockedIndefinitely")
INSTANCE_TYPEABLE0(Deadlock,deadlockTc,"Deadlock")

instance Exception SomeException where
    toException se = se
    fromException = Just

instance Exception IOException where
    toException = IOException
    fromException (IOException e) = Just e
    fromException _ = Nothing

instance Exception ArrayException where
    toException = ArrayException
    fromException (ArrayException e) = Just e
    fromException _ = Nothing

instance Exception ArithException where
    toException = ArithException
    fromException (ArithException e) = Just e
    fromException _ = Nothing

instance Exception ExitCode where
    toException = ExitException
    fromException (ExitException e) = Just e
    fromException _ = Nothing

data ErrorCall = ErrorCall String

instance Show ErrorCall where
    showsPrec _ (ErrorCall err) = showString err

instance Exception ErrorCall where
    toException (ErrorCall s) = Hugs.Exception.ErrorCall s
    fromException (Hugs.Exception.ErrorCall s) = Just (ErrorCall s)
    fromException _ = Nothing

data BlockedOnDeadMVar = BlockedOnDeadMVar
data BlockedIndefinitely = BlockedIndefinitely
data Deadlock = Deadlock
data AssertionFailed = AssertionFailed String
data AsyncException
  = StackOverflow
  | HeapOverflow
  | ThreadKilled
  | UserInterrupt
  deriving (Eq, Ord)

instance Show BlockedOnDeadMVar where
    showsPrec _ BlockedOnDeadMVar = showString "thread blocked indefinitely"

instance Show BlockedIndefinitely where
    showsPrec _ BlockedIndefinitely = showString "thread blocked indefinitely"

instance Show Deadlock where
    showsPrec _ Deadlock = showString "<<deadlock>>"

instance Show AssertionFailed where
    showsPrec _ (AssertionFailed err) = showString err

instance Show AsyncException where
    showsPrec _ StackOverflow   = showString "stack overflow"
    showsPrec _ HeapOverflow    = showString "heap overflow"
    showsPrec _ ThreadKilled    = showString "thread killed"
    showsPrec _ UserInterrupt   = showString "user interrupt"

instance Exception BlockedOnDeadMVar
instance Exception BlockedIndefinitely
instance Exception Deadlock
instance Exception AssertionFailed
instance Exception AsyncException

throw :: Exception e => e -> a
throw e = Hugs.Exception.throw (toException e)

throwIO :: Exception e => e -> IO a
throwIO e = Hugs.Exception.throwIO (toException e)
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
#if __GLASGOW_HASKELL__
catch = GHC.IOBase.catchException
#elif __HUGS__
catch m h = Hugs.Exception.catchException m h'
  where h' e = case fromException e of
            Just e' -> h e'
            Nothing -> throwIO e
#endif
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

onException :: IO a -> IO b -> IO a
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
    r <- unblock (thing a) `onException` after a
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
    r <- unblock a `onException` sequel
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
    unblock (thing a) `onException` after a
  )

#if !(__GLASGOW_HASKELL__ || __NHC__)
assert :: Bool -> a -> a
assert True x = x
assert False _ = throw (AssertionFailed "")
#endif

-----

#if __GLASGOW_HASKELL__ || __HUGS__
data PatternMatchFail = PatternMatchFail String
INSTANCE_TYPEABLE0(PatternMatchFail,patternMatchFailTc,"PatternMatchFail")

instance Show PatternMatchFail where
    showsPrec _ (PatternMatchFail err) = showString err

#ifdef __HUGS__
instance Exception PatternMatchFail where
    toException (PatternMatchFail err) = Hugs.Exception.PatternMatchFail err
    fromException (Hugs.Exception.PatternMatchFail err) = Just (PatternMatchFail err)
    fromException _ = Nothing
#else
instance Exception PatternMatchFail
#endif

-----

data RecSelError = RecSelError String
INSTANCE_TYPEABLE0(RecSelError,recSelErrorTc,"RecSelError")

instance Show RecSelError where
    showsPrec _ (RecSelError err) = showString err

#ifdef __HUGS__
instance Exception RecSelError where
    toException (RecSelError err) = Hugs.Exception.RecSelError err
    fromException (Hugs.Exception.RecSelError err) = Just (RecSelError err)
    fromException _ = Nothing
#else
instance Exception RecSelError
#endif

-----

data RecConError = RecConError String
INSTANCE_TYPEABLE0(RecConError,recConErrorTc,"RecConError")

instance Show RecConError where
    showsPrec _ (RecConError err) = showString err

#ifdef __HUGS__
instance Exception RecConError where
    toException (RecConError err) = Hugs.Exception.RecConError err
    fromException (Hugs.Exception.RecConError err) = Just (RecConError err)
    fromException _ = Nothing
#else
instance Exception RecConError
#endif

-----

data RecUpdError = RecUpdError String
INSTANCE_TYPEABLE0(RecUpdError,recUpdErrorTc,"RecUpdError")

instance Show RecUpdError where
    showsPrec _ (RecUpdError err) = showString err

#ifdef __HUGS__
instance Exception RecUpdError where
    toException (RecUpdError err) = Hugs.Exception.RecUpdError err
    fromException (Hugs.Exception.RecUpdError err) = Just (RecUpdError err)
    fromException _ = Nothing
#else
instance Exception RecUpdError
#endif

-----

data NoMethodError = NoMethodError String
INSTANCE_TYPEABLE0(NoMethodError,noMethodErrorTc,"NoMethodError")

instance Show NoMethodError where
    showsPrec _ (NoMethodError err) = showString err

#ifdef __HUGS__
instance Exception NoMethodError where
    toException (NoMethodError err) = Hugs.Exception.NoMethodError err
    fromException (Hugs.Exception.NoMethodError err) = Just (NoMethodError err)
    fromException _ = Nothing
#else
instance Exception NoMethodError
#endif

-----

data NonTermination = NonTermination
INSTANCE_TYPEABLE0(NonTermination,nonTerminationTc,"NonTermination")

instance Show NonTermination where
    showsPrec _ NonTermination = showString "<<loop>>"

#ifdef __HUGS__
instance Exception NonTermination where
    toException NonTermination = Hugs.Exception.NonTermination
    fromException Hugs.Exception.NonTermination = Just NonTermination
    fromException _ = Nothing
#else
instance Exception NonTermination
#endif

-----

data NestedAtomically = NestedAtomically
INSTANCE_TYPEABLE0(NestedAtomically,nestedAtomicallyTc,"NestedAtomically")

instance Show NestedAtomically where
    showsPrec _ NestedAtomically = showString "Control.Concurrent.STM.atomically was nested"

instance Exception NestedAtomically

-----

instance Exception Dynamic

#endif /* __GLASGOW_HASKELL__ || __HUGS__ */

#ifdef __GLASGOW_HASKELL__
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

-- GHC's RTS calls this
nonTermination :: SomeException
nonTermination = toException NonTermination

-- GHC's RTS calls this
nestedAtomically :: SomeException
nestedAtomically = toException NestedAtomically
#endif
