{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Exception.Type
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exceptions and exception-handling functions.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

module GHC.Exception.Type
       ( Exception(..)    -- Class
       , SomeException(..)
       , exceptionContext
       , addExceptionContext
       , mapExceptionContext
       , NoBacktrace(..)
         -- * Exception context
       , ExceptionContext(..)
       , emptyExceptionContext
       , mergeExceptionContext
       , ExceptionWithContext(..)
         -- * 'WhileHandling' annotations
       , WhileHandling(..)
         -- * Arithmetic exceptions
       , ArithException(..)
       , divZeroException, overflowException, ratioZeroDenomException
       , underflowException
       ) where

import Data.Maybe
import Data.Typeable (Typeable, cast)
   -- loop: Data.Typeable -> GHC.Err -> GHC.Exception
import GHC.Base
import GHC.Show
import GHC.Exception.Context

{- |
The @SomeException@ type is the root of the exception type hierarchy.
When an exception of type @e@ is thrown, behind the scenes it is
encapsulated in a @SomeException@.
-}
data SomeException = forall e. (Exception e, ?exceptionContext :: ExceptionContext) => SomeException e

-- | View the 'ExceptionContext' of a 'SomeException'.
exceptionContext :: SomeException -> ExceptionContext
exceptionContext (SomeException _) = ?exceptionContext

-- | Add more 'ExceptionContext' to a 'SomeException'.
addExceptionContext :: ExceptionAnnotation a => a -> SomeException -> SomeException
addExceptionContext ann =
    mapExceptionContext (addExceptionAnnotation ann)

mapExceptionContext :: (ExceptionContext -> ExceptionContext) -> SomeException -> SomeException
mapExceptionContext f se@(SomeException e) =
    let ?exceptionContext = f (exceptionContext se)
    in SomeException e

-- | @since 3.0
instance Show SomeException where
    showsPrec p (SomeException e) = showsPrec p e

{- |
Any type that you wish to throw or catch as an exception must be an
instance of the @Exception@ class. The simplest case is a new exception
type directly below the root:

> data MyException = ThisException | ThatException
>     deriving Show
>
> instance Exception MyException

The default method definitions in the @Exception@ class do what we need
in this case. You can now throw and catch @ThisException@ and
@ThatException@ as exceptions:

@
*Main> throw ThisException \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: MyException))
Caught ThisException
@

In more complicated examples, you may wish to define a whole hierarchy
of exceptions:

> ---------------------------------------------------------------------
> -- Make the root exception type for all the exceptions in a compiler
>
> data SomeCompilerException = forall e . Exception e => SomeCompilerException e
>
> instance Show SomeCompilerException where
>     show (SomeCompilerException e) = show e
>
> instance Exception SomeCompilerException
>
> compilerExceptionToException :: Exception e => e -> SomeException
> compilerExceptionToException = toException . SomeCompilerException
>
> compilerExceptionFromException :: Exception e => SomeException -> Maybe e
> compilerExceptionFromException x = do
>     SomeCompilerException a <- fromException x
>     cast a
>
> ---------------------------------------------------------------------
> -- Make a subhierarchy for exceptions in the frontend of the compiler
>
> data SomeFrontendException = forall e . Exception e => SomeFrontendException e
>
> instance Show SomeFrontendException where
>     show (SomeFrontendException e) = show e
>
> instance Exception SomeFrontendException where
>     toException = compilerExceptionToException
>     fromException = compilerExceptionFromException
>
> frontendExceptionToException :: Exception e => e -> SomeException
> frontendExceptionToException = toException . SomeFrontendException
>
> frontendExceptionFromException :: Exception e => SomeException -> Maybe e
> frontendExceptionFromException x = do
>     SomeFrontendException a <- fromException x
>     cast a
>
> ---------------------------------------------------------------------
> -- Make an exception type for a particular frontend compiler exception
>
> data MismatchedParentheses = MismatchedParentheses
>     deriving Show
>
> instance Exception MismatchedParentheses where
>     toException   = frontendExceptionToException
>     fromException = frontendExceptionFromException

We can now catch a @MismatchedParentheses@ exception as
@MismatchedParentheses@, @SomeFrontendException@ or
@SomeCompilerException@, but not other types, e.g. @IOException@:

@
*Main> throw MismatchedParentheses \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: MismatchedParentheses))
Caught MismatchedParentheses
*Main> throw MismatchedParentheses \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeFrontendException))
Caught MismatchedParentheses
*Main> throw MismatchedParentheses \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: SomeCompilerException))
Caught MismatchedParentheses
*Main> throw MismatchedParentheses \`catch\` \\e -> putStrLn (\"Caught \" ++ show (e :: IOException))
*** Exception: MismatchedParentheses
@

-}
class (Typeable e, Show e) => Exception e where
    -- | @toException@ should produce a 'SomeException' with no attached 'ExceptionContext'.
    toException   :: e -> SomeException
    fromException :: SomeException -> Maybe e

    toException e = SomeException e
      where ?exceptionContext = emptyExceptionContext
    fromException (SomeException e) = cast e

    -- | Render this exception value in a human-friendly manner.
    --
    -- Default implementation: @'show'@.
    --
    -- @since 4.8.0.0
    displayException :: e -> String
    displayException = show

    backtraceDesired :: e -> Bool
    backtraceDesired _ = True

-- | @since 4.8.0.0
instance Exception Void

-- | @since 3.0
instance Exception SomeException where
    toException (SomeException e) =
        let ?exceptionContext = emptyExceptionContext
        in SomeException e
    fromException = Just
    backtraceDesired (SomeException e) = backtraceDesired e
    displayException (SomeException e) =
        displayException e ++ "\n" ++ displayContext ?exceptionContext

displayContext :: ExceptionContext -> String
displayContext (ExceptionContext anns0) = go anns0
  where
    go (SomeExceptionAnnotation ann : anns) = displayExceptionAnnotation ann ++ "\n" ++ go anns
    go [] = "\n"

newtype NoBacktrace e = NoBacktrace e
    deriving (Show)

instance Exception e => Exception (NoBacktrace e) where
    fromException = fmap NoBacktrace . fromException
    toException (NoBacktrace e) = toException e
    backtraceDesired _ = False

-- | Wraps a particular exception exposing its 'ExceptionContext'. Intended to
-- be used when 'catch'ing exceptions in cases where access to the context is
-- desired.
data ExceptionWithContext a = ExceptionWithContext ExceptionContext a

instance Show a => Show (ExceptionWithContext a) where
    showsPrec _ (ExceptionWithContext _ e) = showString "ExceptionWithContext _ " . shows e

instance Exception a => Exception (ExceptionWithContext a) where
    toException (ExceptionWithContext ctxt e) =
        SomeException e
      where ?exceptionContext = ctxt
    fromException se = do
        e <- fromException se
        return (ExceptionWithContext (exceptionContext se) e)
    backtraceDesired (ExceptionWithContext _ e) = backtraceDesired e
    displayException = displayException . toException

-- | An 'ExceptionAnnotation' applied by 'catch' and similar operations
-- to exceptions thrown while handling another exception.
--
-- @since 4.19.0.0
newtype WhileHandling = WhileHandling SomeException

instance ExceptionAnnotation WhileHandling where
    displayExceptionAnnotation (WhileHandling e) =
        "While handling: " ++ displayException e

-- |Arithmetic exceptions.
data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  | RatioZeroDenominator -- ^ @since 4.6.0.0
  deriving ( Eq  -- ^ @since 3.0
           , Ord -- ^ @since 3.0
           )

divZeroException, overflowException, ratioZeroDenomException, underflowException  :: SomeException
divZeroException        = toException DivideByZero
overflowException       = toException Overflow
ratioZeroDenomException = toException RatioZeroDenominator
underflowException      = toException Underflow

-- | @since 4.0.0.0
instance Exception ArithException

-- | @since 4.0.0.0
instance Show ArithException where
  showsPrec _ Overflow        = showString "arithmetic overflow"
  showsPrec _ Underflow       = showString "arithmetic underflow"
  showsPrec _ LossOfPrecision = showString "loss of precision"
  showsPrec _ DivideByZero    = showString "divide by zero"
  showsPrec _ Denormal        = showString "denormal"
  showsPrec _ RatioZeroDenominator = showString "Ratio has zero denominator"
