\begin{code}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Exception
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exceptions and exception-handling functions.
-- 
-----------------------------------------------------------------------------

-- #hide
module GHC.Exception where

import Data.Maybe
import {-# SOURCE #-} Data.Typeable
import GHC.Base
import GHC.Show
\end{code}

%*********************************************************
%*                                                      *
\subsection{Exceptions}
%*                                                      *
%*********************************************************

\begin{code}
data SomeException = forall e . Exception e => SomeException e
    deriving Typeable

instance Show SomeException where
    showsPrec p (SomeException e) = showsPrec p e

class (Typeable e, Show e) => Exception e where
    toException   :: e -> SomeException
    fromException :: SomeException -> Maybe e

    toException = SomeException
    fromException (SomeException e) = cast e

instance Exception SomeException where
    toException se = se
    fromException = Just
\end{code}

%*********************************************************
%*                                                      *
\subsection{Primitive throw}
%*                                                      *
%*********************************************************

\begin{code}
-- | Throw an exception.  Exceptions may be thrown from purely
-- functional code, but may only be caught within the 'IO' monad.
throw :: Exception e => e -> a
throw e = raise# (toException e)
\end{code}

\begin{code}
data ErrorCall = ErrorCall String
    deriving Typeable

instance Exception ErrorCall

instance Show ErrorCall where
    showsPrec _ (ErrorCall err) = showString err

-----

-- |The type of arithmetic exceptions
data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  deriving (Eq, Ord, Typeable)

instance Exception ArithException

instance Show ArithException where
  showsPrec _ Overflow        = showString "arithmetic overflow"
  showsPrec _ Underflow       = showString "arithmetic underflow"
  showsPrec _ LossOfPrecision = showString "loss of precision"
  showsPrec _ DivideByZero    = showString "divide by zero"
  showsPrec _ Denormal        = showString "denormal"

\end{code}
