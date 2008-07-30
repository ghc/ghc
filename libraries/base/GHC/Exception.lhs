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

