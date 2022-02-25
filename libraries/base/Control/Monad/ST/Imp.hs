{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Imp
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This library provides support for /strict/ state threads, as
-- described in the PLDI \'94 paper by John Launchbury and Simon Peyton
-- Jones /Lazy Functional State Threads/.
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Imp (
        -- * The 'ST' Monad
        ST,             -- abstract, instance of Functor, Monad, Typeable.
        runST,
        fixST,

        -- * Converting 'ST' to 'Prelude.IO'
        RealWorld,              -- abstract
        stToIO,

        -- * Unsafe operations
        unsafeInterleaveST,
        unsafeDupableInterleaveST,
        unsafeIOToST,
        unsafeSTToIO
    ) where

import GHC.ST           ( ST, runST, unsafeInterleaveST
                        , unsafeDupableInterleaveST )
import GHC.Base         ( RealWorld, ($), return )
import GHC.IO           ( stToIO, unsafeIOToST, unsafeSTToIO
                        , unsafeDupableInterleaveIO )
import GHC.MVar         ( readMVar, putMVar, newEmptyMVar )
import Control.Exception.Base
                        ( catch, throwIO, NonTermination (..)
                        , BlockedIndefinitelyOnMVar (..) )

-- | Allow the result of an 'ST' computation to be used (lazily)
-- inside the computation.
--
-- Note that if @f@ is strict, @'fixST' f = _|_@.
fixST :: (a -> ST s a) -> ST s a
-- See Note [fixST]
fixST k = unsafeIOToST $ do
    m <- newEmptyMVar
    ans <- unsafeDupableInterleaveIO
             (readMVar m `catch` \BlockedIndefinitelyOnMVar ->
                                    throwIO NonTermination)
    result <- unsafeSTToIO (k ans)
    putMVar m result
    return result

{- Note [fixST]
   ~~~~~~~~~~~~
For many years, we implemented fixST much like a pure fixpoint,
using liftST:

  fixST :: (a -> ST s a) -> ST s a
  fixST k = ST $ \ s ->
      let ans       = liftST (k r) s
          STret _ r = ans
      in
      case ans of STret s' x -> (# s', x #)

We knew that lazy blackholing could cause the computation to be re-run if the
result was demanded strictly, but we thought that would be okay in the case of
ST. However, that is not the case (see #15349). Notably, the first time
the computation is executed, it may mutate variables that cause it to behave
*differently* the second time it's run. That may allow it to terminate when it
should not. More frighteningly, Arseniy Alekseyev produced a somewhat contrived
example ( https://mail.haskell.org/pipermail/libraries/2018-July/028889.html )
demonstrating that it can break reasonable assumptions in "trustworthy" code,
causing a memory safety violation. So now we implement fixST much like we do
fixIO. See also the implementation notes for fixIO. Simon Marlow wondered
whether we could get away with an IORef instead of an MVar. I believe we
cannot. The function passed to fixST may spark a parallel computation that
demands the final result. Such a computation should block until the final
result is available.
-}
