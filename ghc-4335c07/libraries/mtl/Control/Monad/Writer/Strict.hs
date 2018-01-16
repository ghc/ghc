-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Strict writer monads.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer.Strict (
    -- * MonadWriter class
    MonadWriter(..),
    listens,
    censor,
    -- * The Writer monad
    Writer,
    runWriter,
    execWriter,
    mapWriter,
    -- * The WriterT monad transformer
    WriterT(..),
    execWriterT,
    mapWriterT,
    module Control.Monad,
    module Control.Monad.Fix,
    module Control.Monad.Trans,
    module Data.Monoid,
  ) where

import Control.Monad.Writer.Class

import Control.Monad.Trans
import Control.Monad.Trans.Writer.Strict (
        Writer, runWriter, execWriter, mapWriter,
        WriterT(..), execWriterT, mapWriterT)

import Control.Monad
import Control.Monad.Fix
import Data.Monoid
