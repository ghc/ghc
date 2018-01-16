-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Classes for monad transformers.
--
-- A monad transformer makes new monad out of an existing monad, such
-- that computations of the old monad may be embedded in the new one.
-- To construct a monad with a desired set of features, one typically
-- starts with a base monad, such as @Identity@, @[]@ or 'IO', and
-- applies a sequence of monad transformers.
--
-- Most monad transformer modules include the special case of applying the
-- transformer to @Identity@.  For example, @State s@ is an abbreviation
-- for @StateT s Identity@.
--
-- Each monad transformer also comes with an operation @run@/XXX/ to
-- unwrap the transformer, exposing a computation of the inner monad.
-----------------------------------------------------------------------------

module Control.Monad.Trans (
    module Control.Monad.Trans.Class,
    module Control.Monad.IO.Class
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
