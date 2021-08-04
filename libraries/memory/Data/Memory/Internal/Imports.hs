-- |
-- Module      : Data.Memory.Internal.Imports
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE CPP #-}
module Data.Memory.Internal.Imports
    ( module X
    ) where

import Data.Word                    as X
import Control.Applicative          as X
import Control.Monad                as X (forM, forM_, void, when)
import Control.Arrow                as X (first, second)
import Data.Memory.Internal.DeepSeq as X
