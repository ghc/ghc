-- |
-- Module      : Crypto.Internal.Imports
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE CPP #-}
module Crypto.Internal.Imports
    ( module X
    ) where

import Data.Word               as X
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup          as X (Semigroup(..))
#endif
import Control.Applicative     as X
import Control.Monad           as X (forM, forM_, void)
import Control.Arrow           as X (first, second)
import Crypto.Internal.DeepSeq as X
