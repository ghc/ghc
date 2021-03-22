{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Linux.IO.URing.PollEvent
    ( Event(..)
    , pollIn, pollOut, pollErr, pollHup
    ) where

#include <poll.h>

import GHC.Base
import GHC.Enum
import GHC.Num (Num)
import GHC.Real
import GHC.Show (Show)
import Data.Bits (Bits, FiniteBits)
import Foreign.C.Types (CShort(..))
import Foreign.Storable (Storable(..))

-- | A @poll@ event mask.
newtype Event = Event CShort
    deriving ( Eq         -- ^ @since 4.4.0.0
             , Show       -- ^ @since 4.4.0.0
             , Num        -- ^ @since 4.4.0.0
             , Storable   -- ^ @since 4.4.0.0
             , Bits       -- ^ @since 4.4.0.0
             , FiniteBits -- ^ @since 4.7.0.0
             , Enum
             , Ord
             , Real
             , Integral
             )

#{enum Event, Event
 , pollIn    = POLLIN
 , pollOut   = POLLOUT
 , pollErr   = POLLERR
 , pollHup   = POLLHUP
 }
