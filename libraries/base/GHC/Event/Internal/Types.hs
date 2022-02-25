{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Types
-- Copyright   :  (c) Tamar Christina 2018
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable
--
-- Abstraction over C Handle types for GHC, Unix wants FD (CInt) while Windows
-- Wants Handle (CIntPtr), so we abstract over them here.
--
-------------------------------------------------------------------------------

module GHC.Event.Internal.Types
    (
    -- * Event type
      Event
    , evtRead
    , evtWrite
    , evtClose
    , evtNothing
    , eventIs
    -- * Lifetimes
    , Lifetime(..)
    , EventLifetime
    , eventLifetime
    , elLifetime
    , elEvent
    -- * Timeout type
    , Timeout(..)
    ) where

import Data.OldList (foldl', filter, intercalate, null)

import Data.Bits ((.|.), (.&.))
import Data.Semigroup.Internal (stimesMonoid)

import GHC.Base
import GHC.Show (Show(..))
import GHC.Word (Word64)

-- | An I\/O event.
newtype Event = Event Int
    deriving Eq -- ^ @since 4.4.0.0

evtNothing :: Event
evtNothing = Event 0
{-# INLINE evtNothing #-}

-- | Data is available to be read.
evtRead :: Event
evtRead = Event 1
{-# INLINE evtRead #-}

-- | The file descriptor is ready to accept a write.
evtWrite :: Event
evtWrite = Event 2
{-# INLINE evtWrite #-}

-- | Another thread closed the file descriptor.
evtClose :: Event
evtClose = Event 4
{-# INLINE evtClose #-}

eventIs :: Event -> Event -> Bool
eventIs (Event a) (Event b) = a .&. b /= 0

-- | @since 4.4.0.0
instance Show Event where
    show e = '[' : (intercalate "," . filter (not . null) $
                    [evtRead `so` "evtRead",
                     evtWrite `so` "evtWrite",
                     evtClose `so` "evtClose"]) ++ "]"
        where ev `so` disp | e `eventIs` ev = disp
                           | otherwise      = ""

-- | @since 4.10.0.0
instance Semigroup Event where
    (<>)    = evtCombine
    stimes  = stimesMonoid

-- | @since 4.4.0.0
instance Monoid Event where
    mempty  = evtNothing
    mconcat = evtConcat

evtCombine :: Event -> Event -> Event
evtCombine (Event a) (Event b) = Event (a .|. b)
{-# INLINE evtCombine #-}

evtConcat :: [Event] -> Event
evtConcat = foldl' evtCombine evtNothing
{-# INLINE evtConcat #-}

-- | The lifetime of an event registration.
--
-- @since 4.8.1.0
data Lifetime = OneShot   -- ^ the registration will be active for only one
                          -- event
              | MultiShot -- ^ the registration will trigger multiple times
              deriving ( Show -- ^ @since 4.8.1.0
                       , Eq   -- ^ @since 4.8.1.0
                       )

-- | The longer of two lifetimes.
elSupremum :: Lifetime -> Lifetime -> Lifetime
elSupremum OneShot OneShot = OneShot
elSupremum _       _       = MultiShot
{-# INLINE elSupremum #-}

-- | @since 4.10.0.0
instance Semigroup Lifetime where
    (<>) = elSupremum
    stimes = stimesMonoid

-- | @mappend@ takes the longer of two lifetimes.
--
-- @since 4.8.0.0
instance Monoid Lifetime where
    mempty = OneShot

-- | A pair of an event and lifetime
--
-- Here we encode the event in the bottom three bits and the lifetime
-- in the fourth bit.
newtype EventLifetime = EL Int
                      deriving ( Show -- ^ @since 4.8.0.0
                               , Eq   -- ^ @since 4.8.0.0
                               )

-- | @since 4.11.0.0
instance Semigroup EventLifetime where
    EL a <> EL b = EL (a .|. b)

-- | @since 4.8.0.0
instance Monoid EventLifetime where
    mempty = EL 0

eventLifetime :: Event -> Lifetime -> EventLifetime
eventLifetime (Event e) l = EL (e .|. lifetimeBit l)
  where
    lifetimeBit OneShot   = 0
    lifetimeBit MultiShot = 8
{-# INLINE eventLifetime #-}

elLifetime :: EventLifetime -> Lifetime
elLifetime (EL x) = if x .&. 8 == 0 then OneShot else MultiShot
{-# INLINE elLifetime #-}

elEvent :: EventLifetime -> Event
elEvent (EL x) = Event (x .&. 0x7)
{-# INLINE elEvent #-}

-- | A type alias for timeouts, specified in nanoseconds.
data Timeout = Timeout {-# UNPACK #-} !Word64
             | Forever
               deriving Show -- ^ @since 4.4.0.0
