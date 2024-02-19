-- |
--
-- Module      :  GHC.IOPort
-- Copyright   :  (c) Tamar Christina 2019
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'IOPort' type. This is a facility used by the Windows IO subsystem.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-- We have strict rules with an I/O Port:
-- * writing more than once is an error
-- * reading more than once is an error
--
-- It gives us the ability to have one thread to block, wait for a result from
-- another thread and then being woken up. *Nothing* more.
--
-- This type is very much GHC internal. It might be changed or removed without
-- notice in future releases.
--

module GHC.IOPort
    (-- *  IOPorts
     IOPort(..),
     newIOPort,
     newEmptyIOPort,
     readIOPort,
     writeIOPort,
     doubleReadException
     ) where

import GHC.Internal.IOPort