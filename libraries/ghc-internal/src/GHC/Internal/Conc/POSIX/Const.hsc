{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Conc.POSIX.Const
-- Copyright   :  (c) The University of Glasgow, 2019
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Constants shared with the rts, GHC.Internal.Conc.POSIX uses MagicHash which confuses
-- hsc2hs so these are moved to a new module.
--
-----------------------------------------------------------------------------

-- #not-home
module GHC.Internal.Conc.POSIX.Const where

import GHC.Internal.Word

#include <Rts.h>

io_MANAGER_WAKEUP, io_MANAGER_DIE :: Word32
io_MANAGER_WAKEUP = #{const IO_MANAGER_WAKEUP}
io_MANAGER_DIE    = #{const IO_MANAGER_DIE}
