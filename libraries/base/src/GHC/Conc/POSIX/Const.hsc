{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc.POSIX.Const
-- Copyright   :  (c) The University of Glasgow, 2019
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Constants shared with the rts, GHC.Conc.POSIX uses MagicHash which confuses
-- hsc2hs so these are moved to a new module.
--
-----------------------------------------------------------------------------

-- #not-home
module GHC.Conc.POSIX.Const where

import Data.Word

#include <Rts.h>

io_MANAGER_WAKEUP, io_MANAGER_DIE :: Word32
io_MANAGER_WAKEUP = #{const IO_MANAGER_WAKEUP}
io_MANAGER_DIE    = #{const IO_MANAGER_DIE}
