{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE Safe              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.IO.StdHandles [boot]
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module GHC.Internal.IO.StdHandles where

import GHC.Internal.IO.Handle.Types

-- used in GHC.Conc, which is below GHC.Internal.IO.Handle.FD
stdout :: Handle

