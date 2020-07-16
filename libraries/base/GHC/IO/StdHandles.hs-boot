{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.StdHandles [boot]
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module GHC.IO.StdHandles where

import GHC.IO.Handle.Types

-- used in GHC.Conc, which is below GHC.IO.Handle.FD
stdout :: Handle

