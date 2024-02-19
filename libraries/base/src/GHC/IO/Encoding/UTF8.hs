{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.Encoding.UTF8
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- UTF-8 Codec for the IO library
--
-- This is one of several UTF-8 implementations provided by GHC; see Note
-- [GHC's many UTF-8 implementations] in "GHC.Encoding.UTF8" for an
-- overview.
--
-- Portions Copyright   : (c) Tom Harper 2008-2009,
--                        (c) Bryan O'Sullivan 2009,
--                        (c) Duncan Coutts 2009
--

module GHC.IO.Encoding.UTF8
    (utf8,
     mkUTF8,
     utf8_bom,
     mkUTF8_bom
     ) where

import GHC.Internal.IO.Encoding.UTF8