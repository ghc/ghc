{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Float
-- Copyright   :  (c) GHC contributors 2026
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Core Libraries Committee
-- Stability   :  stable
-- Portability :  portable
--
-- 'Float' and associated functions.
module Data.Float
    ( Float
    , castWord32ToFloat
    , castFloatToWord32
    ) where

import GHC.Float