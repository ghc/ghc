{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Double
-- Copyright   :  (c) GHC contributors 2026
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Core Libraries Committee
-- Stability   :  stable
-- Portability :  portable
--
-- 'Double' and associated functions.
module Data.Double
    ( Double
    , castWord64ToDouble
    , castDoubleToWord64
    ) where

import GHC.Float