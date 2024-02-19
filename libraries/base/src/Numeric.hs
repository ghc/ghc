{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Numeric
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Odds and ends, mostly functions for reading and showing
-- 'RealFloat'-like kind of values.
--

module Numeric
    (-- *  Showing
     showSigned,
     showIntAtBase,
     showInt,
     showBin,
     showHex,
     showOct,
     showEFloat,
     showFFloat,
     showGFloat,
     showFFloatAlt,
     showGFloatAlt,
     showFloat,
     showHFloat,
     floatToDigits,
     -- *  Reading
     -- |  /NB:/ 'readInt' is the \'dual\' of 'showIntAtBase',
     -- and 'readDec' is the \`dual\' of 'showInt'.
     -- The inconsistent naming is a historical accident.
     readSigned,
     readInt,
     readBin,
     readDec,
     readOct,
     readHex,
     readFloat,
     lexDigits,
     -- *  Miscellaneous
     fromRat,
     Floating(..)
     ) where

import GHC.Internal.Numeric
