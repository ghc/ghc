{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Text.ParserCombinators.ReadPrec
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (uses Text.ParserCombinators.ReadP)
--
-- This module defines parser combinators for precedence parsing.

module Text.ParserCombinators.ReadPrec
    (ReadPrec,
     -- *  Precedences
     Prec,
     minPrec,
     -- *  Precedence operations
     lift,
     prec,
     step,
     reset,
     -- *  Other operations
     -- |  All are based directly on their similarly-named 'ReadP' counterparts.
     get,
     look,
     (+++),
     (<++),
     pfail,
     choice,
     -- *  Converters
     readPrec_to_P,
     readP_to_Prec,
     readPrec_to_S,
     readS_to_Prec
     ) where

import GHC.Internal.Text.ParserCombinators.ReadPrec
