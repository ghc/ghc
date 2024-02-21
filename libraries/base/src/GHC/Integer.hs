{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Compatibility module for pre-@ghc-bignum@ code.

module GHC.Integer
    (Integer,
     -- *  Construct 'Integer's
     smallInteger,
     wordToInteger,
     -- *  Conversion to other integral types
     integerToWord,
     integerToInt,
     -- *  Helpers for 'RealFloat' type-class operations
     encodeFloatInteger,
     encodeDoubleInteger,
     decodeDoubleInteger,
     -- *  Arithmetic operations
     plusInteger,
     minusInteger,
     timesInteger,
     negateInteger,
     absInteger,
     signumInteger,
     divModInteger,
     divInteger,
     modInteger,
     quotRemInteger,
     quotInteger,
     remInteger,
     -- *  Comparison predicates
     eqInteger,
     neqInteger,
     leInteger,
     gtInteger,
     ltInteger,
     geInteger,
     compareInteger,
     -- **  'Int#'-boolean valued versions of comparison predicates
     -- |  These operations return @0#@ and @1#@ instead of 'False' and
     -- 'True' respectively.  See
     -- <https://gitlab.haskell.org/ghc/ghc/wikis/prim-bool PrimBool wiki-page>
     -- for more details
     eqInteger#,
     neqInteger#,
     leInteger#,
     gtInteger#,
     ltInteger#,
     geInteger#,
     -- *  Bit-operations
     andInteger,
     orInteger,
     xorInteger,
     complementInteger,
     shiftLInteger,
     shiftRInteger,
     testBitInteger,
     popCountInteger,
     bitInteger,
     -- *  Hashing
     hashInteger
     ) where

import GHC.Internal.Integer
