{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Types
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC type definitions.
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Types (
        Bool(..), Char(..), Int(..), Word(..),
        Float(..), Double(..),
        Ordering(..), IO(..)
    ) where

import GHC.Prim


infixr 5 :

data [] a = [] | a : [a]

data {-# CTYPE "HsBool" #-} Bool = False | True

{- | The character type 'Char' is an enumeration whose values represent
Unicode (or equivalently ISO\/IEC 10646) characters (see
<http://www.unicode.org/> for details).  This set extends the ISO 8859-1
(Latin-1) character set (the first 256 characters), which is itself an extension
of the ASCII character set (the first 128 characters).  A character literal in
Haskell has type 'Char'.

To convert a 'Char' to or from the corresponding 'Int' value defined
by Unicode, use 'Prelude.toEnum' and 'Prelude.fromEnum' from the
'Prelude.Enum' class respectively (or equivalently 'ord' and 'chr').
-}
data {-# CTYPE "HsChar" #-} Char = C# Char#

-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.
data {-# CTYPE "HsInt" #-} Int = I# Int#

-- |A 'Word' is an unsigned integral type, with the same size as 'Int'.
data {-# CTYPE "HsWord" #-} Word = W# Word#

-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
data {-# CTYPE "HsFloat" #-} Float = F# Float#

-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
data {-# CTYPE "HsDouble" #-} Double = D# Double#

data Ordering = LT | EQ | GT

{- |
A value of type @'IO' a@ is a computation which, when performed,
does some I\/O before returning a value of type @a@.

There is really only one way to \"perform\" an I\/O action: bind it to
@Main.main@ in your program.  When your program is run, the I\/O will
be performed.  It isn't possible to perform I\/O from an arbitrary
function, unless that function is itself in the 'IO' monad and called
at some point, directly or indirectly, from @Main.main@.

'IO' is a monad, so 'IO' actions can be combined using either the do-notation
or the '>>' and '>>=' operations from the 'Monad' class.
-}
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))


-- | A data constructor used to box up all unlifted equalities
--
-- The type constructor is special in that GHC pretends that it
-- has kind (? -> ? -> Fact) rather than (* -> * -> *)
data (~) a b = Eq# ((~#) a b)

-- | A data constructor used to box up unlifted representational equalities.
--
-- The type constructor is special as GHC pretends the field of EqR# has type
-- (a ~R# b), which is not representable in Haskell, and turns it into a class.
data Coercible a b = MkCoercible ((~#) a b)
