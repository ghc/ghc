{-# LANGUAGE NoImplicitPrelude, TypeFamilies, DeriveGeneric #-}
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
        Bool(..), Char(..), Int(..),
        Float(..), Double(..), IO(..)
    ) where

import GHC.Prim
import GHC.Generics


infixr 5 :

data [] a = [] | a : [a] deriving Generic

data Bool = False | True deriving Generic

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
data Char = C# Char#

-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.
data Int = I# Int#

-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
data Float = F# Float#

-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
data Double = D# Double#

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


--------------------------------------------------------------------------------
-- Generic representations
--------------------------------------------------------------------------------

-- Int
data D_Int
data C_Int

instance Datatype D_Int where
  datatypeName _ = "Int"
  moduleName   _ = "GHC.Int"

instance Constructor C_Int where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

instance Generic Int where
  type Rep Int = D1 D_Int (C1 C_Int (S1 NoSelector (Rec0 Int)))
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x


-- Float
data D_Float
data C_Float

instance Datatype D_Float where
  datatypeName _ = "Float"
  moduleName   _ = "GHC.Float"

instance Constructor C_Float where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

instance Generic Float where
  type Rep Float = D1 D_Float (C1 C_Float (S1 NoSelector (Rec0 Float)))
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x


-- Double
data D_Double
data C_Double

instance Datatype D_Double where
  datatypeName _ = "Double"
  moduleName   _ = "GHC.Float"

instance Constructor C_Double where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

instance Generic Double where
  type Rep Double = D1 D_Double (C1 C_Double (S1 NoSelector (Rec0 Double)))
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x


-- Char
data D_Char
data C_Char

instance Datatype D_Char where
  datatypeName _ = "Char"
  moduleName   _ = "GHC.Base"

instance Constructor C_Char where
  conName _ = "" -- JPM: I'm not sure this is the right implementation...

instance Generic Char where
  type Rep Char = D1 D_Char (C1 C_Char (S1 NoSelector (Rec0 Char)))
  from x = M1 (M1 (M1 (K1 x)))
  to (M1 (M1 (M1 (K1 x)))) = x

