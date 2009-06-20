
{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module GHC.Types (Char(..), Int(..), Float(..), Double(..), IO(..)) where

import GHC.Prim
-- We need Inl etc behind the scenes for the type definitions
import GHC.Generics ()

infixr 5 :

data [] a = [] | a : [a]

data Char = C# Char#

data Int = I# Int#
-- ^A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.

-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
data Float      = F# Float#

-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
data Double     = D# Double#

{-|
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

