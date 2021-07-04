{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

{-|
This module exports the Type Literal kinds as well as the comparison type
families for those kinds.  It is needed to prevent module cycles while still
allowing these identifiers to be improted in 'Data.Type.Ord'.

@since 4.16.0.0
-}

module GHC.TypeLits.Internal
  ( Symbol
  , CmpSymbol, CmpChar
  , ErrorMessage (..), TypeError
  ) where

import GHC.Base(Ordering)
import GHC.Types(Symbol, Char)

-- | Comparison of type-level symbols, as a function.
--
-- @since 4.7.0.0
type family CmpSymbol (m :: Symbol) (n :: Symbol) :: Ordering

-- Char-related type families

-- | Comparison of type-level characters.
--
-- @since 4.16.0.0
type family CmpChar (a :: Char) (b :: Char) :: Ordering

-- | A description of a custom type error.
data {-kind-} ErrorMessage = Text Symbol
                             -- ^ Show the text as is.

                           | forall t. ShowType t
                             -- ^ Pretty print the type.
                             -- @ShowType :: k -> ErrorMessage@

                           | ErrorMessage :<>: ErrorMessage
                             -- ^ Put two pieces of error message next
                             -- to each other.

                           | ErrorMessage :$$: ErrorMessage
                             -- ^ Stack two pieces of error message on top
                             -- of each other.

infixl 5 :$$:
infixl 6 :<>:

-- | The type-level equivalent of 'Prelude.error'.
--
-- The polymorphic kind of this type allows it to be used in several settings.
-- For instance, it can be used as a constraint, e.g. to provide a better error
-- message for a non-existent instance,
--
-- @
-- -- in a context
-- instance TypeError (Text "Cannot 'Show' functions." :$$:
--                     Text "Perhaps there is a missing argument?")
--       => Show (a -> b) where
--     showsPrec = error "unreachable"
-- @
--
-- It can also be placed on the right-hand side of a type-level function
-- to provide an error for an invalid case,
--
-- @
-- type family ByteSize x where
--    ByteSize Word16   = 2
--    ByteSize Word8    = 1
--    ByteSize a        = TypeError (Text "The type " :<>: ShowType a :<>:
--                                   Text " is not exportable.")
-- @
--
-- @since 4.9.0.0
type family TypeError (a :: ErrorMessage) :: b where
