{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Maybe type
module GHC.Maybe
   ( Maybe (..)
   )
where

import GHC.Num.Integer () -- See Note [Depend on GHC.Num.Integer] in GHC.Base
import GHC.Classes

default ()

-------------------------------------------------------------------------------
-- Maybe type
-------------------------------------------------------------------------------

-- | The 'Maybe' type encapsulates an optional value.  A value of type
-- @'Maybe' a@ either contains a value of type @a@ (represented as @'Just' a@),
-- or it is empty (represented as 'Nothing').  Using 'Maybe' is a good way to
-- deal with errors or exceptional cases without resorting to drastic
-- measures such as 'Prelude.error'.
--
-- The 'Maybe' type is also a monad.  It is a simple kind of error
-- monad, where all errors are represented by 'Nothing'.  A richer
-- error monad can be built using the 'Data.Either.Either' type.
--
data  Maybe a  =  Nothing | Just a
  deriving ( Eq  -- ^ @since 2.01
           , Ord -- ^ @since 2.01
           )
